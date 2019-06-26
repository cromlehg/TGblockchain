package techgen.api

import java.lang.reflect.Constructor
import java.util.Random

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.parser.parse
import io.circe.syntax._
import techgen.blocks.BDBlock
import techgen.nodeViewHolder._
import techgen.transaction._
import org.whispersystems.curve25519.OpportunisticCurve25519Provider
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}
import scorex.core.api.http.{ApiError, ApiResponse, ApiRoute}
import scorex.core.settings.RESTApiSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.signatures.{PrivateKey, PublicKey}
import scorex.util.ModifierId

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scala.util.{Failure, Success, Try}


case class BDApiRoute(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
                     (implicit val context: ActorRefFactory, val ec: ExecutionContext)
  extends ApiRoute with ScorexEncoding {

  type PM = BDBlock
  type HIS = BDBlockchain
  type MP = BDMempool
  type MS = BDState
  type VL = BDWallet


  val random = new Random()

  override val route: Route = (pathPrefix("bd") & withCors) {
    containsModifier ~ block ~ sendTransaction ~ generateKeyPair ~ transfer ~ getState
  }

  def sendTransaction: Route = (post & path("transaction")) {
    entity(as[String]) { body =>
      parse(body) match {
        case Left(failure) => ApiError(failure.getCause)
        case Right(json) => Try {
        } match {
          case Success(resp) => ApiResponse(resp)
          case Failure(e) =>
            e.printStackTrace()
            ApiError(e)
        }
      }

    }
  }

  private val provider: OpportunisticCurve25519Provider = {
    val constructor = classOf[OpportunisticCurve25519Provider]
      .getDeclaredConstructors
      .head
      .asInstanceOf[Constructor[OpportunisticCurve25519Provider]]
    constructor.setAccessible(true)
    constructor.newInstance()
  }

  def transfer: Route = (post & path("transfer")) {
    entity(as[String]) { body =>
      withAuth {
        parse(body) match {
          case Left(failure) => ApiError(failure.getCause)
          case Right(json) => Try {
            val amount: Value = Value @@ (json \\ "amount").head.asNumber.get.toLong.get
            val recipient: String = (json \\ "recipient").head.asString.get
            val pk: String = (json \\ "pk").head.asString.get
            val wallet = Await.result((nodeViewHolderRef ? GetDataFromCurrentView[HIS, MS, VL, MP, BDWallet](v => v.vault)).mapTo[BDWallet], 1.second)
            val prop: PublicKey25519Proposition = PublicKey25519Proposition.validPubKey(recipient).get

            val privateKeyBytes: Array[Byte] = encoder.decode(pk).get

            val secret = PrivateKey @@ privateKeyBytes
            val pub = PublicKey @@ provider.generatePublicKey(secret)
            val pubProposition = PublicKey25519Proposition(pub)
            val privateKey = PrivateKey25519(secret, pub)

            val tx = TransferAssetsBDTransaction(prop,
              amount,
              0,
              pubProposition,
              BDTransaction.emptySign,
              BDTransaction.emptyHash)

            val signedTx = BDTransaction.signAndSetHash(tx, privateKey)

            nodeViewHolderRef ! LocallyGeneratedTransaction[AbstractBDTransaction](signedTx)

            ("tx", signedTx.id)
          } match {
            case Success(resp) => ApiResponse(resp)
            case Failure(e) =>
              e.printStackTrace()
              ApiError(e)
          }
        }
      }
    }
  }

  def getState: Route = (get & path("state" / Segment)) { encodedAddr: String =>

    val pubKeyProposition: PublicKey25519Proposition = PublicKey25519Proposition.validPubKey(encodedAddr).get

    def f(v: CurrentView[HIS, MS, VL, MP]): Option[AccountState] =
      v.state.state.find(_.target.toString == encodedAddr)

    val contains = (nodeViewHolderRef ? GetDataFromCurrentView[HIS, MS, VL, MP, Option[AccountState]](f)).mapTo[Option[AccountState]]

    onComplete(contains) { r =>
      ApiResponse(r.toOption.flatten.fold {
        AccountState.empty(pubKeyProposition).asJson
      } { state =>
        state.asJson
      })
    }

  }

  def block: Route = (get & path("blocks" / Segment)) { encodedId: String =>

    def f(v: CurrentView[HIS, MS, VL, MP]): Option[PM] =
      encodedId match {
        case "best" => Some(v.history.bestBlock)
        case "genesis" => Some(v.history.genesis)
        case t =>
          Try(t.toInt).toEither match {
            case Left(_) => v.history.modifierById(ModifierId @@ t)
            case Right(blockHeight) => v.history.blockAt(blockHeight)
          }
      }

    val contains = (nodeViewHolderRef ? GetDataFromCurrentView[HIS, MS, VL, MP, Option[PM]](f)).mapTo[Option[PM]]

    onComplete(contains) { r =>
      r.toOption.flatten.fold {
        ApiResponse(
          "contains" -> false.asJson
        )
      } { block =>
        ApiResponse(
          "contains" -> true.asJson,
          "block" -> block.asJson
        )
      }
    }

  }

  def containsModifier: Route = (get & path("contains" / Segment)) { encodedId =>
    def f(v: CurrentView[HIS, MS, VL, MP]): Option[PM] = v.history.modifierById(ModifierId @@ encodedId)

    val contains = (nodeViewHolderRef ? GetDataFromCurrentView[HIS, MS, VL, MP, Option[PM]](f)).mapTo[Option[PM]]

    onComplete(contains) { r =>
      ApiResponse(
        "id" -> encodedId.asJson,
        "contains" -> r.toOption.flatten.isDefined.asJson
      )
    }
  }

  def generateKeyPair: Route = (get & path("genpk")) {

    val bytes = Array[Byte](100)
    random.nextBytes(bytes)

    val (priv: PrivateKey25519, pub: PublicKey25519Proposition) =
      PrivateKey25519Companion.generateKeys(bytes)

    ApiResponse(
      "address" -> pub.toString.asJson,
      "pk" -> encoder.encode(priv.privKeyBytes).asJson
    )
  }

}