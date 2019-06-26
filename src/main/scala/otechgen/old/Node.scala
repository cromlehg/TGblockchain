package otechgen.old

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import play.api.libs.functional.syntax._
import play.api.libs.json.{Reads, _}
import play.api.libs.ws.JsonBodyWritables._
import play.api.libs.ws.ahc.StandaloneAhcWSClient

import scala.concurrent.duration.{Duration, _}
import scala.concurrent.{Await, Future}
import scala.io.StdIn
import scala.util.Try


case class NodeAddress(val ip: String, val port: Int) {

  val str = "http://" + ip + ":" + port

  override def toString = "Node(" + str + ")"

}

object NodeAddress {

  implicit val locationReads: Reads[NodeAddress] = (
    (JsPath \ "ip").read[String] and
      (JsPath \ "port").read[Int]
    ) (NodeAddress.apply _)

}


/**
  *
  * Add node:
  *
  * curl -X POST -H "Content-Type:application/json" -d "{\"ip\":\"127.0.0.1\", \"port\":8080}" localhost:8080/nodes/add
  *
  * @param account
  * @param storage
  */
class Node(val name: String, account: Option[Account], val storage: Storage, nodeAddress: NodeAddress, isMiner: Boolean) extends APIMarshallers {

  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  import scala.concurrent.Future.{successful => future}


  val ws = StandaloneAhcWSClient()


  val blockchain = new Blockchain(storage, block => {
    system.scheduler.scheduleOnce(0 milliseconds) {

      println("Node " + name + ": Broadcast block send starts for each node: ")

      val jsValue = Block.blockWrites.writes(block)

      val task: Future[Seq[Unit]] = storage.getAllNodes.flatMap(t => Future.sequence(t.map { nodeAddr =>
        ws.url(nodeAddr.str + "/blocks/add")
          .addHttpHeaders("Accept" -> "application/json")
          .addHttpHeaders("Content-Type" -> "application/json")
          .post(jsValue).map { response =>
          println("Node " + name + ": Block  " + block.header.id + ", hash = " + block.header.hash + " sended to node " + nodeAddr.str + ". Result : " + response.status)
        }
      }))

      Await.result(task, Duration.Inf)

    }
  })

  def start {

    println("Node " + name + ": starting...")


    val route =
      pathPrefix("blocks") {
        pathPrefix("block") {
          path(Segment) { id =>
            get {
              onSuccess(storage.getBlockByIdOrHash(id)) { blockOpt =>
                blockOpt.fold(complete(NotFound)) { block =>
                  complete(ToResponseMarshallable(block))
                }
              }
            }
          }
        } ~ pathPrefix("add") {
          post {
            entity(as[Block]) { target =>
              onSuccess(blockchain.append(target)) { result =>
                println("Node " + name + ": try to add block from external node")
                complete(result.fold(e => StatusCodes.UnprocessableEntity, _ => StatusCodes.OK))
              }
            }
          }
        }
      } ~ pathPrefix("nodes") {
        pathPrefix("add") {
          post {
            entity(as[NodeAddress]) { target =>
              onSuccess(storage.addNode(target)) { _ =>
                complete(StatusCodes.OK)
              }
            }
          }
        }
      }

    println("Node " + name + ": starting network layer...")

    system.scheduler.scheduleOnce(0 milliseconds) {
      // `route` will be implicitly converted to `Flow` using `RouteResult.route2HandlerFlow`
      val bindingFuture = Http().bindAndHandle(route, nodeAddress.ip, nodeAddress.port)
      println(s"Node " + name + " started at http://" + nodeAddress.ip + ":" + nodeAddress.port + "/\nPress RETURN to stop...")
      StdIn.readLine() // let it run until user presses return
      bindingFuture
        .flatMap(_.unbind()) // trigger unbinding from the port
        .onComplete(_ => system.terminate()) // and shutdown when done
    }

    println("Node " + name + ": network layer started")


    if(isMiner) {
      val miner: Option[Miner] = account.map(a => new Miner(a, blockchain))
      println("Node " + name + ": miner prepared")

      system.scheduler.scheduleOnce(50 milliseconds) {
        miner.map(_.mine)
      }
      println("Node " + name + ": miner started")
    }


    println("Node " + name + ": node prepared!")

  }

}