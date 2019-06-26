package techgen.nodeViewHolder

import io.circe.Encoder
import io.circe.syntax._
import techgen.Name
import techgen.blocks.BDBlock
import techgen.blocks.BDBlock.encoder
import techgen.transaction.{AbstractBDTransaction, _}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.utils.ScorexEncoder
import scorex.core.{VersionTag, _}
import scorex.util.ScorexLogging

import scala.util.{Failure, Try}

case class BDState(override val version: VersionTag, state: Seq[AccountState], votes: Seq[Vote]) extends MinimalState[BDBlock, BDState]
  with ScorexLogging {

  override def applyModifier(mod: BDBlock): Try[BDState] = Try {
    log.info(s"Apply block ${mod.id} with ${mod.transactions.size} transactions to state of version $version")

    val miner = state.find(_.target == mod.generatorBox)
    loggedRequire(miner.map(_.name.isDefined).getOrElse(false), "Miner must be identified!")
    loggedRequire(miner.map(_.permissions.contains(Permissions.MINE)).getOrElse(false), "Miner not have permissions to mine block!")

    var changedState = state

    mod.transactions.sortBy(_.nonce).foreach { tx =>
      loggedRequire(BDTransaction.signatureValid(tx), "Signature not valid")
      loggedRequire(BDTransaction.checkHash(tx), "Incorrect hash")
      tx match {
        // TODO: VoteTransaction
        case t: ChangePermissionsBDTransaction =>
          val actorPerms = state.find(_.target == tx.signer).fold[Seq[Int]](Seq.empty)(_.permissions)
          t.cpos foreach { cpo =>
            val targetPerms = state.find(_.target == cpo.recipient).fold[Seq[Int]](Seq.empty)(_.permissions)
            if (cpo.isRemove) {
              cpo.permission.id match {
                case Permissions.MINE =>
                  txLoggedRequire(tx, targetPerms.contains(Permissions.MINE), "Target not have permission to remove!")
                  txLoggedRequire(tx, actorPerms.contains(Permissions.CHANGE_MINER_PERMISSION), "Actor not have corresponding permissions!")
                  changedState = state.filterNot(_.target == cpo.recipient) :+ state.find(_.target == cpo.recipient).map(as => as.copy(permissions = as.permissions.filterNot(_ == Permissions.MINE))).get
                case Permissions.CHANGE_MINER_PERMISSION =>
                  txLoggedRequire(tx, targetPerms.contains(Permissions.CHANGE_MINER_PERMISSION), "Target not have permission to remove!")
                  txLoggedRequire(tx, actorPerms.contains(Permissions.GOD_CHANGE_MINER_PERMISSION), "Actor not have corresponding permissions!")
                  changedState = state.filterNot(_.target == cpo.recipient) :+ state.find(_.target == cpo.recipient).map(as => as.copy(permissions = as.permissions.filterNot(_ == Permissions.CHANGE_MINER_PERMISSION))).get
              }
            } else {
              cpo.permission.id match {
                case Permissions.MINE =>
                  txLoggedRequire(tx, !targetPerms.contains(Permissions.MINE), "Target already have permission!")
                  txLoggedRequire(tx, actorPerms.contains(Permissions.CHANGE_MINER_PERMISSION), "Actor not have corresponding permissions!")
                  changedState = state.filterNot(_.target == cpo.recipient) :+ (state.find(_.target == cpo.recipient) match {
                    case Some(as) => as.copy(permissions = as.permissions :+ Permissions.MINE)
                    case _ => AccountState(cpo.recipient, None, Seq(Permissions.MINE), Value @@ 0L)
                  })
                case Permissions.CHANGE_MINER_PERMISSION =>
                  txLoggedRequire(tx, !targetPerms.contains(Permissions.CHANGE_MINER_PERMISSION), "Target already have permission!")
                  txLoggedRequire(tx, actorPerms.contains(Permissions.GOD_CHANGE_MINER_PERMISSION), "Actor not have corresponding permissions!")
                  changedState = state.filterNot(_.target == cpo.recipient) :+
                    (state.find(_.target == cpo.recipient) match {
                      case Some(as) => as.copy(permissions = as.permissions :+ Permissions.CHANGE_MINER_PERMISSION)
                      case _ => AccountState(cpo.recipient, None, Seq(Permissions.CHANGE_MINER_PERMISSION), Value @@ 0L)
                    })
              }
            }
          }
        case t: TransferAssetsBDTransaction =>
          val summaryToTransfer = t.amount.toLong
          txLoggedRequire(tx, state.find(_.target == t.signer).map(_.value.toLong).getOrElse(0L) >= summaryToTransfer, "Sender have not so much as transferred!")
          val newState = state.find(_.target == t.recipient) match {
            case Some(accountState) => accountState.copy(value = Value @@ (summaryToTransfer + accountState.value.toLong))
            case _ => AccountState(t.recipient, None, Seq.empty, t.amount)
          }
          val allNewStates = Seq(state.find(_.target == t.signer).map(t => t.copy(value = Value @@ (t.value.toLong - summaryToTransfer))).get, newState)
          changedState = state.filterNot(t => allNewStates.map(_.target).contains(t.target)) ++ allNewStates
        case t: IdentifyBDTransaction =>
          val foundAccount = state.find(_.target == t.signer)
          txLoggedRequire(tx, foundAccount.isEmpty, "Account must be in state")
          state.find(_.target == t.signer).foreach { account: AccountState =>
            txLoggedRequire(tx, account.name.isEmpty, "Identify operation can process only for not identified accounts!")
            txLoggedRequire(tx, account.value.toLong >= IdentifyTransaction.cost, "To identify account must have " + IdentifyTransaction.cost)
            changedState = state.filterNot(_.target == t.signer) :+ account.copy(value = Value @@ (account.value.toLong - IdentifyTransaction.cost), name = Some(t.name))
          }
        case _ => Failure(new UnsupportedOperationException("Incorrect transaction type"))
      }
    }

    BDState(idToVersion(mod.id), changedState, votes)
  }

  @inline final def txLoggedRequire(tx: AbstractBDTransaction, requirement: Boolean, message: => Any) {
    if (!requirement) {
      logger.error("Error for TX " + tx.id + " :" + message.toString)
      throw new IllegalArgumentException("requirement failed: " + message)
    }
  }

  @inline final def loggedRequire(requirement: Boolean, message: => Any) {
    if (!requirement) {
      logger.error(message.toString)
      throw new IllegalArgumentException("requirement failed: " + message)
    }
  }

  override def rollbackTo(version: VersionTag): Try[BDState] = Failure(new Error("Not supported"))

  override def maxRollbackDepth: Int = 0

  override type NVCT = this.type
}

case class AccountState(val target: PublicKey25519Proposition,
                        val name: Option[Name],
                        val permissions: Seq[Int],
                        val value: Value)

case class Vote(from: PublicKey25519Proposition, to: PublicKey25519Proposition)

object AccountState extends ScorexEncoder {

  def empty(target: PublicKey25519Proposition) =
    AccountState(target, None, Seq.empty, Value @@ 0L)

  implicit val apiEncoder: Encoder[AccountState] =
    Encoder.forProduct4("target",
      "name",
      "permissions",
      "value"
    ) { u: AccountState =>
      (encoder.encode(u.target.toString).asJson,
        u.name.asJson,
        u.permissions.asJson,
        u.value.toLong.asJson)
    }

}

object BDState {

  private def genesisState(genesisBlock: BDBlock): Seq[AccountState] =
    Seq(AccountState(genesisBlock.generatorBox,
      Some(Name.create("genesis")),
      Seq(Permissions.MINE,
        Permissions.CHANGE_MINER_PERMISSION,
        Permissions.GOD_CHANGE_MINER_PERMISSION),
      Value @@ 1000000L))

  def empty(genesisBlock: BDBlock): BDState =
    BDState(idToVersion(genesisBlock.id), genesisState(genesisBlock),
      Seq(Vote(genesisBlock.generatorBox, genesisBlock.generatorBox)))

}