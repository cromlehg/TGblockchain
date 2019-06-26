package techgen.nodeViewHolder

import techgen.blocks.BDBlock
import techgen.transaction.AbstractBDTransaction
import scorex.core.VersionTag
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.transaction.wallet.Vault
import scorex.util.ScorexLogging
import techgen.transaction.Value

import scala.util.Try

case class BDWallet(secret: PrivateKey25519, accountState: AccountState)
  extends Vault[AbstractBDTransaction, BDBlock, BDWallet] with ScorexLogging {

  override type NVCT = BDWallet

  // we don't care about transactions in mempool
  override def scanOffchain(tx: AbstractBDTransaction): BDWallet = this

  // we don't care about transactions in mempool
  override def scanOffchain(txs: Seq[AbstractBDTransaction]): BDWallet = this

  override def scanPersistent(modifier: BDBlock): BDWallet = {
    val txs = modifier.transactions
//    val spentIds = txs.flatMap(_.inputs)
//    val remainingBoxes = boxes.filter(b => !spentIds.exists(_ sameElements b.id))
//    val newBoxes = txs.flatMap(_.outputs).filter(_.proposition == secret.publicImage)
//    BDWallet(secret, remainingBoxes ++ newBoxes)
    this
  }

  override def rollback(to: VersionTag): Try[BDWallet] = Try {
    // todo not implemented
    this
  }

}

object BDWallet {
  val empty: BDWallet = {
    //TODO should read seed from config
    val (secret, public) = PrivateKey25519Companion.generateKeys("secret founders seed".getBytes())
    BDWallet(secret, AccountState(public, None, Seq.empty, Value @@ 0L))
  }
}