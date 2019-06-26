package techgen.nodeViewHolder

import techgen.transaction.AbstractBDTransaction
import scorex.core.transaction.MemoryPool
import scorex.util.ModifierId

import scala.collection.concurrent.TrieMap
import scala.util.Try


case class BDMempool(unconfirmed: TrieMap[ModifierId, AbstractBDTransaction] = TrieMap())
  extends MemoryPool[AbstractBDTransaction, BDMempool] {

  override type NVCT = BDMempool

  override def modifierById(id: ModifierId): Option[AbstractBDTransaction] = unconfirmed.get(id)

  override def put(tx: AbstractBDTransaction): Try[BDMempool] = put(Seq(tx))

  override def put(txs: Iterable[AbstractBDTransaction]): Try[BDMempool] = Try {
    // todo some checks here
    putWithoutCheck(txs)
  }

  override def putWithoutCheck(txs: Iterable[AbstractBDTransaction]): BDMempool = {
    val unique = txs.map(tx => tx.id -> tx).toMap.values
    val newTransactions = unique.filter(tx => !unconfirmed.contains(tx.id)).take(BDMempool.Limit - unconfirmed.size)
    newTransactions.foreach(tx => unconfirmed.put(tx.id, tx))
    this
  }

  override def remove(tx: AbstractBDTransaction): BDMempool = {
    unconfirmed.remove(tx.id)
    this
  }

  override def filter(condition: AbstractBDTransaction => Boolean): BDMempool = {
    unconfirmed.retain { (_, v) =>
      condition(v)
    }
    this
  }

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[AbstractBDTransaction] = ids.flatMap(modifierById)

  override def size: Int = unconfirmed.size

  override def take(limit: Int): Iterable[AbstractBDTransaction] =
    unconfirmed.values.take(limit)

}

object BDMempool {
  val Limit = 500

  val empty: BDMempool = BDMempool()
}