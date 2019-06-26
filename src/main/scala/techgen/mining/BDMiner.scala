package techgen.mining

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import techgen.blocks.BDBlock
import techgen.mining.BDMiner.MineBlock
import techgen.nodeViewHolder.{BDBlockchain, BDMempool}
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedMempool}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class BDMiner(publicKey: PublicKey25519Proposition,
              privateKey: PrivateKey25519, genesis: BDBlock,
              viewHolderRef: ActorRef,
              timeProvider: NetworkTimeProvider) extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ChangedMempool[_]])
    context.system.eventStream.subscribe(self, classOf[ChangedHistory[_]])
  }

  var currentMempool: BDMempool = new BDMempool
  var bestBlock = genesis

  override def receive: Receive = {
    case ChangedHistory(h: BDBlockchain@unchecked) =>
      log.info("Got blockchain history change event in miner. Best block now " + h.bestBlock.id)
      bestBlock = h.bestBlock

    case ChangedMempool(pool: BDMempool) =>
      log.info("Got mempool change event in miner. Pull length now " + pool.unconfirmed.size)
      currentMempool = pool

    case MineBlock =>
      logger.info("Start mining at height " + bestBlock.height + " with block " + bestBlock.id)
      val newBlock = constructNewBlock
      logger.info("New block constructed")
      BDMiner.correctWorkDone(bestBlock, newBlock) match {
        case Some(err) =>
          logger.info(err)
        case _ =>
          log.info(s"New block ${newBlock.encodedId} found")
          newBlock.transactions.foreach(tx => currentMempool = currentMempool.remove(tx))
          viewHolderRef ! LocallyGeneratedModifier(newBlock)

      }
      logger.info("Schedule next mine iteration")
      context.system.scheduler.scheduleOnce(10.second) {
        self ! MineBlock
      }

    case m => log.warn(s"Unexpected message $m")
  }

  private def constructNewBlock: BDBlock = {
    val txs = currentMempool.take(10).toSeq
    log.info("Construct block with " + txs.length + " transactions.")
    BDBlock.create(bestBlock.height + 1,
      timeProvider.time(),
      bestBlock.id,
      publicKey,
      txs,
      privateKey)
  }

}

object BDMiner extends ScorexLogging {

  case class MineBlock()

  val diff: Long = 2000

  def correctWorkDone(parent: BDBlock, block: BDBlock): Option[String] = {
    log.info("Parent block have height " + parent.height)
    log.info("Try to check block with height " + block.height + " and hash " + block.hash )
    if (parent.id != block.parentId)
      Some("Incorrect parent id: " + parent.id + " from parent and " + block.parentId + " from target!")
    else if (block.timestamp - parent.timestamp <= diff)
      Some("Time diff too small")
    else if (!BDBlock.signatureValid(block))
      Some("Signature not valid")
    else if (!BDBlock.checkHash(block)) {
      Some("Incorrect hash")
    } else
      None
  }

}

object BDMinerRef {

  def props(publicKey: PublicKey25519Proposition,
            privateKey: PrivateKey25519,
            genesis: BDBlock,
            viewHolderRef: ActorRef,
            timeProvider: NetworkTimeProvider): Props =
    Props(new BDMiner(publicKey: PublicKey25519Proposition,
      privateKey: PrivateKey25519, genesis: BDBlock, viewHolderRef: ActorRef, timeProvider: NetworkTimeProvider))

  def apply(publicKey: PublicKey25519Proposition,
            privateKey: PrivateKey25519, genesis: BDBlock,
            viewHolderRef: ActorRef,
            timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(publicKey: PublicKey25519Proposition,
      privateKey: PrivateKey25519, genesis, viewHolderRef, timeProvider))

}