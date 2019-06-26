package techgen.nodeViewHolder

import techgen.blocks.BDBlock
import techgen.mining.BDMiner
import techgen.transaction.AbstractBDTransaction
import scorex.core.consensus.BlockChain.Score
import scorex.core.consensus.History._
import scorex.core.consensus.{BlockChain, ModifierSemanticValidity}
import scorex.core.utils.ScorexEncoding
import scorex.util.ModifierId

import scala.util.{Failure, Success, Try}

case class BDBlockchain(blocks: Map[Int, BDBlock],
                        reverseMap: Map[String, Int], isValid: Map[BDBlock, Boolean])
  extends BlockChain[AbstractBDTransaction, BDBlock, BDSyncInfo, BDBlockchain] with ScorexEncoding {

  def bestBlock: BDBlock = blocks.maxBy(_._1)._2

  def genesis: BDBlock = blockAt(0).get

  override def height(): Int = blocks.keys.max

  override def heightOf(id: ModifierId): Option[Int] = reverseMap.get(id)

  override def blockAt(height: Int): Option[BDBlock] = {
    /*log.info("############ BLOCK AT ################################################")
    log.info(" length " + blocks.size)
    log.info("\n" + blocks.map(_._2).map(t => t.id + " -> " + t.height + " -> " + t.transactions.length).mkString("\n"))
    log.info("######################################################################")*/

    blocks.get(height)
  }

  override def children(blockId: ModifierId): Seq[BDBlock] = heightOf(blockId).map(_ + 1).flatMap(blockAt).toSeq

  // TODO this is simplified version
  override def score(block: BDBlock): Score = BigInt(heightOf(block).getOrElse(0))

  override def chainScore(): Score = score(bestBlock)

  override def append(block: BDBlock): Try[(BDBlockchain, ProgressInfo[BDBlock])] = Try {
    val blockHeight = height() + 1
    val progressInfo = ProgressInfo(None, Seq.empty, Seq(block), Seq.empty)
    log.info(s"Appended block ${block.id} with height $blockHeight and  ${block.transactions.length} transactions")

    /*log.info("########################## append ####################################")
    log.info(" length " + blocks.size)
    log.info("\n" + blocks.map(_._2).map(t => t.id + " -> " + t.height + " -> " + t.transactions.length).mkString("\n"))
    log.info("######################################################################")*/

    (BDBlockchain(blocks + (blockHeight -> block), reverseMap + (block.id -> blockHeight), isValid), progressInfo)
  }

  override def reportModifierIsValid(modifier: BDBlock): BDBlockchain = {
    BDBlockchain(blocks, reverseMap, isValid + (modifier -> true))
  }

  override def reportModifierIsInvalid(modifier: BDBlock, progressInfo: ProgressInfo[BDBlock]): (BDBlockchain, ProgressInfo[BDBlock]) = {
    (BDBlockchain(blocks - reverseMap(modifier.encodedId), reverseMap - modifier.encodedId, isValid + (modifier -> false)), ProgressInfo[BDBlock](None, Seq.empty, Seq.empty, Seq.empty))
  }

  override def applicableTry(block: BDBlock): Try[Unit] = Try {
    BDMiner.correctWorkDone(bestBlock, block) match {
      case Some(err) =>
        logger.info(err)
        Failure(new Error(err))
      case _ =>
        Success()
      // check transactions
    }
  }

  override def modifierById(id: ModifierId): Option[BDBlock] = reverseMap.get(id).flatMap(h => blocks.get(h))

  override def isSemanticallyValid(id: ModifierId): ModifierSemanticValidity = reverseMap.get(id) match {
    case Some(_) => ModifierSemanticValidity.Valid
    case _ => ModifierSemanticValidity.Unknown
  }

  override def syncInfo: BDSyncInfo = {
    BDSyncInfo(lastBlockIds(BDSyncInfo.idsSize))
  }

  override def compare(other: BDSyncInfo): HistoryComparisonResult = {
    val theirIds = other.ids
    theirIds.reverse.find(id => contains(id)) match {
      case Some(common) =>
        val commonHeight = heightOf(common).get
        val theirTotalHeight = theirIds.indexWhere(_ sameElements common) + commonHeight
        val ourHeight = height()
        if (theirTotalHeight == ourHeight) {
          Equal
        } else if (theirTotalHeight > ourHeight) {
          Older
        } else {
          Younger
        }
      case _ => Unknown
    }
  }

  override type NVCT = BDBlockchain

}

object BDBlockchain {

  def empty(genesis: BDBlock): BDBlockchain =
    BDBlockchain(Map(0 -> genesis), Map(genesis.encodedId -> 0), Map(genesis -> true))

}
