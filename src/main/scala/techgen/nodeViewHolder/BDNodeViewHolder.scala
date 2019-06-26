package techgen.nodeViewHolder

import akka.actor.{ActorRef, ActorSystem, Props}
import techgen.NoveViewHolder.NVHSettings
import techgen.blocks.BDBlock
import techgen.transaction.AbstractBDTransaction
import scorex.core.NodeViewHolder
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider

class BDNodeViewHolder(val genesis: BDBlock, val nvhSettings: NVHSettings,
                       timeProvider: NetworkTimeProvider)
  extends NodeViewHolder[AbstractBDTransaction, BDBlock] {

  override type SI = BDSyncInfo
  override type HIS = BDBlockchain
  override type MS = BDState
  override type VL = BDWallet
  override type MP = BDMempool

  implicit override lazy val scorexSettings: ScorexSettings = nvhSettings.scorexSettings

  override def restoreState(): Option[(BDBlockchain, BDState, BDWallet, BDMempool)] = None

  override protected def genesisState: (BDBlockchain, BDState, BDWallet, BDMempool) =
    BDNodeViewHolder.generateGenesisState(genesis, nvhSettings)

}

object BDNodeViewHolder {

  def generateGenesisState(genesis: BDBlock, nvhSettings: NVHSettings):
  (BDBlockchain, BDState, BDWallet, BDMempool) = {
    val settings: ScorexSettings = nvhSettings.scorexSettings
    (BDBlockchain.empty(genesis), BDState.empty(genesis), BDWallet.empty, BDMempool.empty)
  }

}

object BDNodeViewHolderRef {

  def props(genesis: BDBlock, settings: NVHSettings,
            timeProvider: NetworkTimeProvider): Props =
    Props(new BDNodeViewHolder(genesis, settings: NVHSettings, timeProvider))

  def apply(genesis: BDBlock,
            settings: NVHSettings,
            timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(genesis, settings, timeProvider))

}