package techgen.nodeView

import akka.actor.ActorRef
import techgen.blocks.{BDBlock, BDBlockSerializer}
import techgen.nodeViewHolder.{BDBlockchain, BDMempool, BDSyncInfo, BDSyncInfoMessageSpec}
import techgen.transaction.{AbstractBDTransaction, BDTransactionSerializer}
import scorex.core.network.NodeViewSynchronizer
import scorex.core.settings.NetworkSettings
import scorex.core.transaction.Transaction
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext

class BDNodeViewSynchronizer(networkControllerRef: ActorRef,
                             viewHolderRef: ActorRef,
                             syncInfoSpec: BDSyncInfoMessageSpec.type,
                             networkSettings: NetworkSettings,
                             timeProvider: NetworkTimeProvider)(implicit ex: ExecutionContext) extends
  NodeViewSynchronizer[AbstractBDTransaction, BDSyncInfo, BDSyncInfoMessageSpec.type, BDBlock,
    BDBlockchain, BDMempool](networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider,
    Map(BDBlock.BDBlockModifierTypeId -> BDBlockSerializer, Transaction.ModifierTypeId -> BDTransactionSerializer)
  )