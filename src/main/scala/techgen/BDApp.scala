package techgen

import akka.actor.{ActorRef, Props}
import techgen.GenesisCreateApp.block
import techgen.NoveViewHolder.NVHSettings
import techgen.api.BDApiRoute
import techgen.blocks.{BDBlock, BDBlockSerializer}
import techgen.mining.BDMiner.MineBlock
import techgen.mining.BDMinerRef
import techgen.nodeView.BDNodeViewSynchronizer
import techgen.nodeViewHolder._
import techgen.transaction.AbstractBDTransaction
import scorex.core.api.http.{ApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.PeerFeature
import scorex.core.network.message.MessageSpec
import scorex.core.settings.ScorexSettings

import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

class BDApp(configPath: String) extends {
  private val nvhSettings = NVHSettings.read(Some(configPath))
  override implicit val settings: ScorexSettings = nvhSettings.scorexSettings
  override protected val features: Seq[PeerFeature] = Seq()
} with Application {

  override type TX = AbstractBDTransaction
  override type PMOD = BDBlock
  override type NVHT = BDNodeViewHolder

  val secret = nvhSettings.secret

  val genesis = nvhSettings.genesis.orElse {
    log.info("Genesis not predefined. Start to generate new genesis")
    secret.map { t =>
      val block = BDBlock.genesisBlock(t.publicImage, t)
      log.info("New genesis generated: " + block)
      log.info("Base64 view: " + BDBlock.toBase64String(block))
      block
    }
  }.get

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(BDSyncInfoMessageSpec)

  override val nodeViewHolderRef: ActorRef = BDNodeViewHolderRef(genesis, nvhSettings, timeProvider)

  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(Props(new BDNodeViewSynchronizer(networkControllerRef, nodeViewHolderRef,
      BDSyncInfoMessageSpec, settings.network, timeProvider)))

  override val swaggerConfig: String = Source.fromResource("api.yaml").getLines.mkString("\n")

  override val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings.restApi),
    BDApiRoute(settings.restApi, nodeViewHolderRef),
    PeersApiRoute(peerManagerRef, networkControllerRef, timeProvider, settings.restApi)
  )

  if (settings.network.nodeName.contains("mining-node")) {
    secret.map { priv =>
      val miner = BDMinerRef(priv.publicImage, priv, genesis, nodeViewHolderRef, timeProvider)
      actorSystem.scheduler.scheduleOnce(3.second) {
        logger.info("Miner scheduled")
        miner ! MineBlock
      }
    }
  }
}

object BDApp {

  def main(args: Array[String]) {

    args.headOption match {
      case Some(config) =>
        new BDApp(config).run()
      case _ =>
        new BDApp("src/main/resources/node1.conf").run()
        //new BDApp("src/main/resources/node2.conf").run()
    }

  }

}