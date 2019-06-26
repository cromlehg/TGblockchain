package techgen.NoveViewHolder

import com.typesafe.config.Config
import techgen.Keys
import techgen.blocks.BDBlock
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader
import scorex.core.settings.ScorexSettings.readConfigFromPath
import scorex.core.settings.{SettingsReaders, _}
import scorex.core.transaction.state.PrivateKey25519
import scorex.util.ScorexLogging

case class NVHSettings(val scorexSettings: ScorexSettings,
                       val genesis: Option[BDBlock],
                       val secret: Option[PrivateKey25519])

object NVHSettings extends ScorexLogging with SettingsReaders {

  def read(userConfigPath: Option[String]): NVHSettings = {
    fromConfig(readConfigFromPath(userConfigPath, "scorex"))
  }

  implicit val networkSettingsValueReader: ValueReader[NVHSettings] =
    (cfg: Config, path: String) => fromConfig(cfg.getConfig(path))

  private def fromConfig(config: Config): NVHSettings = {
    log.info(config.toString)
    val genesisString = config.as[Option[String]]("scorex.genesis")
    val genesisSecret = config.as[Option[String]]("scorex.secret")
    val scorexSettings = config.as[ScorexSettings]("scorex")
    NVHSettings(scorexSettings,
      genesisString.flatMap(t => BDBlock.fromBase64String(t).toOption),
      genesisSecret.flatMap(t => Keys.decodePriv(t)))
  }
}