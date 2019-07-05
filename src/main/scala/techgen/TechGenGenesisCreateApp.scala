package techgen

import techgen.blocks.BDBlock
import scorex.core.transaction.state.PrivateKey25519Companion


object TechGenGenesisCreateApp extends App {

  val genesisAccount = PrivateKey25519Companion.generateKeys("genesis".getBytes)

  val block = BDBlock.createEmpty(1,
    BDBlock.VERSION,
    System.currentTimeMillis(),
    BDBlock.emptyParentId,
    genesisAccount._2,
    genesisAccount._1)

  println(BDBlock.toBase64String(block))

}
