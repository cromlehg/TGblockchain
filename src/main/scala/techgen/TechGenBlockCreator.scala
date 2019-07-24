package techgen

import techgen.blocks.BDBlock
import scorex.core.transaction.state.PrivateKey25519Companion

import scala.language.postfixOps

object TechGenBlockCreator extends App {

  val (privKey, pubKey) = PrivateKey25519Companion.generateKeys("genesis".getBytes)

  val genesis = BDBlock.genesisBlock(pubKey, privKey)

  val string = BDBlock.toBase64String(genesis)

  println("Private key:")
  println(privKey)
  println("public key:")
  println(pubKey)
  println("Genesis:")
  println(string)

}
