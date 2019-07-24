package techgen

import techgen.blocks.BDBlock
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.util.{ScorexEncoding, ScorexLogging}
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.transaction.state.PrivateKey25519Serializer
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}

object TechGenKeysCreateApp extends App with ScorexEncoding {

  val (priv: PrivateKey25519, pub: PublicKey25519Proposition) =
     PrivateKey25519Companion.generateKeys("genesis".getBytes)
     
   println("PRIVATE: ")
   println(encoder.encode(priv.privKeyBytes))
   println("PUBLIC: ")
   println(pub.toString())

}
