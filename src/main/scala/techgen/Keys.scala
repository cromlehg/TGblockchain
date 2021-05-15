package techgen

import scorex.core.transaction.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Serializer}

object Keys  {

  def encodePub(pub: PublicKey25519Proposition): String =
    ByteStr(PublicKey25519PropositionSerializer.toBytes(pub)).base64

  def decodePub(str: String): Option[PublicKey25519Proposition] =
    ByteStr.decodeBase64(str).map(_.arr).map(PublicKey25519PropositionSerializer.parseBytes).toOption

  def encodePriv(priv: PrivateKey25519): String =
    ByteStr(PrivateKey25519Serializer.toBytes(priv)).base64

  def decodePriv(str: String): Option[PrivateKey25519] =
    ByteStr.decodeBase64(str).map(_.arr).map(PrivateKey25519Serializer.parseBytes).toOption

}
