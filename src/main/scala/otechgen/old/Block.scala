package otechgen.old

import com.google.common.primitives.{Ints, Longs}
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes, _}
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PrivateKey}

case class BlockData(val data: String)

object BlockData {

  def getBytes(blockData: BlockData) =
    blockData.data.getBytes()

  implicit val blockDataReads: Reads[BlockData] =
    (__ \ "name").read[String].map(t => BlockData(t))

  implicit lazy val blockDataWrites: Writes[BlockData] = new Writes[BlockData] {

    def writes(t: BlockData) = Json.obj(
      "data" -> t.data
    )
  }


}

case class BlockHeader(
                        val id: Long,
                        val version: Int,
                        val timestamp: Long,
                        val prevBlockHash: ByteStr,
                        val signerPublicKey: ByteStr,
                        val signature: ByteStr,
                        val hash: ByteStr) {

  lazy val hashStr = hash.toString

}

object BlockHeader {


  implicit val blockHeaderReads: Reads[BlockHeader] = (
    (JsPath \ "id").read[Long] and
      (JsPath \ "version").read[Int] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "prev_block_hash").read[String].map(ByteStr.decodeBase58).map(_.get) and
      (JsPath \ "signer_public_key").read[String].map(ByteStr.decodeBase58).map(_.get) and
      (JsPath \ "signature").read[String].map(ByteStr.decodeBase58).map(_.get) and
      (JsPath \ "hash").read[String].map(ByteStr.decodeBase58).map(_.get)
    ) (BlockHeader.apply _)


  implicit lazy val blockHeaderWrites: Writes[BlockHeader] = new Writes[BlockHeader] {

    def writes(t: BlockHeader) = Json.obj(
      "id" -> t.id,
      "version" -> t.version,
      "timestamp" -> t.timestamp,
      "prev_block_hash" -> t.prevBlockHash.toString,
      "signer_public_key" -> t.signerPublicKey.toString,
      "signature" -> t.signature.toString,
      "hash" -> t.hash.toString
    )
  }

}


case class Block(val header: BlockHeader,
                 val data: Option[BlockData])

object Block {

  val VERSION = 1

  implicit val blockReads: Reads[Block] = (
    (JsPath \ "header").read[BlockHeader] and
      (JsPath \ "data").readNullable[BlockData]
    ) (Block.apply _)


  implicit lazy val blockWrites: Writes[Block] = new Writes[Block] {

    import BlockData.blockDataWrites
    import BlockHeader.blockHeaderWrites

    def writes(t: Block) = {
      val jsObj = Json.obj(
        "header" -> t.header
      )
      t.data.fold(jsObj) { data =>
        jsObj ++ Json.obj("data" -> data)
      }
    }
  }


  def genesis(account: Account): Block =
    genesis(account.privKey, account.privKey)

  def genesis(signerPrivateKey: ByteStr, signerPublicKey: ByteStr): Block =
    performBlock(None, None, signerPrivateKey, signerPublicKey)

  def performSignatureBytes(block: Block): ByteStr =
    performSignatureBytes(
      block.data,
      block.header.id,
      block.header.version,
      block.header.prevBlockHash,
      block.header.timestamp,
      block.header.signerPublicKey)

  def performSignatureBytes(
                             blockData: Option[BlockData],
                             id: Long,
                             version: Int,
                             prevBlockHash: ByteStr,
                             timestamp: Long,
                             signerPublicKey: ByteStr): ByteStr =
    ByteStr(Longs.toByteArray(id) ++
      Ints.toByteArray(version) ++
      Longs.toByteArray(timestamp) ++
      prevBlockHash.arr ++
      signerPublicKey.arr ++
      blockData.map(BlockData.getBytes).getOrElse(Array.empty))

  def performSignature(
                        signerPrivateKey: ByteStr,
                        dataToSign: ByteStr): ByteStr =
    ByteStr(Curve25519.sign(PrivateKey @@ signerPrivateKey.arr, dataToSign.arr))

  def performHash(block: Block, signatureBytes: ByteStr): ByteStr =
    performHash(
      block.header.signerPublicKey,
      signatureBytes,
      block.header.signature)

  def performHash(
                   signerPublicKey: ByteStr,
                   signatureBytes: ByteStr,
                   signature: ByteStr): ByteStr =
    ByteStr(Blake2b256.hash(signatureBytes.arr ++ signerPublicKey.arr ++ signature.arr))

  def performBlockHeader(
                          prevBlockHeader: Option[BlockHeader],
                          blockData: Option[BlockData],
                          signerPublicKey: ByteStr,
                          signerPrivateKey: ByteStr): BlockHeader = {
    val id = prevBlockHeader.map(_.id + 1).getOrElse(0L)
    val version = VERSION
    val prevBlockHash = prevBlockHeader.map(_.hash).getOrElse(ByteStr(Array.fill[Byte](32)(0)))
    val timestamp = System.currentTimeMillis()
    val signatureBytes = performSignatureBytes(
      blockData,
      id,
      version,
      prevBlockHash,
      timestamp,
      signerPublicKey)
    val signature = performSignature(signerPrivateKey, signatureBytes)
    val hash = performHash(signerPublicKey, signatureBytes, signature)
    BlockHeader(
      prevBlockHeader.map(_.id + 1).getOrElse(0),
      VERSION,
      timestamp,
      prevBlockHeader.map(_.hash).getOrElse(ByteStr(Array.fill[Byte](32)(0))),
      signerPublicKey,
      signature,
      hash)
  }

  def performBlock(
                    prevBlock: Option[Block],
                    blockData: Option[BlockData],
                    signerPublicKey: ByteStr,
                    signerPrivateKey: ByteStr): Block =
    Block(
      performBlockHeader(
        prevBlock.map(_.header),
        blockData,
        signerPublicKey,
        signerPrivateKey),
      blockData)

  def performBlock(
                    prevBlock: Option[Block],
                    blockData: Option[BlockData],
                    account: Account): Block =
    performBlock(prevBlock, blockData, account.pubKey, account.privKey)

}