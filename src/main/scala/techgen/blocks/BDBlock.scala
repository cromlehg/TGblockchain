package techgen.blocks

import scorex.core.utils.ScorexEncoding
import io.circe.Encoder
import io.circe.syntax._
import techgen.ByteStr
import techgen.transaction.{AbstractBDTransaction, BDTransactionSerializer}
import scorex.core.block.Block
import scorex.core.block.Block.Version
import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import scorex.core.transaction.proof.{Signature25519, Signature25519Serializer}
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.{ModifierTypeId, bytesToId, idToBytes}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.crypto.signatures.{Curve25519, Signature}
import scorex.util.ModifierId
import scorex.util.serialization.{Reader, Writer}

import scala.util.Try


case class BDBlock(val height: Long,
                   val version: Version,
                   val timestamp: Long,
                   val parentId: ModifierId,
                   val generatorBox: PublicKey25519Proposition,
                   val signature: Signature25519,
                   val transactions: Seq[AbstractBDTransaction],
                   val hash: Digest32) extends Block[AbstractBDTransaction] {

  override val modifierTypeId: ModifierTypeId = BDBlock.BDBlockModifierTypeId

  override val id: ModifierId = bytesToId(hash)

  override def toString =
    s"Block{${height},${version},${timestamp},${parentId},${generatorBox},${signature},${ByteStr(hash).base64}"

}

object BDBlock extends ScorexEncoding {

  val VERSION = 0: Byte

  val BDBlockModifierTypeId: ModifierTypeId = ModifierTypeId @@ 10.toByte

  val emptySign = Signature25519(Signature @@ Array.fill(Curve25519.SignatureLength)(0: Byte))

  val emptyHash = Digest32(Array.fill(32)(0: Byte))

  val emptyParentId = bytesToId(Array.fill(32)(0: Byte))

  implicit val apiEncoder: Encoder[BDBlock] =
    Encoder.forProduct8("id",
      "height",
      "version",
      "timestamp",
      "parentId",
      "generatorBox",
      "signature",
      "transactions",
    ) { u =>
      (encoder.encode(u.id).asJson,
        u.height.asJson,
        u.version.asJson,
        u.timestamp.asJson,
        encoder.encode(u.parentId).asJson,
        encoder.encode(u.generatorBox.toString).asJson,
        encoder.encode(u.signature.signature).asJson,
        u.transactions.asJson,
      )
    }

  def genesisBlock(publicKey: PublicKey25519Proposition, privateKey: PrivateKey25519) =
    BDBlock.create(0,
      System.currentTimeMillis(),
      BDBlock.emptyParentId,
      publicKey,
      Seq.empty,
      privateKey)

  def createUnsigned(height: Long,
                     version: Version,
                     timestamp: Long,
                     parentId: ModifierId,
                     generatorBox: PublicKey25519Proposition,
                     transactions: Seq[AbstractBDTransaction]) =
    BDBlock(height,
      version,
      timestamp,
      parentId,
      generatorBox,
      emptySign,
      transactions,
      emptyHash)

  def createEmptySigned(height: Long,
                        version: Version,
                        timestamp: Long,
                        parentId: ModifierId,
                        generatorBox: PublicKey25519Proposition,
                        signature: Signature25519) =
    BDBlock(height,
      version,
      timestamp,
      parentId,
      generatorBox,
      emptySign,
      Seq.empty,
      emptyHash)


  def createEmptyUnsigned(height: Long,
                          version: Version,
                          timestamp: Long,
                          parentId: ModifierId,
                          generatorBox: PublicKey25519Proposition) =
    createUnsigned(height,
      version,
      timestamp,
      parentId,
      generatorBox,
      Seq.empty)

  def create(height: Long,
             timestamp: Long,
             parentId: ModifierId,
             generatorBox: PublicKey25519Proposition,
             transactions: Seq[AbstractBDTransaction],
             privateKey: PrivateKey25519) =
    signAndSetHash(createUnsigned(height,
      VERSION,
      timestamp,
      parentId,
      generatorBox,
      transactions),
      privateKey)

  def createEmpty(height: Long,
                  version: Version,
                  timestamp: Long,
                  parentId: ModifierId,
                  generatorBox: PublicKey25519Proposition,
                  privateKey: PrivateKey25519) =
    signAndSetHash(createUnsigned(height,
      version,
      timestamp,
      parentId,
      generatorBox,
      Seq.empty),
      privateKey)

  def emptyHashBlock(block: BDBlock): BDBlock =
    block.copy(hash = emptyHash)

  def unsignedBlock(block: BDBlock): BDBlock =
    block.copy(signature = emptySign, hash = emptyHash)

  def signAndSetHash(block: BDBlock, privateKey: PrivateKey25519): BDBlock =
    setHash(sign(block, privateKey))

  def setHash(block: BDBlock): BDBlock =
    block.copy(hash = calculateHash(block))

  def checkHash(block: BDBlock): Boolean =
    ByteStr(calculateHash(emptyHashBlock(block))) == ByteStr(block.hash)

  def calculateHash(block: BDBlock): Digest32 =
    Blake2b256(BDBlockSerializer.toBytes(block))

  def sign(block: BDBlock, privateKey: PrivateKey25519): BDBlock =
    block.copy(signature = calculateSignature(block, privateKey))

  def calculateSignature(block: BDBlock, privateKey: PrivateKey25519): Signature25519 = {
    require(java.util.Arrays.equals(block.generatorBox.pubKeyBytes, privateKey.publicKeyBytes))
    Signature25519(Curve25519.sign(privateKey.privKeyBytes, BDBlockSerializer.toByteString(block).toArray))
  }

  def signatureValid(block: BDBlock): Boolean = {
    val unsigned = unsignedBlock(block)
    val unsignedBytes = BDBlockSerializer.toByteString(unsigned).toArray
    block.generatorBox.verify(unsignedBytes, block.signature.signature)
  }

  def toBase64String(block: BDBlock): String =
    ByteStr(BDBlockSerializer.toBytes(block)).base64

  def fromBase64String(block: String): Try[BDBlock] =
    ByteStr.decodeBase64(block).map(_.arr).map(BDBlockSerializer.parseBytes)

}

object BDBlockSerializer extends ScorexSerializer[BDBlock] {

  override def serialize(obj: BDBlock, w: Writer): Unit = {
    w.putLong(obj.height)
    w.put(obj.version)
    w.putLong(obj.timestamp)
    w.putBytes(idToBytes(obj.parentId))
    PublicKey25519PropositionSerializer.serialize(obj.generatorBox, w)
    Signature25519Serializer.serialize(obj.signature, w)
    w.putInt(obj.transactions.size)
    obj.transactions.foreach(tx => BDTransactionSerializer.serialize(tx, w))
    w.putBytes(obj.hash)
  }

  override def parse(r: Reader): BDBlock = {
    val height = r.getLong()
    val version = r.getByte()
    val timestamp = r.getLong()
    val parentId = bytesToId(r.getBytes(32))
    val generatorBox = PublicKey25519PropositionSerializer.parse(r)
    val signature = Signature25519Serializer.parse(r)
    val txSize = r.getInt()
    val txs = (0 until txSize) map (_ => BDTransactionSerializer.parse(r))
    val hash = Digest32(r.getBytes(32))
    BDBlock(height,
      version,
      timestamp,
      parentId,
      generatorBox,
      signature,
      txs,
      hash)
  }

}