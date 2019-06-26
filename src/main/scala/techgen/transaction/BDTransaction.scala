package techgen.transaction

import io.circe.syntax._
import io.circe.{Encoder, Json}
import techgen.{ByteStr, Name, NameSerializer}
import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import scorex.core.transaction.proof.{Signature25519, Signature25519Serializer}
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.ScorexEncoding
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.crypto.signatures.{Curve25519, Signature}
import scorex.util.ByteArrayBuilder
import scorex.util.serialization.{Reader, VLQByteBufferWriter, Writer}

object AbstractBDTransaction extends ScorexEncoding {

  implicit val apiEncoder: Encoder[AbstractBDTransaction] =
    new Encoder[AbstractBDTransaction] {

      def apply(u: AbstractBDTransaction): Json = {
        val abstractFields = Seq("id" -> encoder.encode(u.id).asJson,
          "nonce" -> u.nonce.asJson,
          "signer" -> encoder.encode(u.signer.toString).asJson,
          "signature" -> encoder.encode(u.signature.signature).asJson)

        val concreteFields = u match {
          case t: ChangePermissionsBDTransaction => Seq("type" -> BDTransactions.TX_CHANGE_PERMISSIONS.asJson)
          case t: TransferAssetsBDTransaction =>
            Seq("type" -> BDTransactions.TX_TRANSFER_ASSETS.asJson,
              "recipient" -> encoder.encode(t.recipient.toString).asJson,
              "amount" -> t.amount.toLong.asJson)
          case t: IdentifyBDTransaction => Seq("type" -> BDTransactions.TX_IDENTIFY.asJson)
          case t: VoteBDTransaction => Seq("type" -> BDTransactions.TX_VOTE.asJson)
          case _ => Seq()
        }

        Json.obj((abstractFields ++ concreteFields): _*)

      }

    }

}

object BDTransaction {

  val emptyHash = Digest32(Array.fill(32)(0: Byte))

  val emptySign = Signature25519(Signature @@ Array.fill(Curve25519.SignatureLength)(0: Byte))

  def unsignedTx(tx: AbstractBDTransaction): AbstractBDTransaction =
    tx match {
      case t: ChangePermissionsBDTransaction => t.copy(signature = emptySign, hash = emptyHash)
      case t: TransferAssetsBDTransaction => t.copy(signature = emptySign, hash = emptyHash)
      case t: IdentifyBDTransaction => t.copy(signature = emptySign, hash = emptyHash)
      case t: VoteBDTransaction => t.copy(signature = emptySign, hash = emptyHash)
      case _ => throw new UnsupportedOperationException
    }

  def emptyHashTx(tx: AbstractBDTransaction): AbstractBDTransaction =
    tx match {
      case t: ChangePermissionsBDTransaction => t.copy(hash = emptyHash)
      case t: TransferAssetsBDTransaction => t.copy(hash = emptyHash)
      case t: IdentifyBDTransaction => t.copy(hash = emptyHash)
      case t: VoteBDTransaction => t.copy(hash = emptyHash)
      case _ => throw new UnsupportedOperationException
    }

  def signatureValid(tx: AbstractBDTransaction): Boolean = {
    val unsigned = unsignedTx(tx)
    val unsignedBytes = BDTransactionSerializer.toByteString(unsigned).toArray
    tx.signer.verify(unsignedBytes, tx.signature.signature)
  }

  def calculateHash(tx: AbstractBDTransaction): Digest32 =
    Blake2b256(BDTransactionSerializer.toBytes(tx))

  def checkHash(tx: AbstractBDTransaction): Boolean =
    ByteStr(calculateHash(emptyHashTx(tx))) == ByteStr(tx.hash)

  def setHash(tx: AbstractBDTransaction): AbstractBDTransaction =
    tx match {
      case t: ChangePermissionsBDTransaction => t.copy(hash = calculateHash(tx))
      case t: TransferAssetsBDTransaction => t.copy(hash = calculateHash(tx))
      case t: IdentifyBDTransaction => t.copy(hash = calculateHash(tx))
      case t: VoteBDTransaction => t.copy(hash = calculateHash(tx))
      case _ => throw new UnsupportedOperationException
    }

  def sign(tx: AbstractBDTransaction, privateKey: PrivateKey25519): AbstractBDTransaction =
    tx match {
      case t: ChangePermissionsBDTransaction => t.copy(signature = calculateSignature(tx, privateKey))
      case t: TransferAssetsBDTransaction => t.copy(signature = calculateSignature(tx, privateKey))
      case t: IdentifyBDTransaction => t.copy(signature = calculateSignature(tx, privateKey))
      case t: VoteBDTransaction => t.copy(signature = calculateSignature(tx, privateKey))
      case _ => throw new UnsupportedOperationException
    }

  def calculateSignature(tx: AbstractBDTransaction, privateKey: PrivateKey25519): Signature25519 = {
    require(java.util.Arrays.equals(tx.signer.pubKeyBytes, privateKey.publicKeyBytes))
    Signature25519(Curve25519.sign(privateKey.privKeyBytes, BDTransactionSerializer.toByteString(tx).toArray))
  }

  def signAndSetHash(tx: AbstractBDTransaction, privateKey: PrivateKey25519): AbstractBDTransaction =
    setHash(sign(tx, privateKey))

}

object BDTransactions {

  val TX_CHANGE_PERMISSIONS = 0

  val TX_TRANSFER_ASSETS = 1

  val TX_IDENTIFY = 2

  val TX_VOTE = 3

}

object BDTransactionSerializer extends ScorexSerializer[AbstractBDTransaction] {

  override def serialize(obj: AbstractBDTransaction, w: Writer): Unit =
    obj match {
      case t: ChangePermissionsBDTransaction =>
        w.putInt(BDTransactions.TX_CHANGE_PERMISSIONS)
        ChangePermissionsBDTransactionSerializer.serialize(t, w)
      case t: TransferAssetsBDTransaction =>
        w.putInt(BDTransactions.TX_TRANSFER_ASSETS)
        TranferAssetsBDTransactionSerializer.serialize(t, w)
      case t: IdentifyBDTransaction =>
        w.putInt(BDTransactions.TX_IDENTIFY)
        IdentifyBDTransactionSerializer.serialize(t, w)
      case t: VoteBDTransaction =>
        w.putInt(BDTransactions.TX_VOTE)
        VoteBDTransactionSerializer.serialize(t, w)
      case _ => throw new UnsupportedOperationException
    }


  override def parse(r: Reader): AbstractBDTransaction =
    r.getInt() match {
      case BDTransactions.TX_CHANGE_PERMISSIONS =>
        ChangePermissionsBDTransactionSerializer.parse(r)
      case BDTransactions.TX_TRANSFER_ASSETS =>
        TranferAssetsBDTransactionSerializer.parse(r)
      case BDTransactions.TX_IDENTIFY =>
        IdentifyBDTransactionSerializer.parse(r)
      case BDTransactions.TX_VOTE =>
        VoteBDTransactionSerializer.parse(r)
      case _ => throw new UnsupportedOperationException
    }

}

sealed abstract class AbstractBDTransaction(val nonce: Long,
                                            val signer: PublicKey25519Proposition,
                                            val signature: Signature25519,
                                            val hash: Digest32) extends Transaction

case class Permission(id: Long) {

  override def equals(in: Any): Boolean =
    in match {
      case Permission(inId) => id == inId
      case _ => false
    }

}


object PermissionSerializer extends ScorexSerializer[Permission] {

  override def serialize(obj: Permission, w: Writer): Unit =
    w.putLong(obj.id)

  override def parse(r: Reader): Permission =
    Permission(r.getLong())

}

case class ChangePermissionOperation(val recipient: PublicKey25519Proposition,
                                     val permission: Permission,
                                     val isRemove: Boolean)


object ChangePermissionOperationSerializer extends ScorexSerializer[ChangePermissionOperation] {

  override def serialize(obj: ChangePermissionOperation, w: Writer): Unit = {
    PublicKey25519PropositionSerializer.serialize(obj.recipient, w)
    PermissionSerializer.serialize(obj.permission, w)
    w.putBits(Array(obj.isRemove))
  }

  override def parse(r: Reader): ChangePermissionOperation = {
    val recepient = PublicKey25519PropositionSerializer.parse(r)
    val permission = PermissionSerializer.parse(r)
    val isRemove = r.getBits(1)(0)
    ChangePermissionOperation(recepient,
      permission,
      isRemove)
  }

}

object Permissions {

  val MINE: Int = 1

  val CHANGE_MINER_PERMISSION: Int = 2

  val GOD_CHANGE_MINER_PERMISSION: Int = 3

}

case class ChangePermissionsBDTransaction(val cpos: Seq[ChangePermissionOperation],
                                          override val nonce: Long,
                                          override val signer: PublicKey25519Proposition,
                                          override val signature: Signature25519,
                                          override val hash: Digest32)
  extends AbstractBDTransaction(nonce, signer, signature, hash) {

  override val messageToSign: Array[Byte] = {
    val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
    ChangePermissionsBDTransactionSerializer.serialize(this, writer)
    writer.result().toBytes
  }

}

case class TransferAssetsBDTransaction(val recipient: PublicKey25519Proposition,
                                       val amount: Value,
                                       override val nonce: Long,
                                       override val signer: PublicKey25519Proposition,
                                       override val signature: Signature25519,
                                       override val hash: Digest32)
  extends AbstractBDTransaction(nonce, signer, signature, hash) {

  override val messageToSign: Array[Byte] = {
    val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
    TranferAssetsBDTransactionSerializer.serialize(this, writer)
    writer.result().toBytes
  }

}

case class VoteBDTransaction(val target: PublicKey25519Proposition,
                             override val nonce: Long,
                             override val signer: PublicKey25519Proposition,
                             override val signature: Signature25519,
                             override val hash: Digest32)
  extends AbstractBDTransaction(nonce, signer, signature, hash) {

  override val messageToSign: Array[Byte] = {
    val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
    VoteBDTransactionSerializer.serialize(this, writer)
    writer.result().toBytes
  }

}

object IdentifyTransaction {

  val cost = 10000L

}

case class IdentifyBDTransaction(val name: Name,
                                 override val nonce: Long,
                                 override val signer: PublicKey25519Proposition,
                                 override val signature: Signature25519,
                                 override val hash: Digest32)
  extends AbstractBDTransaction(nonce, signer, signature, hash) {

  override val messageToSign: Array[Byte] = {
    val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
    IdentifyBDTransactionSerializer.serialize(this, writer)
    writer.result().toBytes
  }

}

object IdentifyBDTransactionSerializer extends ScorexSerializer[IdentifyBDTransaction] {

  override def serialize(obj: IdentifyBDTransaction, w: Writer): Unit = {
    w.putLong(obj.nonce)
    PublicKey25519PropositionSerializer.serialize(obj.signer, w)
    Signature25519Serializer.serialize(obj.signature, w)
    w.putBytes(obj.hash)
    NameSerializer.serialize(obj.name, w)
  }

  override def parse(r: Reader): IdentifyBDTransaction = {
    val nonce = r.getLong()
    val signer = PublicKey25519PropositionSerializer.parse(r)
    val signature = Signature25519Serializer.parse(r)
    val hash = Digest32(r.getBytes(32))
    val name = NameSerializer.parse(r)
    IdentifyBDTransaction(name,
      nonce,
      signer,
      signature,
      hash)
  }

}


object VoteBDTransactionSerializer extends ScorexSerializer[VoteBDTransaction] {

  override def serialize(obj: VoteBDTransaction, w: Writer): Unit = {
    w.putLong(obj.nonce)
    PublicKey25519PropositionSerializer.serialize(obj.signer, w)
    Signature25519Serializer.serialize(obj.signature, w)
    w.putBytes(obj.hash)
    PublicKey25519PropositionSerializer.serialize(obj.target, w)
  }

  override def parse(r: Reader): VoteBDTransaction = {
    val nonce = r.getLong()
    val signer = PublicKey25519PropositionSerializer.parse(r)
    val signature = Signature25519Serializer.parse(r)
    val hash = Digest32(r.getBytes(32))
    val target = PublicKey25519PropositionSerializer.parse(r)
    VoteBDTransaction(target,
      nonce,
      signer,
      signature,
      hash)
  }

}

object TranferAssetsBDTransactionSerializer extends ScorexSerializer[TransferAssetsBDTransaction] {

  override def serialize(obj: TransferAssetsBDTransaction, w: Writer): Unit = {
    w.putLong(obj.nonce)
    PublicKey25519PropositionSerializer.serialize(obj.signer, w)
    Signature25519Serializer.serialize(obj.signature, w)
    w.putBytes(obj.hash)
    PublicKey25519PropositionSerializer.serialize(obj.recipient, w)
    w.putLong(obj.amount)
  }

  override def parse(r: Reader): TransferAssetsBDTransaction = {
    val nonce = r.getLong()
    val signer = PublicKey25519PropositionSerializer.parse(r)
    val signature = Signature25519Serializer.parse(r)
    val hash = Digest32(r.getBytes(32))
    val recepient = PublicKey25519PropositionSerializer.parse(r)
    val amount = r.getLong()
    TransferAssetsBDTransaction(recepient,
      Value @@ amount,
      nonce,
      signer,
      signature,
      hash)
  }

}


object ChangePermissionsBDTransactionSerializer extends ScorexSerializer[ChangePermissionsBDTransaction] {

  override def serialize(obj: ChangePermissionsBDTransaction, w: Writer): Unit = {
    w.putLong(obj.nonce)
    PublicKey25519PropositionSerializer.serialize(obj.signer, w)
    Signature25519Serializer.serialize(obj.signature, w)
    w.putBytes(obj.hash)
    w.putInt(obj.cpos.length)
    obj.cpos.foreach(cpo => ChangePermissionOperationSerializer.serialize(cpo, w))
  }

  override def parse(r: Reader): ChangePermissionsBDTransaction = {
    val nonce = r.getLong()
    val signer = PublicKey25519PropositionSerializer.parse(r)
    val signature = Signature25519Serializer.parse(r)
    val hash = Digest32(r.getBytes(32))
    val cposSize = r.getInt()
    val cpos = (0 until cposSize) map (_ => ChangePermissionOperationSerializer.parse(r))
    ChangePermissionsBDTransaction(cpos,
      nonce,
      signer,
      signature,
      hash)
  }

}