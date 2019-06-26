package techgen

import scorex.core.block.Block
import scorex.core.transaction.Transaction

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.util.Try
import techgen.transaction.AbstractBDTransaction
import techgen.blocks.BDBlock

trait Storage[TX <: Transaction, B <: Block[TX]] {

  def getLastBlock: Future[B]

  def addBlock(block: B): Future[B]

  def getBlockByHash(hash: ByteStr): Future[Option[B]]

  def getBlockById(id: Long): Future[Option[B]]

  def getBlockByIdOrHash(id: String): Future[Option[B]]

  def existsBlockByIdAndHash(id: Long, hash: ByteStr): Future[Boolean]

}

class MemoryStorage(genesis: BDBlock) extends Storage[AbstractBDTransaction, BDBlock] with WithAtomicOperations {

  import scala.concurrent.Future.{successful => future}

  private val blocks = ListBuffer(genesis)

  private val sinks = ListBuffer(genesis)

  override def existsBlockByIdAndHash(id: Long, hash: ByteStr): Future[Boolean] =
    atomicFuture(future(blocks.exists(b => b.id == id && b.hash == hash)))

  override def getLastBlock: Future[BDBlock] =
    atomicFuture(future(sinks.maxBy(_.id)))

  override def addBlock(block: BDBlock): Future[BDBlock] = atomicFuture {
    future{
      blocks += block
      sinks.find(_.id == block.parentId) foreach { found =>
        sinks -= found
        sinks += block
      }
      println("Block added: " + block.id + ", timestamp = " + block.timestamp)
      block
    }
  }

  override def getBlockByIdOrHash(r: String): Future[Option[BDBlock]] =
    Try(getBlockById(r.toLong)).orElse(ByteStr.decodeBase58(r).map(getBlockByHash)).getOrElse(future(None))

  override def getBlockByHash(hash: ByteStr): Future[Option[BDBlock]] =
    atomicFuture(future(blocks.find(_.hash == hash)))

  override def getBlockById(id: Long): Future[Option[BDBlock]] =
    atomicFuture(future(blocks.find(_.id == id)))

}