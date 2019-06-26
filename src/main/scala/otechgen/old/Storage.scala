package otechgen.old

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.util.Try


trait Storage {

  def getLastBlock: Future[Block]

  def addBlock(block: Block): Future[Block]

  def addNode(nodeAddr: NodeAddress): Future[NodeAddress]

  def getAllNodes: Future[Seq[NodeAddress]]

  def getBlockByHash(hash: ByteStr): Future[Option[Block]]

  def getBlockById(id: Long): Future[Option[Block]]

  def getBlockByIdOrHash(id: String): Future[Option[Block]]

  def existsBlockByIdAndHash(id: Long, hash: ByteStr): Future[Boolean]

}

class MemoryStorage(genesis: Block) extends Storage with WithAtomicOperations {

  import scala.concurrent.Future.{successful => future}

  private val blocks = ListBuffer(genesis)

  private val sinks = ListBuffer(genesis)

  private val nodes = ListBuffer[NodeAddress]()

  override def existsBlockByIdAndHash(id: Long, hash: ByteStr): Future[Boolean] =
    atomicFuture(blocks.exists(b => b.header.id == id && b.header.hash == hash))

  override def getAllNodes: Future[Seq[NodeAddress]] =
    atomicFuture(nodes.clone)

  override def addNode(nodeAddr: NodeAddress): Future[NodeAddress] =
    atomicFuture {
      nodes += nodeAddr
      nodeAddr
    }

  override def getLastBlock: Future[Block] =
    atomicFuture(sinks.maxBy(_.header.id))

  override def addBlock(block: Block): Future[Block] = atomicFuture {
    blocks += block
    sinks.find(_.header.hash == block.header.prevBlockHash) foreach { found =>
      sinks -= found
      sinks += block
    }
    println("Block added: " + block.header.id + ", timestamp = " + block.header.timestamp)
    block
  }

  override def getBlockByIdOrHash(r: String): Future[Option[Block]] =
    Try(getBlockById(r.toLong)).orElse(ByteStr.decodeBase58(r).map(getBlockByHash)).getOrElse(future(None))

  override def getBlockByHash(hash: ByteStr): Future[Option[Block]] =
    atomicFuture(blocks.find(_.header.hash == hash))

  override def getBlockById(id: Long): Future[Option[Block]] =
    atomicFuture(blocks.find(_.header.id == id))

}