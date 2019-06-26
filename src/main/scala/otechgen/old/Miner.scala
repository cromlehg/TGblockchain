package otechgen.old

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Miner(account: Account, blockchain: Blockchain) {

  val blockInterval = 10000

  val minerInterval = blockInterval / 5

  import scala.concurrent.Future.{successful => future}

  def mineTask: Future[Either[Throwable, Block]] =
    blockchain.storage.getLastBlock.flatMap { lastBlock =>
//      println("last block: id = " +
//        lastBlock.header.id +
//        ", timestamp = " +
//        lastBlock.header.timestamp +
//        ", timediff = " +
//        (System.currentTimeMillis() - lastBlock.header.timestamp) +
//        ", interval = " +
//        blockInterval)
      if (lastBlock.header.timestamp + blockInterval < System.currentTimeMillis()) {
        val block = Block.performBlock(Some(lastBlock), None, account: Account)
        blockchain.append(block)
      } else {
        future(Left(ValidationException("Not valid")))
      }
    }

  def mine: Future[Either[Throwable, Block]] =
    mineTask.flatMap { _ =>
      Thread.sleep(minerInterval)
      mine
    }

}