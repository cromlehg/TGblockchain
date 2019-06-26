package otechgen.old

import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Blockchain(val storage: Storage, f: Block => Unit) {

  import scala.concurrent.Future.{successful => future}

  def append(block: Block): Future[Either[Throwable, Block]] =
    validate(block).flatMap(_ match {
      case Right(_) =>
        println("Try to add validated block")
        storage.addBlock(block) map { _ =>
          f(block)
          Right(block)
        }
      case t => future(t)
    })

  def validate(block: Block): Future[Either[Throwable, Block]] = {
    val signatureBytes = Block.performSignatureBytes(block)
    storage.existsBlockByIdAndHash(block.header.id, block.header.hash) flatMap { exists =>
      if (exists)
        future(Left(ValidationException("Already exists!")))
      else
        storage.getBlockByHash(block.header.prevBlockHash).map { prevBlock =>
          for {
            _ <- prevBlock.toRight(ValidationException("Not found prev block!"))
            _ <- Either.cond(Block.performHash(block, signatureBytes) == block.header.hash,
              block,
              ValidationException("Hash not valid"))
            _ <- Either.cond(Curve25519.verify(Signature @@ block.header.signature.arr,
              signatureBytes.arr,
              PublicKey @@ block.header.signerPublicKey.arr),
              block,
              ValidationException("Signature is not valid"))
          } yield block
        }
    }
  }

}