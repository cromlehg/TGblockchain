package otechgen.old

import java.util.concurrent.locks.ReentrantLock
import scala.concurrent.Future

trait WithAtomicOperations {

  import scala.concurrent.Future.{ successful => future }

  val lock = new ReentrantLock()

  def atomicFuture[T](f: => T): Future[T] =
    future(atomic(f))

  def atomic[T](f: => T): T = {
    lock.lock()
    try f finally lock.unlock()
  }

}