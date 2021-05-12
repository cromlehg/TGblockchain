package techgen

import java.util.concurrent.locks.ReentrantLock

import scala.concurrent.Future

trait WithAtomicOperations {

  val lock = new ReentrantLock()

  def atomicFuture[T](f: => Future[T]): Future[T] =
    atomic(f)

  def atomic[T](f: => T): T = {
    lock.lock()
    try f finally lock.unlock()
  }

}
