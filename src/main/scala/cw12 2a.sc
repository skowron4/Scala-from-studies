import scala.concurrent.Future

def pairFut[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] = {
  fut1.zip(fut2)
}