import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

def pairFut[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] = {
  for {
    x <- fut1
    y <- fut2
  }yield (x, y)
}
val fut1 = Future { 1 + 2 }
val fut2 = Future { "Hello" + " World" }

val combinedFuture = pairFut(fut1, fut2)

combinedFuture onComplete {
  case Success(result) => println(s"Result: $result")
  case Failure(exception) => println(s"Error: $exception")
}