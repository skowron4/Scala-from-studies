import java.util.concurrent.{ArrayBlockingQueue, ForkJoinPool}
import scala.concurrent.ExecutionContext

object ProdCons{
  def main(): Unit = {
    val buf: ArrayBlockingQueue[Int] = new ArrayBlockingQueue[Int](5);
    val executor = new ForkJoinPool()
    implicit val ec = ExecutionContext.fromExecutor(executor)

    for (i <- 1 to 2) {
      ec.execute(() => {
        for (j <- 1 to 10) {
          println(s"Producer $i producing $j")
          buf.put(j)
        }
      })
    }

    for(i <- 1 to 3){
      ec.execute(() => {
        for (j <- 1 to 10){
          println(s"Consumer $i consumed ${buf.take()}")
        }
      })
    }
  }
}

ProdCons.main()

// program kończy się, ponieważ każdy konsument kończy swoją pracę po 10 pobranych
// elementach, a każdy producent kończy swoją pracę po wyprodukowaniu 10 elementów .
// Wszystkie wątki są zakończone i nie ma już nic do wykonania.