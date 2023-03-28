import java.util.concurrent.ArrayBlockingQueue

class Producer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name){
  override def run(): Unit =
    for (i <- 1 to 10) {println(s"$getName producing $i")
      buf.put(i)}
}

class Consumer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name){
  override def run(): Unit =
    for (i <- 1 to 10) println(s"$getName consumed ${buf.take()}")
}

object ProdCons {
  def main(): Unit = {
    val buf: ArrayBlockingQueue[Int] = new ArrayBlockingQueue[Int](5)

    //    zad 1.b.1
    for (i <- 1 to 2) {
      new Producer(s"Producer $i", buf).start
      new Consumer(s"Consumer $i", buf).start
    }
    new Producer(s"Producer3", buf).start
    //    zad 1.b.2
//    new Producer("Producer1", buf).start()
//    new Producer("Producer2", buf).start()
//
//    new Consumer("Consumer1", buf).start()
//    new Consumer("Consumer2", buf).start()
//    new Consumer("Consumer3", buf).start()

  }
}
ProdCons.main()

// Program się nie kończy, ponieważ w pętli tworzone są dwa producenty i dwa
// konsumenty, ale bufor jest ograniczony do rozmiaru 5. Producenty produkują 10
// elementów, ale konsumenci nie mogą ich wszystkich zużyć, ponieważ bufor jest pełny.

