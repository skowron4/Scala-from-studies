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

object ProdCons{
  def main(): Unit = { //main(args: Array[String])
    val buf: ArrayBlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
    //zad 1.a
    new Producer("Producer", buf).start()
    new Consumer("Consumer", buf).start()

    println("Koniec")
  }
}

ProdCons.main()