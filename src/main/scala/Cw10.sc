import scala.collection.mutable
//class Mutable[-T] (private[this] var x : T) {
//}

// 1.a Mutator error

// 1.b
// var -> val
// +T -> T
// private[this]

// 1.c
// Accesor error

//val s = new Mutable();

//zad 2
//abstract class Sequence[+A] {
//  def append[B >: A](x: Sequence[B]): Sequence[B]
//}

//zad 3
class EmptyQueueException(msg: String) extends Exception(msg)

class SQueue[+T]private(private val queue:(List[T], List[T])){
  def enqueue[S >: T](el: S): SQueue[S] = {
    val (s, e) = queue
    normalize(s, el::e)
  }

  private def normalize[S >: T](list1: List[S], list2: List[S]):SQueue[S] = {
    (list1, list2) match {
      case (Nil, end) => new SQueue(end.reverse, List())
      case queue => new SQueue(queue)
    }
  }

  def dequeue: SQueue[T] = {
    queue match {
      case (_::tail, end) => normalize(tail, end)
      case _ => normalize(List(), List())
    }
  }

  def first: T = {
    if (isEmpty) throw new EmptyQueueException("SQueue is empty")
    else queue._1.head
  }

  def isEmpty: Boolean = {
    queue._1 == Nil
  }
}

//zad 4
def nibba[T](x: mutable.Seq[T], src: mutable.Seq[T]): Unit = {
  require(x.length >= src.length)
  var i = 0;
  src.foreach(element => x.update(i, element))
  i += 1
}