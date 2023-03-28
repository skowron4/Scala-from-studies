import scala.annotation.tailrec

//zad2
def fib(n: Int): Int = {
  if (n <= 1) n
  else fib(n - 1) + fib(n - 2)
}

fib(42)

def fibTail(n: Int): Int = {
  def fibTailRec(n: Int, a: Int, b: Int): Int = {
    if (n == 0) a
    else if (n == 1) b
    else fibTailRec(n - 1, b, a + b)
  }
  fibTailRec(n, 0, 1)
}

fibTail(42)

//zad3

def root3(a: Double): Double = {
  val x0 = a match {
    case a if (a > 1) => a/3
    case _ => a
  }
  val dokladnosc = 1e-13
  def root3Rec(x: Double): Double = {
    if(Math.abs((x * x * x) - a) <= dokladnosc * Math.abs(a)) (x + (a / (x * x) - x) / 3)
    else root3Rec(x + (a / (x * x) - x) / 3)
  }
  root3Rec(x0)
}

root3(2)

//zad4

def zad4A: Int = {
  val _::_::(x: Int)::_ = List(-2, -1, 0, 1, 2)
  x
}

zad4A

def zad4B: Int = {
  val (_, _)::(x, _)::_ = List((1, 2), (0, 1))
  x
}

zad4B

//zad5
def initSegment[A](list1: List[A], list2: List[A]): Boolean = {
  (list1, list2) match {
    case (List(), _) => true
    case (_, List()) => false
    case (head1::tail1, head2::tail2) =>  if (head1 != head2) false
                                          else initSegment(tail1, tail2)
  }
}

val l1 = List(1, 2, 3)
val l2 = List(1, 2, 3, 4)
initSegment(l1, l2)

//zad6
def replaceNth[A](list: List[A], n: Int, e: A): List[A] = {
  list match {
    case Nil => Nil
    case head::tail =>  if (n == 0) e::tail
                        else head::replaceNth(tail, n - 1, e)
  }
}

replaceNth (List('o','l','a', 'm', 'a', 'k', 'o', 't', 'a'), 1, 's')

//zad2.1.a
def fib (n: Int): Int = {
  n match {
    case n if n == 0 => 0
    case n if n == 1 || n == 2 => 1
    case _ => fib(n - 1) + fib(n - 2)
  }
}

//zad2.1.b
def fibTail (n: Int): Int = {
  @tailrec
  def fibTailRec(n: Int, f: Int, s: Int): Int = {
    n match {
      case n if n == 0 => 0
      case n if n == 1 || n == 2 => 1
      case _ => fibTailRec(n - 1, s, f + s)
    }
  }
  fibTailRec(n, 0, 1)
}

//zad 3.1
def root3(a: Double): Double ={
  val x = a match {
    case a if a > 1 => a/3
    case _ => a
  }

  val dokladnosc = 1e-15

  @tailrec
  def root3Rec(x: Double): Double = {
    x match {
      case x if Math.abs(x * x * x - a) <= dokladnosc * Math.abs(a) => x
      case _ =>root3Rec(x + (a / (x * x) - x)/3)
    }
  }
  root3Rec(a)
}

//zad4.1.a
def zad4: Int = {
  val _::_::(x: Int)::_ = List(-2, -1, 0, 1, 2)
  x
}
//zad4.1.b
def zad41: Int = {
  val (_,_)::(x,_)::_ = List((1, 2), (0, 1))
  x
}

//zad 5.1
def initSegment[A](list1: List[A], list2: List[A]): Boolean = {
  (list1, list2) match {
    case (List(), _) => true
    case (hd1::tl1, hd2::tl2) if hd1 == hd2 => initSegment(tl1, tl2)
    case (_::tl, List()) => false
    case (_, _) => false
  }
}

val l1 = List(1, 2, 3)
val l2 = List(1, 2, 3, 4)
initSegment(l1, l2)

//zad6
def replaceNth[A](list: List[A], nr: Int, e: A): List[A] = {
  (list, nr) match {
    case (hd::tl, nr) if nr > 0 => hd::replaceNth(tl, nr - 1, e)
    case (_::tl, nr) if nr == 0 => e::tl
  }
}

replaceNth (List('o','l','a', 'm', 'a', 'k', 'o', 't', 'a'), 1, 's')