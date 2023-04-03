import scala.annotation.tailrec

// zad1
def irepeat[A](k: Int, lazyList: LazyList[A]): LazyList[A] = {
  lazyList.foldLeft(LazyList.empty[A])( (s, i) => LazyList.from(1).take(k).map(_ => i).appended(s))
}

irepeat(3, LazyList('a','b','c','d')).toList
irepeat(3, LazyList.from(1).take(10)).toList
irepeat(3, LazyList().take(15)).toList

// zad2
def lfib = {
  def lfib_rec(a: Int, b: Int): LazyList[Int] = {
    a#::lfib_rec(b, b + a)
  }
  lfib_rec(0, 1)
}

lfib.take(10).toList

//zad3

sealed trait BT[+A]
  case object LEmpty extends BT[Nothing]
  case class Node[+A](elem: A, left: () => BT[A], right: () => BT[A]) extends BT[A]

//b)
def lTree(n: Int): BT[Int] = {
  Node(n, () => lTree(2 * n), () => lTree(2 * n + 1))
}

//a)
def lBreadth(tree: BT[Int]): LazyList[Int] = {
  def lBreadthRec(queue: List[BT[Int]]): LazyList[Int] = {
    queue match {
      case Nil => LazyList()
      case LEmpty::tl => lBreadthRec(tl)
      case Node(v, l, r)::tl => v#::lBreadthRec(tl:::List(l(), r()))
    }
  }
  lBreadthRec(List(tree))
}

lBreadth(lTree(1)).take(20).toList

//zad 1
def lrepeat(k: Int, llist: LazyList[Int]):LazyList[Int] = {
  def lrepeat(n: Int, lazylist: LazyList[Int]): LazyList[Int] = {
    (n, llist) match {
      case (_, LazyList()) => LazyList()
      case (0, hd#::tl) => lrepeat(k, tl)
      case (_, hd#::tl) => hd#::lrepeat(n - 1, tl)
    }
  }
  lrepeat(k, llist)
}

//zad 2.1.a
def lFib(): LazyList[Int] = {
  @tailrec
  def lFibRec(f: Int, s: Int, acc: LazyList[Int]): LazyList[Int] = {
    lFibRec(s, f + s, (f + s)#::acc)
  }
  lFibRec(0, 1, LazyList())
}
//zad 3.1.a
def lBreadth(ltree: BT[Int]): LazyList[Int] = {
  def lBreadthRec(queue: List[BT[Int]]):LazyList[Int] = {
    queue match {
      case List() => LazyList()
      case Node(v, l, r)::tl => v#::lBreadthRec(tl:::List(l(), r()))
      case LEmpty::tl => lBreadthRec(tl)
    }
  }
  lBreadthRec(List(ltree))
}

//zad 3.1.b
def lTree(n: Int):BT[Int] ={
  Node(n, () => lTree(2 * n), () => lTree(2 * n + 1))
}