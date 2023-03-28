import scala.annotation.tailrec

//zad 2
def f[A](x:A) = throw new Exception("ex")

//zad 3
sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

val tt = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Empty))

def breadthSearch[A](tree: BT[A]): List[A] = {
  @tailrec
  def breadthInn(queue: List[BT[A]])(acc: List[A]):List[A] ={
    queue match {
      case Nil => acc
      case hd::tl =>
        hd match {
          case Empty => breadthInn(tl)(acc)
          case Node(v,n1,n2) => breadthInn(tl:::List(n1):::List(n2))(v::acc)
        }
    }
  }
  breadthInn(List(tree))(Nil).reverse
}

breadthSearch(tt)

//zad4a
def dlugoscWew[A](tree: BT[A]): Int = {
  def dlugoscWewInn(node: BT[A])(acc: Int): Int = {
    node match {
      case Empty => 0
      case Node(_,n1,n2) => acc + dlugoscWewInn(n1)(acc+1) + dlugoscWewInn(n2)(acc+1)
    }
  }
  dlugoscWewInn(tree)(0)
}

dlugoscWew(tt)

//zad4b
def dlugoscZew[A](tree: BT[A]): Int = {
  def dlugoscWewInn(node: BT[A])(acc: Int): Int = {
    node match {
      case Empty => acc
      case Node(_,n1,n2) => dlugoscWewInn(n1)(acc+1) + dlugoscWewInn(n2)(acc+1)
    }
  }
  dlugoscWewInn(tree)(0)
}

dlugoscZew(tt)

//zad5
sealed trait Graphs[A]
case class Graph[A](succ: A=>List[A]) extends Graphs[A]

val g = Graph((i: Int) =>
  i match {
    case 0 => List(3)
    case 1 => List(0, 2, 4)
    case 2 => List(1)
    case 3 => Nil
    case 4 => List(0, 2)
    case n => throw new Exception(s"Graph g: node $n doesn't exist")
  })

def depthSearch[A](g: Graph[A])(startNode: A): List[A] = {
  def depthSearchInn(visited: List[A], node: A): List[A] = {
    if (visited.contains(node)) visited
    else (g succ node).foldLeft(node::visited)(depthSearchInn)
  }
  depthSearchInn(Nil, startNode).reverse
}

depthSearch(g)(4)

/*def depthSearch[A](g: Graph[A])(startNode: A):List[A] = {
  @tailrec
  def depthSearchInn(visited: List[A])(queue: List[A])(acc: List[A]):List[A] = {
    queue match {
      case Nil => acc
      case hd::tl =>
        if(visited.contains(hd)) depthSearchInn(visited)(tl)(acc)
        else depthSearchInn(hd::visited)((g succ hd):::tl)(hd::acc)
    }
  }
  depthSearchInn(Nil)(List(startNode))(Nil).reverse
}

depthSearch(g)(4)*/

//zad 3.1
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

val tt = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Empty))
def breadthBT[A](t: BT[A]): List[A] = {
  @tailrec
  def breadthBTRec[A](q: List[BT[A]], visited: List[A]): List[A] = {
    q match {
      case Nil => visited.reverse
      case hd::tl => hd match {
        case Empty => breadthBTRec(tl, visited)
        case Node(v, l, r) => breadthBTRec(tl ++ List(l, r), v::visited)
      }
    }
  }
  breadthBTRec(List(t), List())
}

//zad 4.1.a
def depthInner(t: BT[Int]):Int = {
  def depthInnerRec(n: BT[Int], acc: Int): Int = {
    n match {
      case Empty => acc
      case Node(v, l, r) => depthInnerRec(l, acc + 1) + depthInnerRec(r, acc + 1)
    }
  }
  depthInnerRec(t, 0)
}

//zad 4.1.b
def depthOuter(t: BT[Int]): Int = {
  def depthOuterRec(n: BT[Int], acc: Int): Int = {
    n match {
      case Empty => acc
      case Node(v, l, r) => depthOuterRec(l, acc + 1) + depthOuterRec(r, acc + 1)
    }
  }
  depthOuterRec(t, 0)
}

//zad 5.1
def depthSearch(g: Graph[Int], n: Int): List[Int] = {
  def depthSearchRec(v: List[Int], n: Int): List[Int] = {
    v match {
      case v if v.contains(n) => v
      case _ => (g succ n).foldLeft(n::v)(depthSearchRec)
    }
  }
  depthSearchRec(List(n), n)
}