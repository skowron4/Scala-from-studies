import scala.annotation.tailrec

// zad 2
def curry3[A, B, C, D](f: A => B => C => D) (x: A, y: B, z: C): D = {
  f(x) (y) (z)
}

def uncurry3[A, B, C, D](f: (A, B, C) => D, x: A, y: B, z: C): D = {
  f(x, y, z)
}

def plus (x: Int) (y: Int) (z: Int): Int = {
  x + y + z
}

def add (x: Int, y: Int, z: Int): Int = {
  x + y + z
}

curry3(plus) (3, 4, 5)
uncurry3(add, 3, 4, 5)

// zad 3.1
def sumProd1 (list: List[Int]): (Int, Int) = {
  list.foldLeft((0, 1)) ((acc, head) => (acc._1 + head, acc._2 * head))
}
sumProd1(List(1, 2, 3, 4, 5, 6))

//zad 3.2
def sumProd2 (list: List[Int]): (Int, Int) = {
  list.foldLeft((0, 1)) ((acc, head) => acc match {
    case _ => (acc._1 + head, acc._2 * head)
  })
}
sumProd2(List(1, 2, 3, 4, 5, 6))

// zad 4 a
def quicksort(list: List[Int]): List[Int] = {
  list match {
    case Nil => Nil
    case _::Nil => list
    case _ =>
      val small = list.filter(elem => elem < list.head)
      val large = list.filter(elem => elem >= list.head)
      quicksort(small):::quicksort(large)
  }
}
// nieskończona rekurencja

// zad 4 b
def quicksort2(list: List[Int]): List[Int] = {
  list match {
    case Nil => Nil
    case _::Nil => list
    case head::tail =>
      val small = tail.filter(elem => elem < head)
      val large = tail.filter(elem => elem > head)
      quicksort2(small):::(head::quicksort2(large))
  }
}
//nie porownujemy dwoch gowien

// zad 5.1.a
def insertion[A](f: A => A => Boolean, list: List[A], el: A): List[A] = {
  list match {
    case Nil => List(el)
    case head::tail if f (head) (el) => head::insertion(f, tail, el)
    case _ => el::list
  }
}

def insertionsort[A](f: A => A => Boolean, list: List[A]): List[A] = {
  list.foldLeft(Nil:List[A]) ((acc, head) => insertion(f, acc, head))
}

//insertionsort(x => y => x > y, List(3,2,4,5,1,2,2,7,6))
//insertionsort(x => y => x < y, List(3,2,4,5,1,2,2,7,6))

//zad 5.1.b

def insertsort[A](f: A => A => Boolean, list: List[A]): List[A] = {
  list match {
    case Nil => Nil
    case head::tail => insertion(f, insertsort(f, tail), head)
  }
}

insertsort(x => y => x > y, List(3,2,4,5,1,2,2,7,6))
insertsort(x => y => x < y, List(3,2,4,5,1,2,2,7,6))

// zad 5.2.

def merge[A](f: A => A => Boolean, list1: List[A], list2: List[A]): List[A] = {
  (list1, list2) match {
    case (_, Nil) => list1
    case (Nil, _) => list2
    case (head1::tail1, head2::tail2) =>
      f(head1) (head2) match {
        case true => head1::merge(f, tail1, list2)
        case _ => head2::merge(f, list1, tail2)
    }
  }
}

//def halfList(list: List[Int]): (List[Int], List[Int]) = {
//  list match {
//    case Nil => (Nil, Nil)
//    case _::Nil => (list, Nil)
//    case first::second::tail =>
//      val (list1, list2) = halfList(tail)
//      (first::list1, second::list2)
//  }
//}

def halfList[A](list: List[A]): (List[A], List[A]) = {
  @tailrec
  def halfListRec[A](list: List[A], list1: List[A], list2: List[A]):  (List[A], List[A]) = {
    list match {
      case Nil => (list1, list2)
      case head::tail => halfListRec(tail, head::list2, list1)
    }
  }
  halfListRec(list, Nil, Nil)
}

def mergesort[A](f: A => A => Boolean, list: List[A]): List[A] = {
  list match {
    case Nil => Nil
    case _::Nil => list
    case _ =>
      val (list1, list2) = halfList(list)
      merge(f, mergesort(f, list1), mergesort(f, list2))
  }
}

//mergesort(x=>y=> x>y, List(3,2,4,5,1,2,2,7,6))
//mergesort(x=>y=> x<y, List(3,2,4,5,1,2,2,7,6))


//zad 2.1
def curry3[A, B, C, D](f: A => B => C => D)(x: A, y: B, z: C): D = {
  f(x)(y)(z)
}
def uncurry[A, B, C, D](f: (A, B, C) => D, x: A, y: B, z: C):D = {
  f(x, y, z)
}

//zad 3.1
def sumProd(list: List[Int]): (Int, Int) ={
  list.foldLeft((0, 1))((acc, head) => (acc._1 + head, head * acc._2))
}

//zad 5.1.a
def insertion(f: (Int, Int) => Boolean, list: List[Int], el: Int): List[Int] = {
  list match {
    case Nil => List(el)
    case hd::tl if f(hd, el) => hd::insertion(f, tl, el)
    case _ => el::list
  }
}

def insertsort(list: List[Int], f: Int => Int => Boolean): List[Int] = {
  list.foldLeft(Nil:List[Int])((acc, head) => insertion(f, acc, head))
}

def insersort(list: List[Int], f: Int => Int => Boolean): List[Int] = {
  list match {
    case Nil => Nil
    case hd::tl => insertion(f, insertionsort(f, tl), hd)
  }
}

//zad 5.1.b
def split(list: List[Int]):(List[Int], List[Int]) = {
  @tailrec
  def splitRec(l: List[Int], l1: List[Int], l2: List[Int]):(List[Int], List[Int]) = {
    list match {
      case Nil => (l1, l2)
      case hd::tl => splitRec(tl, hd::l2, l1)
    }
  }
}

def mergesort_inner(f: (Int, Int) => Boolean, l1: List[Int], l2: List[Int]): List[Int] = {
  (l1, l2) match {
    case (Nil, _) => l2
    case (_, Nil) => l1
    case (hd1::tl1, hd2::tl2) =>
      f(hd1, hd2) match {
        case true => hd1::mergesort_inner(f, tl1, l2)
        case _ => hd2::mergesort_inner(f, l1, tl2)
      }
  }
}

def mergesort1(f: (Int, Int) => Boolean, l: List[Int]): List[Int] = {
  l match {
    case Nil => Nil
    case hd::Nil => List(hd)
    case _ => val (l1, l2) = split(l)
    mergesort_inner(f, mergesort1(f, l1), mergesort1(f, l2))
  }
}