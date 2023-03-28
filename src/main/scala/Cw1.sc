import scala.annotation.tailrec

//zad 1
def flatten1[A] (list: List[List[A]]): List[A] = {
  if (list == Nil) Nil
  else list.head ++ flatten1(list.tail)
}

flatten1(List(List(1,2,3), List(4,5)))

//zad 2
def count(e: Int, list: List[Int]): Int = {
  list match {
    case list if (list == Nil) => 0
    case list if list.head == e => count(e, list.tail) + 1
    case _ => count(e, list.tail)
  }
}

//zad 3
def replicate[A](e: A, int: Int): List[A] = {
  if (int <= 0) Nil
  else List(e) ++ replicate(e, int - 1)
}

replicate(1, 3)

//zad 4
def sqrt(list: List[Int]): List[Int] = {
  list.map(a => a*a)
}
sqrt(List(1,2,3,4))
//zad 5
def palindrome(list: List[Int]): Boolean = {
  list.reverse == list
}

//zad 6
def listLenght(list: List[Int]): Int = {
  if (list == Nil) 0
  else listLenght(list.tail) + 1
}