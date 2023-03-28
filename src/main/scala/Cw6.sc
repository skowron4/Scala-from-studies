import java.beans.Expression
import scala.annotation.tailrec

//zad1
@tailrec
def whileLoop(condition: => Boolean) (expression: => Unit): Unit = {
  if (condition) {
    expression
    whileLoop(condition) (expression)
  }
}

var i = 0
whileLoop(i < 3){
  i = i + 1
  println(i)
}

//zad2

//a) swap:
def swap(tab: Array[Int], i: Int, j: Int): Unit = {
  val tmp = tab(i)
  tab(i) = tab (j)
  tab(j) = tmp
}

//b) partition
def partition(tab: Array[Int], l: Int, r: Int): (Int, Int) = {
  var i = l
  var j = r
  val pivot = tab((l + r) / 2)
  while (i <= j){
    while (tab(i) < pivot){
      i += 1
    }
    while (pivot < tab(j)){
      j -= 1
    }
    if (i <= j) {
      swap(tab, i, j)
      i += 1
      j -= 1
    }
  }
  (i, j)
}

def quick(tab: Array[Int], l: Int, r: Int): Unit = {
  if (l < r) {
    val (i, j) = partition(tab, l, r)
      quick(tab, l, j)
      quick(tab, i, r)
  } else ()
}

def quickSort(tab: Array[Int]): Unit = {
  quick(tab, 0, tab.length - 1)
}

var t1 = Array(4, 8, 1, 12, 7, 3, 1, 9)
quickSort(t1)
println(t1.mkString("Array(", ", ", ")"))

//zad 1.1
@tailrec
def whileLoop(condition: => Boolean)(expression: => Unit):Unit = {
  if (condition) {
    expression
    whileLoop (condition) (expression)
  }
}

var i = 0
whileLoop(i < 3){
  i = i + 1
  println(i)
}

//zad 3
def swap1(tab: Array[Int], i: Int, j: Int): Unit = {
  val tmp = tab(i)
  tab(i) = tab(j)
  tab(j) = tmp
}

def partition1(tab: Array[Int], l: Int, r: Int): (Int, Int) = {
  var i = l
  var j = r
  val p = tab((l + r) / 2)
  while (i <= j) {
    while (tab(i) < p){
      i += 1
    }
    while (tab(j) > p){
      j -= 1
    }
    if (i <= j){
      swap1(tab, i, j)
      i += 1
      j -= 1
    }
  }
  (i, j)
}

def quick1(tab: Array[Int], l: Int, r: Int):Unit = {
  if (l < r){
    val (i, j) = partition1(tab, l, r)
    quick1(tab, l, j)
    quick1(tab, i, r)
  } else ()
}

def quicksort1(tab: Array[Int]):Unit = {
  quick1(tab, 0, tab.length - 1)
}

var t1 = Array(4, 8, 1, 12, 7, 3, 1, 9)
quicksort1(t1)
println(t1.mkString("Array(", ", ", ")"))