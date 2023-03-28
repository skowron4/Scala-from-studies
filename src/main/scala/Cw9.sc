//zad 1
class Time1(var hour: Int) {
  if (hour < 0) hour = 0
}

object Time1 {
  def apply(hour: Int) = new Time1(hour)
}

//zad 2.a
class Time(var h: Int, var m: Int) {
  require(h >= 0 && h <= 23)
  require(m >= 0 && m <= 59)

  def hour: Int = h
  def hour_=(newH: Int):Unit = {
    if (newH < 0 || newH > 23) throw new IllegalArgumentException
    h = newH
  }

  def minute: Int = h

  def minute_=(newM: Int):Unit = {
    if (newM < 0 || newM > 59) throw new IllegalArgumentException
    m = newM
  }

  def before(other: Time): Boolean = {
//    other match {
//      case other.h > this.h => true
//      case other.h == this.h && other.m > this.m => true
//      case _ => false
//    }
    if (other.h > this.h) true
    else if (other.h == this.h && other.m > this.m) true
    else false
  }
}

//zad 2.b
class Time2(var h: Int, var m: Int) {
  require(h >= 0 && h <= 23)
  require(m >= 0 && m <= 59)

  var mpmn = 60 * h + m

  def hour: Int = mpmn / 60

  def hour_=(newH: Int):Unit = {
    if (newH < 0 || newH > 23) throw new IllegalArgumentException
    mpmn = mpmn % 60 + newH * 60
  }

  def minute: Int = mpmn % 60

  def minute_=(newM: Int):Unit = {
    if (newM < 0 || newM > 59) throw new IllegalArgumentException
    mpmn = (60 / 60) * 60 + newM
  }

  def before(other: Time2): Boolean = {
    other.mpmn > this.mpmn
  }
}

//zad 3
class Pojazd(val producer: String, val model: String, year: Int = -1, var rg: String = "") {
  override def toString: String = producer  + " " + model + " " + year + " " + rg
}

println("" + new Pojazd("A", "B"))
println(new Pojazd("A", "B", 2016))
println(new Pojazd("A", "B", 2016, "XYZ"))
println(new Pojazd("A", "B",  rg = "XYZ"))

//zad 4
// Demonstracja wykorzystania metod getMessage i printStackTrace,
// dziedziczonych przez wszystkie klasy wyjatkow.

//def metoda3() = {
//  throw new Exception( "Wyjatek zgloszony w metoda3" )
//}
//def metoda2() = {
//  metoda3()
//}
//def metoda1() = {
//  metoda2()
//}
//
//try {
//  metoda1
//} catch {
//  case e : Throwable => println(e.getMessage)
//}

//zad.1.1
class Time11(var hour: Int){
  if (hour < 0) hour = 0
}

object Time11 {
  def apply(hour: Int) = new Time1(hour)
}

//zad 2.1.a
class Time21a(var h: Int, var m: Int){
  require(h >= 0 && h < 24)
  require(m >= 0 && m < 60)

  def hour: Int = hour
  def hour_=(newH: Int): Unit = {
    if (newH < 0 || newH > 23) throw new IllegalArgumentException("illegal argument")
    h = newH
  }

  def minute: Int = minute
  def minute_=(newM: Int): Unit = {
    if (newM < 0 || newM > 59) throw new IllegalArgumentException("illegal argument exception")
    m = newM
  }

  def before(other: Time21a): Boolean = {
    (other.h, other.m) match {
      case (oh, _) if oh > h => true
      case (_, om) if om > m => true
      case (_, _) => false
    }
  }
}

//zad 3.1
class Pojazd1(val producent: String, val model: String, val rokProdukcji: Int = -1, var nrRejestracyjny: String = ""){
  override def toString: String = producent + " " + model + " " + rokProdukcji + " " + nrRejestracyjny + " "
}