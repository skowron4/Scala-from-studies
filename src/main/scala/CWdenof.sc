val x = false
val y = true

def or(x: Any, y: Any): Boolean = {
  x match {
    case y => true
    case _ => false
  }
}

or(x, y)