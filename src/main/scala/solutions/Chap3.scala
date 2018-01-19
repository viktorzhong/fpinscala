package solutions

import List.sum

object Chap3 extends App {

  /**
    * exercise 3.1
    * 会匹配到第三行 对应x = 1 y = 2
    */
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
}
