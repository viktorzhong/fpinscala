package solutions

import List._

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

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  /**
    * 练习3.9 用foldRight实现List长度
    */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, t) => t + 1)
  }


}
