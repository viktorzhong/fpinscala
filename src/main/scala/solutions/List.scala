package solutions

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  /**
    * 练习3.2 实现tail函数
    * 思路：用模式匹配
    */
  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(_, t) => t
    }
  }

  /**
    * 练习3.3 实现函数 setHead
    * 思路：用模式匹配
    */
  def setHead[A](head: A, list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(_, t) => Cons(head, t)
    }
  }
}