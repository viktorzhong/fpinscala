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

  /**
    * 练习3.4 实现函数 drop
    * 思路：模式匹配 递归调用drop
    */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n - 1)
    }
  }

  /**
    * 练习3.5 实现函数 dropWhile 删除所有符合预判f(A)的元素
    * 思路：模式匹配 递归调用dropWhile 并在=>前增加if判断
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if (f(h)) => dropWhile(t, f)
      case _ => l
    }
  }

  /**
    * 练习3.6 实现函数 init 返回除最后一个元素的列表
    * 思路：模式匹配
    */
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

}