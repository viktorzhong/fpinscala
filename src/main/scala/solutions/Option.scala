package solutions

// 隐藏系统库中的Option Some Either
import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A] {
  // 练习4.1 实现Option中的接口
  // 思路: Option有两个子类 已被定义的情况返回Some, 未定义返回None
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  // B >: A 表示B是A类型或者A类型的父类型
  // default: => B 表示default类型为B 但不是立即求值
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this.map(Some(_)).getOrElse(ob)
  }

}

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // 4.2 根据flatMap实现xs的方差
  def variance(xs: Seq[Double]): Option[Double] = {

    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  // 接受一个非严格求值的A参数, 如果产生异常则返回None
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case e: Exception => None
    }
  }

  // 练习4.3 使用二元函数组合两个option
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(x => b.map(y => f(x, y)))
  }


}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]