package solutions

object Chap2 {

  def find[T](ss: Array[T], f: T => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (f(ss(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  /**
    * solution for exercise 2.2
    * 思路: 比较相邻的两个元素是否有序并递归调用
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }

  /**
    * solution for exercise 2.3
    * 把f转换为只有一个参数的部分应用函数
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    f.curried
  }

  /**
    * solution for exercise 2.3
    * 实现反柯里化
    * 思路: 调用Function.uncurried()
    */

  def uncurry[A, B, C](f: A => B => C) : (A, B) => C = {
    Function.uncurried(f)
  }

  /**
    * solution for exercise 2.3
    * 将两个函数组合为一个函数
    * 思路: 1) 用Function1的compose方法
    *      2) g是输入为类型A，输出为类型B的函数；而f的输入参数类型为B
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    A => f(g(A))
  }
}
