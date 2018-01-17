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
  def curry[A,B,C](f:(A, B) => C) : A => (B => C) = {
      (f _).curried
  }

  /**
    * solution for exercise 2.3
    * 实现反科里化
    * 思路: 调用Function.uncurried()
    */
  def uncurry[A, B, C](f: A => B => C) : (A, B) => C = {
    Function.uncurried(f)
  }

}
