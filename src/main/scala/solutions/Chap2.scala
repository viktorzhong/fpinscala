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


}
