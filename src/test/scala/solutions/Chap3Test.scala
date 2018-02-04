package solutions

import org.scalatest.FunSuite
import solutions.List._

class Chap3Test extends FunSuite {

  test("test tail function") {
    val _list = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val _tail = Cons(2, Cons(3, Cons(4, Nil)))

    assert(_tail === tail(_list))
  }

  test("test setHead function") {

    val _list = Cons(2, Cons(3, Cons(4, Nil)))
    val _new = Cons(1, Cons(3, Cons(4, Nil)))

    assert(_new === setHead(1, _list))
  }

  test("test drop") {
    val _list = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val _tail = Cons(3, Cons(4, Nil))

    assert(_tail === drop(_list, 2))
  }

  test("test dropWhile") {
    val _list = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val _tail = Cons(3, Cons(4, Nil))

    assert(_tail === dropWhile(_list, (x:Int) => x <= 2))
  }

  test("test init") {
    val _list = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val _head = Cons(1, Cons(2, Cons(3, Nil)))

    assert(_head === init(_list))
  }

  test("test foldLeft sum") {
    val _list = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    assert(10 === foldLeft(_list, 0)((x,y) => x + y))
  }

  test("test foldLeft product") {
    val _list = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    assert(24 === foldLeft(_list, 1)((x,y) => x * y))
  }
 }
