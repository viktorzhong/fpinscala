package solutions

import org.scalatest.FunSuite
import solutions.List.{tail,setHead}

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
}
