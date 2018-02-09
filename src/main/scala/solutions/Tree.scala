package solutions

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
    * 练习3.25 统计树里面的节点数
    * @param root 根节点
    * @return 节点数
    */
  def size[A](root: Tree[A]): Int = {
    root match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1
      case _ => 0
    }
  }

  /**
    * 练习3.26 找到树的最大元素  x.max(y) 计算x,y的最大值
    * @param root 根节点
    * @return 节点数
    */
  def maxium(root: Tree[Int]): Int = {
    root match {
      case Leaf(x) => x
      case Branch(l, r) => maxium(l).max(maxium(r))
    }
  }

  
  /**
    * 练习3.27 找到树的最大深度
    * @param root 根节点
    * @return 节点数
    */
  def depth(root: Tree[Int]): Int = {
    root match {
      case Leaf(x) => 1
      case Branch(l, r) => depth(l).max(depth(r)) + 1
      case _ => 0
    }
  }
}