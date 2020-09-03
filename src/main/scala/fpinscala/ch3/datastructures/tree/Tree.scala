package fpinscala.ch3.datastructures.tree

import scala.math


sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree {

  def depth[A](tree:Tree[A]):Int = tree match {
    case Branch(left,right) => 1+(depth(left) max depth(right))
    case Leaf(_) => 0
  }

  def maximum(tree:Tree[Int]):Int = tree match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max  maximum(r)
  }

  def size[A](tree:Tree[A]):Int = tree match {
    case Branch(l,r) => 1+(size(l)+size(r))
    case Leaf(_) => 1
  }
}