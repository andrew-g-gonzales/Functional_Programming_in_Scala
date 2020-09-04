package fpinscala.ch3.datastructures.tree

import scala.math


sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree {

  def map[A,B](tree:Tree[A])(f:A=>B):Tree[B] = tree match {
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v) => Leaf(f(v))
  }

  def fold[A,B](tree:Tree[A])(g:(B,B)=>B)(f:(A)=>B):B = tree match {
    case Branch(l,r) => g(fold(l)(g)(f),fold(r)(g)(f))
    case Leaf(b) => f(b)
  }

  def depthViaFold[A](tree:Tree[A]):Int = fold(tree)((a:Int,b:Int) => 1+(a max b))(_=>0)

  def depth[A](tree:Tree[A]):Int = tree match {
    case Branch(left,right) => 1+(depth(left) max depth(right))
    case Leaf(_) => 0
  }

  def maximumViaFold(tree:Tree[Int]):Int = fold(tree)((a:Int,b:Int) => 1+(a max b))(x=>x)

  def maximum(tree:Tree[Int]):Int = tree match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max  maximum(r)
  }

  def size[A](tree:Tree[A]):Int = tree match {
    case Branch(l,r) => 1+(size(l)+size(r))
    case Leaf(_) => 1
  }
}