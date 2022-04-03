package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (l.depth max r.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(x) => f(x)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))
  
  def sizeViaFold: Int = this.fold(_ => 1, _ + _)
  
  def depthViaFold: Int = this.fold(_ => 0, (d1, d2) => 1 + (d1 max d2))
  
  def mapViaFold[B](f: A => B): Tree[B] = this.fold(leaf => Leaf(f(leaf)), 
    (l, r) => Branch(l, r))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Option[Int] = 
    t match
      case Leaf(x)  => 
        if x > 0 then Some(x ) 
        else None
      case Branch(l, r) => l.firstPositive orElse r.firstPositive

  extension (t: Tree[Int]) def maximum: Int = 
    t match
      case Leaf(x) => x
      case Branch(l, r) => l.maximum max r.maximum

  extension (t: Tree[Int]) def maximumViaFold: Int = 
    t.fold(x => x, (l, r) => l max r)
