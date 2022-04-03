package fpinscala.exercises.datastructures

import scala.annotation.tailrec
/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val x = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]) =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]) =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = 
    l match 
      case Nil         => sys.error("tail of empty list")
      case Cons(_, xs) => xs

  def setHead[A](l: List[A], h: A): List[A] = 
    l match 
      case Nil          =>sys.error("set head of empty list")
      case Cons(_, xs)   => Cons(h, xs)

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =  
    l match 
      case Nil                  => Nil
      case Cons(_, _) if n == 0 => l
      case Cons(x, xs)          => drop(xs, n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match
      case Nil => Nil
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case Cons(_, _ ) => l

  @annotation.tailrec
  def init[A](l: List[A]): List[A] = 
    l match
      case Nil => sys.error("List is empty")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))

    // @annotation.tailrec
    // def go(initList: List[A], inputList: List[A]): List[A] = 
    //   inputList match
    //     case Nil => sys.error("List is empty")
    //     case Cons(_, Nil) => initList
    //     case Cons(x, xs) => go(Cons(x, initList), xs)

    // go(Nil, l)

  def length[A](l: List[A]): Int = 
    foldLeft(l, 0, (acc, _) => acc + 1 )

  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = 
    l match 
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)


  def sumViaFoldLeft(ns: List[Int]) = foldLeft(ns, 0, _ + _) 

  def productViaFoldLeft(ns: List[Double]) = foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (acc, _) => acc + 1 )

  // List(1,2,3) 
  // acc, nil => acc
  // nil, 1 => cons(1, acc)
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A], (acc, x) => Cons(x, acc))

  def foldRightViaFoldLeft[A, B](as: List[A], acc: B, f: (A, B)=> B): B = 
    // foldLeft(reverse(as), acc, (b, a) => f(a, b))
    foldLeft(as, (b: B) => b, (g, a) => b => g(f(a, b)))(acc)

  def foldLeftViaFoldRight[A, B](as: List[A], acc: B, f: (B, A) => B): B = 
    foldRight(as, (b: B) => b, (a, g) => b => g(f(b, a))) (acc)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = 
    foldRight(l, r, Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = 
    foldLeft(l, Nil: List[A], (acc, x) => appendViaFoldRight(acc, x))

  def incrementEach(l: List[Int]): List[Int] = 
    foldRight(l, Nil: List[Int], (x, acc) => Cons(x+1, acc))

  def doubleToString(l: List[Double]): List[String] = 
    foldRight(l, Nil: List[String], (x, acc) => Cons(x.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRight(l, Nil: List[B], (x, acc) => Cons(f(x), acc) )

  def filter[A](as: List[A])(f: A => Boolean): List[A] = 
    foldRight(as, Nil: List[A], (x, acc) =>{
      if f(x) then Cons(x, acc)
      else acc
      })

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
    concat(map(as)(f))
    // foldRight(as, Nil: List[B], (x, acc) => append(f(x), acc))

  def filterUsingFlatmap[A](l: List[A])(f:  A => Boolean): List[A] = 
    flatMap(l) {x =>
        if (f(x)) then Cons(x, Nil)
        else Nil
      }

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = 
    (a, b) match 
      case(Nil, _) => Nil
      case(_, Nil) => Nil
      case(Cons(x, xs), Cons(y, ys)) => Cons (x + y, addPairwise(xs, ys) ) 

  // def zipWith - TODO determine signature
  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    (a, b) match 
      case(Nil, _) => Nil
      case(_, Nil) => Nil
      case(Cons(x, xs), Cons(y, ys)) => Cons (f(x, y), zipWith(xs, ys, f) ) 

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = 
    def startsWith[A](sup: List[A], sub: List[A]): Boolean =
      (sup, sub) match 
        case (_, Nil) => true
        case (Cons(x, xs), Cons(y, ys)) if x == y => startsWith(xs, ys)
        case (Nil, _) => false
      

    sup match 
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(x, xs) => hasSubsequence(xs, sub)

