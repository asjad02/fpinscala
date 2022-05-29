package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = 
    this match
      case Some(x) => Some(f(x))
      case None => None

  def getOrElse[B>:A](default: => B): B = 
    this match 
      case Some(x) => x
      case None => default

  def flatMap[B](f: A => Option[B]): Option[B] = 
    map(x => f(x)).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = 
    map(x => Some(x)).getOrElse(ob)
    // Some(this).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = 
    flatMap{ x => 
      if f(x) then Some(x)
      else None
    }

object Option:

  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch case e: Exception => 43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

    // If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) 
    // for each element x in the sequence.
    // ---->>
    // calculate m -> new list of pow(x-m, 2) -> mean on new list 
  def variance(xs: Seq[Double]): Option[Double] = 
    val meanCalculated: Option[Double] = mean(xs)
    meanCalculated.flatMap { m =>
      mean(xs.map { x =>
        math.pow(x - m, 2)
      })
    }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    for 
      x <- a
      y <- b 
    yield f(x, y)

    // a.flatMap { x=>
    //   b.map { y =>
    //     f(x,y)
    //   }
    // }

    // (a, b) match
    //   case (Some(x), Some(y)) => Some(f(x, y))
    //   case (_, _) => None


    // turn option to outer edge also 
    // if any elem None it should be None not Option 
  def sequence[A](as: List[Option[A]]): Option[List[A]] = 
    as.foldRight(Some(Nil): Option[List[A]]) ((elem, acc) => map2(elem, acc)( _ :: _))

      // List(None, op2, None, op3)
      // the current impl doesnt need extra bit orElse(None) =>
      //  it will never consume None will short-circuit if found

    // as.foldRight(None: Option[List[A]]) {(optA, accOptList) =>
    //     optA.flatMap { x =>
    //       accOptList.map { y =>  x :: y }  
    //     }.orElse(None)
    // }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = 
    as.foldRight(Some(Nil): Option[List[B]]) { (elem, accOptList) =>
      for
        acc <- accOptList 
        e <- f(elem)
      yield (e :: acc)
      // map2(f(elem), accOptList)(_::_)
    }

  def sequenceUsingTraverse[A](as: List[Option[A]]): Option[List[A]] = 
    // A = Option[A] 
    // but A => Option[B]       ||    Option[     A] 
    // returns  Option[List[B]] ||    Option[List[A]]
    traverse(as)(a => (a))

