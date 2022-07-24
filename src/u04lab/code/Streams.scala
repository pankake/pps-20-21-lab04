package u04lab.code

import scala.util.Random

object Streams extends App {
  import Lists._
  sealed trait Stream[A]

  object Stream {
    case class Empty[A]() extends Stream[A]
    case class Cons[A](head: () => A, tail: () => Stream[A]) extends Stream[A]

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def toList[A](stream: Stream[A]): List[A] = stream match {
      case Cons(h,t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()
    }

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match {
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()
    }

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match {
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(_, tail) => filter(tail())(pred)
      case _ => Empty()
    }

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream,n) match {
      case (Cons(head, tail), n) if n>0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()
    }

    def takeWhile[A](stream: Stream[A])(pred: A=>Boolean): Stream[A] = stream match {
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()
    }

    def fold[A,B](stream: Stream[A])(base: =>B)(op: (A,B)=>B): B = stream match {
      case Cons(head,tail) => op(head(), fold(tail())(base)(op))
      case Empty() => base
    }

    def peek[A](stream: Stream[A])(exec: A=>Unit): Stream[A] = stream match {
      case Cons(head,tail) => exec(head()); cons(head(), peek(tail())(exec))
      case Empty() => Empty()
    }

    def iterate[A](init: => A)(next: A => A): Stream[A] = cons(init, iterate(next(init))(next))

    def generate[A](next: => A): Stream[A] = cons(next, generate(next))

    def fromList[A](list: List[A]): Stream[A] = list match {
      case List.Cons(head, tail) => cons(head, fromList(tail))
      case _ => Empty()
    }

  }

  import Stream._
  val n = Random.nextInt(100)
  var str = generate(scala.io.StdIn.readLine())
  var str2 = map(str)(Integer.parseInt(_))
  str2 = takeWhile(str2)(_!=n)
  var str3 = map(str2)({case num if (num>n) => "higher"; case _ => "lower"})
  str3 = peek(str3)(s=>println("yours is: "+s))
  println("you won in "+fold(str3)(0)((a,b)=>b+1)+" steps")

  /*
  // var simplifies chaining of functions a bit..
  var str = Stream.iterate(0)(_+1)   // {0,1,2,3,..}
  str = Stream.map(str)(_+1)    // {1,2,3,4,..}
  str = Stream.filter(str)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  str = Stream.take(str)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str)) // [1,2,21,22,..,28]
  val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]
*/
}