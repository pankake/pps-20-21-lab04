package u04lab.code

import scala.annotation.tailrec

object Lists extends App {

  // A generic linkedlist
  sealed trait List[E]

  // a companion object (i.e., module) for List
  object List {

    def apply[A](items: A*): List[A] = {
      var list: List[A] = Nil()
      items.foreach(e => list = append(list, Cons(e, Nil())))
      list
    }

    case class Cons[E](head: E, tail: List[E]) extends List[E]

    case class Nil[E]() extends List[E]

    def nil[A]: List[A] = Nil() // smart constructor

    def sum(l: List[Int]): Int = l match {
      case Cons(h, t) => h + sum(t)
      case _ => 0
    }

    def append[A](l1: List[A], l2: List[A]): List[A] = (l1, l2) match {
      case (Cons(h, t), l2) => Cons(h, append(t, l2))
      case _ => l2
    }

    def drop[A](l: List[A], n: Int): List[A] = l match {
      case _ if n <= 0 || l == Nil() => l
      case Cons(h, t) => drop(t, n - 1)
    }

    def map[A, B](l: List[A])(f: A => B): List[B] = l match {
      case Cons(h, t) => Cons(f(h), map(t)(f))
      case Nil() => Nil()
    }

    def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
      case Cons(h, t) => filter(t)(f)
      case Nil() => Nil()
    }

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil()
    }

    @tailrec
    def foldLeft[A, B](l: List[A])(acc: B)(f: (B, A) => B): B = l match {
      case Cons(h, t) => foldLeft(t)(f(acc, h))(f)
      case Nil() => acc
    }

    def appendByFold[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1)(l2)((x, acc) => Cons(x, acc))

    def foldRightNonTailRec[A, B](l: List[A])(acc: B)(f: (A, B) => B): B = l match {
      case Cons(h, t) => f(h, foldRightNonTailRec(t)(acc)(f))
      case Nil() => acc
    }

    def reverse[A](l: List[A]): List[A] =
      foldLeft(l)(nil[A])((acc, elem) => Cons(elem, acc))

    def foldRightViaFoldleft[A, B](l: List[A])(acc: B)(f: (A, B) => B): B =
      foldLeft(reverse(l))(acc)((acc, elem) => f(elem, acc))

    def foldRight[A, B](l: List[A])(acc: B)(f: (A, B) => B): B =
      foldRightViaFoldleft(l)(acc)(f)

    def filterByFlatmap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l) {
      case a if f(a) => Cons(a, Nil())
      case _ => Nil()
    }

    def length(l: List[_]): Int = foldLeft(l)(0)((acc, _) => acc + 1)

    @tailrec
    def contains[A](l: List[A])(elem: A): Boolean = l match {
      case Cons(head, _) if head == elem => true
      case Cons(_, tail) => contains(tail)(elem)
      case Nil() => false
    }

    trait ListFactory {
      def create
    }
  }

  // Note "List." qualification
  println(List.sum(List.Cons(10, List.Cons(20, List.Cons(30, List.Nil()))))) // 60
  import List._
  println(append(Cons("10", Nil()), Cons("1", Cons("2", Nil())))) // "10","1","2"

  println(drop(Cons(10, Cons(20, Cons(30, Nil()))),2)) // Cons(30, Nil())
  println(drop(Cons(10, Cons(20, Cons(30, Nil()))),5)) // Nil()
  println(drop(Nil(), 5)) // Nil()

  println(map(Cons(10, Cons(20, Nil())))(_+1))       // Cons(11, Cons(21, Nil()))
  println(map(Cons(10, Cons(20, Nil())))(":"+_+":")) // Cons(":10:", Cons(":20:",Nil()))
  println(filter(Cons(10, Cons(20, Nil())))(_>15)) // Cons(20, Nil())
  println(filter(Cons("a", Cons("bb", Cons("ccc", Nil()))))( _.length <=2)) // Cons("a",Cons("bb", Nil()))

  val lst = Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))
  println(foldLeft(lst)(0)(_-_)) // -16
  println(reverse(lst)) // Cons(5,Cons(1,Cons(7,Cons(3,Nil()))))
  println(foldRightNonTailRec(lst)(0)(_-_)) // -8
  println(foldRightViaFoldleft(lst)(0)(_-_)) // -8

  // EXERCISES:
  println(filterByFlatmap(Cons(10, Cons(20, Nil())))(_>15)) // Cons(20, Nil())
  println(filterByFlatmap(Cons("a", Cons("bb", Cons("ccc", Nil()))))( _.length <=2)) // Cons("a",Cons("bb", Nil()))
  println(appendByFold(Cons(3,Cons(7,Nil())), Cons(1,Cons(5,Nil())))) // Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))
  println(length(Nil())) // 0
  println(length(Cons(3,Cons(7,Cons(1,Cons(5, Nil())))))) // 4

  println(List(10, 20, 30, 40, 50)) //Cons(10,Cons(20,Cons(30,Cons(40,Cons(50,Nil())))))
}