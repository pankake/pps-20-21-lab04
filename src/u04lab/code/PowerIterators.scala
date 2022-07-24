package u04lab.code

import Optionals.Option
import Optionals.Option._

import Lists._
import Lists.List._
import Streams.Stream
import Streams.Stream._

import scala.util.Random

trait PowerIterator[A] {
  def next(): Option[A]
  def allSoFar(): List[A]
  def reversed(): PowerIterator[A]
}

trait PowerIteratorsFactory {
  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  def fromList[A](list: List[A]): PowerIterator[A]
  def randomBooleans(size: Int): PowerIterator[Boolean]
}

object PowerIterator {

  def apply[A](stream: Stream[A]): PowerIterator[A] = PowerIteratorImpl(stream)

  private case class PowerIteratorImpl[A](private var stream: Stream[A]) extends PowerIterator[A] {

    private var _list: List[A] = Nil()

    override def next(): Option[A] = stream match {
      case Stream.Cons(head, tail) => {
        _list = append(_list, List.Cons(head(), Nil()))
        stream = tail()
        Some(head())
      }
      case _ => None()
    }

    override def allSoFar(): List[A] = _list

    override def reversed(): PowerIterator[A] = new PowerIteratorsFactoryImpl().fromList(reverse(_list))
  }
}

class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] = PowerIterator(iterate(start)(successive))

  override def fromList[A](list: List[A]): PowerIterator[A] = PowerIterator(Stream.fromList(list))

  override def randomBooleans(size: Int): PowerIterator[Boolean] = PowerIterator(take(generate(Random.nextBoolean()))(size))
}

