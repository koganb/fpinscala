package fpinscala.testing

import fpinscala.state._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

}


case class Gen[A](sample: MyState[RNG, A])



object Gen {
  def unit[A](a: => A): Gen[A] = Gen(MyState(RNG => (a, RNG)))

  def boolean: Gen[Boolean] = Gen(MyState(RNG.nonNegativeInt).map(n => n % 2 == 0))


  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    if (n <= 0) {
      List()
    }
    else {
      Gen
    }

    Gen(g.sample)
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(MyState(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))


  }
}

//
//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}
//
trait SGen[+A] {

}

