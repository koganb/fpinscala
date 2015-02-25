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


case class Gen[A](sample: MyState[RNG, A]) {
    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(MyState(s => {
        val res: (A, RNG) = sample.run(s)
        f(res._1).sample.run(res._2)
    }))

    def listOfN(size: Gen[Int]): Gen[List[A]] = ???
}


object Gen {
    def unit[A](a: => A): Gen[A] = Gen(MyState(RNG => (a, RNG)))

    def boolean: Gen[Boolean] = Gen(MyState(RNG.nonNegativeInt).map(n => n % 2 == 0))


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

