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

    def listOfN(size: Gen[Int]): Gen[List[A]] = Gen(MyState(s => {
        val res: (Int, RNG) = size.sample.run(s)
        val gen: Gen[List[A]] = Gen.listOfN(res._1, this)
        gen.sample.run(res._2)


    }))
}


object Gen {
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen(MyState(s => {
        val res: (Boolean, RNG) = Gen.boolean.sample.run(s)
        if (res._1) g1.sample.run(res._2)
        else g2.sample.run(res._2)
    }))


    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
        Gen(MyState(RNG.double)).flatMap[A]((a: Double) => if (a < g1._2 / (g1._2 + g2._2)) g1._1 else g2._1)
    }


    def unit[A](a: => A): Gen[A] = Gen(MyState(RNG => (a, RNG)))

    def boolean: Gen[Boolean] = Gen(MyState(RNG.nonNegativeInt).map(n => n % 2 == 0))


    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
        Gen(MyState(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
    }

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = ???
}

//
//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}
//
trait SGen[+A] {

}

