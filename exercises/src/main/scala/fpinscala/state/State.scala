package fpinscala.state

import fpinscala.state.RNG.Simple


trait RNG {
    def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

    // NB - this was called SimpleRNG in the book text

    type Rand[+A] = RNG => (A, RNG)
    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
        rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }


    def doubleViaMap(rng: RNG): (Double, RNG) = map[Int, Double](nonNegativeInt)((i: Int) => (i - 1).toDouble / Int.MaxValue)(rng)


    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val res: ((Int, Double), RNG) = intDouble(rng)
        ((res._1._2, res._1._1), res._2)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val res: (Int, RNG) = rng.nextInt
        val res1: (Double, RNG) = double(res._2)
        ((res._1, res1._1), res1._2)
    }

    def double(rng: RNG): (Double, RNG) = {
        val res: (Int, RNG) = nonNegativeInt(rng)
        ((res._1 - 1).toDouble / Int.MaxValue, res._2)
    }

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val nextInt: (Int, RNG) = rng.nextInt
        val nextIntValue = if (nextInt._1 == Int.MinValue) Int.MaxValue else math.abs(nextInt._1)
        (nextIntValue, nextInt._2)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val res: (Double, RNG) = double(rng)
        val res1: (Double, RNG) = double(res._2)
        val res2: (Double, RNG) = double(res1._2)
        ((res._1, res1._1, res2._1), res2._2)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

        def int_req(c: Int, rng: RNG, list: List[Int]): (List[Int], RNG) = {
            if (c <= 0) (list, rng)
            else {
                val res: (Int, RNG) = rng.nextInt
                int_req(c - 1, res._2, res._1 :: list)
            }
        }
        int_req(count, rng, List())
    }

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        rng => {
            val (a, rng1) = ra(rng)
            val (b, rng2) = rb(rng1)
            (f(a, b), rng2)
        }
    }

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs.foldRight(List[A](), rng)(
        (a: Rand[A], b: (List[A], RNG)) => {
            val tuple: (A, RNG) = a(b._2)
            (tuple._1 :: b._1, tuple._2)
        })


    def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) = {
        sequence(List.fill(count)(int))(rng)
    }


    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
        rng => {
            val (a, rng1) = f(rng)
            g(a)(rng1)
        }
    }

    def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)((a: A) => unit(f(a)))

    def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        flatMap(ra)((a: A) => rng => {
            val (b, rng2) = rb(rng)
            (f(a, b), rng2)
        })
    }

    def map2ViaFlatMap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        flatMap(ra)(a => map[B, C](rb)(b => f(a, b)))
    }


    case class Simple(seed: Long) extends RNG {
        def nextInt: (Int, RNG) = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
            val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
            val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
            (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
        }
    }

}

case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] = State((s: S) => {
        val res: (A, S) = run(s)
        (f(res._1), res._2)
    })

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State((s: S) => {
        val res: (A, S) = run(s)
        val res1: (B, S) = sb.run(res._2)
        (f(res._1, res1._1), res._2)
    })

    def flatMap[B](f: A => State[S, B]): State[S, B] = State((s: S) => {
        val res: (A, S) = run(s)
        f(res._1).run(res._2)
    }
    )

}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
    type Rand[A] = State[RNG, A]

    def unit[S, A](a: A): State[S, A] = State({ s => (a, s)})

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State(s => fs.foldRight(List[A](), s)(
        (a: State[S, A], b: (List[A], S)) => {
            val res: (A, S) = a.run(b._2)
            (res._1 :: b._1, res._2)
        }))


    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
        sequence(inputs.map { input => State(
            (machine: Machine) => input match {
                case Coin if machine.candies > 0 && machine.locked => ((machine.candies, machine.coins), Machine(false, machine.coins, machine.candies))
                case Turn if machine.candies > 0 && !machine.locked => ((machine.candies - 1, machine.coins + 1), Machine(true, machine.coins + 1, machine.candies - 1))
                case _ => ((machine.candies, machine.coins), machine)
            })
        })

    }.map(_.last)
}


object Main extends App {
    private val simple: Simple = RNG.Simple(42)
    println(RNG.ints(10)(simple))
    println(RNG.intsViaSequence(10)(simple))


    private val state: State[Machine, (Int, Int)] = State.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
    state.run(Machine(false, 5, 10))


}
