package fpinscala.state


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

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

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
    def map[B](f: A => B): State[S, B] =
        sys.error("todo")

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
        sys.error("todo")

    def flatMap[B](f: A => State[S, B]): State[S, B] =
        sys.error("todo")
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
    type Rand[A] = State[RNG, A]

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}


object Main extends App {
    println(RNG.ints(10)(RNG.Simple(42)))
}
