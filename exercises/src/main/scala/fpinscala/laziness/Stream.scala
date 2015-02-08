package fpinscala.laziness

import fpinscala.laziness.Stream._

trait Stream[+A] {

    def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
        this match {
            case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
            case _ => z
        }

    def exists(p: A => Boolean): Boolean =
        foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

    @annotation.tailrec
    final def find(f: A => Boolean): Option[A] = this match {
        case Empty => None
        case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
    }

    def take(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
        case _ => empty
    }

    def drop(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 0 => t().drop(n - 1)
        case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
        case _ => empty
    }

    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
        foldRight[Stream[A]](empty[A])((a: A, b) => if (p(a)) cons(a, b) else empty[A])


    def forAll(p: A => Boolean): Boolean = this match {
        case Cons(h, t) => p(h()) && t().forAll(p)
        case _ => true
    }

    def startsWith[B](s: Stream[B]): Boolean = zipAllViaUnfold(s).forAll({
        case (Some(h1), Some(h2)) => h1 == h2
        case (Some(_), _) => true
        case _ => false
    })


    def toList: List[A] = this match {
        case Empty => List.empty
        case Cons(h, t) => h() :: t().toList
    }


    def headOption: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))


    def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](empty[B])((a: A, b) => cons(f(a), b))


    def mapViaUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
        case Cons(h, t) => Some(f(h()), t())
        case _ => None
    }

    def takeViaUnfold[B](n: Int): Stream[A] = Stream.unfold((this, n)) {
        case (Cons(h, t), c) if c > 0 => Some(h(), (t(), c - 1))
        case _ => None
    }


    def takeWhileViaUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
        case Cons(h, t) if p(h()) => Some(h(), t())
        case _ => None
    }

    def zipWithViaUnfold[B, C](b: Stream[B])(f: (A, B) => C) = Stream.unfold(this, b) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
        case _ => None
    }

    def tailsViaUnfold: Stream[Stream[A]] = Stream.unfold(this) {
        case stream@Cons(_, t) => Some(stream, t())
        case _ => None
    } append Stream(empty)


    def zipAllViaUnfold[B](b: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold(this, b) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
        case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), empty))
        case (_, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
        case _ => None
    }

    def filter(f: A => Boolean): Stream[A] = foldRight[Stream[A]](empty[A])((a: A, b) => if (f(a)) cons(a, b) else b)

    def append[B >: A](s: => Stream[B]): Stream[B] = foldRight[Stream[B]](s)((a, b) => cons(a, b))


    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight[Stream[B]](empty[B])((a, b) => f(a) append b)

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
        foldRight((z, Stream(z)))((a, p0) => {
            // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
            lazy val p1 = p0
            val b2 = f(a, p1._1)
            (b2, cons(b2, p1._2))
        })._2

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    val ones: Stream[Int] = Stream.cons(1, ones)

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty
        else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = {
        lazy val const: Stream[A] = Stream.cons(a, const)
        const
    }

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

    def fibs(): Stream[Int] = {

        def fibs(a: Int, b: Int): Stream[Int] = Stream.cons(a + b, fibs(b, a + b))

        Stream.cons(0, Stream.cons(1, fibs(0, 1)))
    }


    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
        f(z) match {
            case Some((a, s)) => Stream.cons(a, unfold(s)(f))
            case None => Stream.empty
        }
    }

    def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(i => Some((i, i + 1)))

    def constantViaUnfold(n: Int): Stream[Int] = unfold(n)(i => Some((i, i)))

    def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

    def fibsViaUnfold(n: Int): Stream[Int] = unfold((0, 1))(i => Some((i._1, (i._2, i._1 + i._2))))


}


object Main extends App {
    println(Stream(1, 2, 3, 5).scanRight(0)(_ + _).toList)
    println(Stream(1, 2, 3, 5).tailsViaUnfold.toList.map(_.toList))
    println(Stream(1, 2, 3, 5).startsWith(Stream(1, 2, 4)))

    println(Stream(1, 2, 3, 5).zipAllViaUnfold(Stream(1, 2, 3)).toList)
    println(Stream(1, 2, 3, 5).zipWithViaUnfold(Stream(1, 2, 3, 5))(_ + _).toList)


    println(Stream(1, 2, 3, 5).takeWhile(_ <= 2).toList)
    println(Stream(1, 2, 3, 5, 4).takeWhileViaFoldRight(_ <= 4).toList)
    println(Stream(1, 2, 3, 5, 4).takeWhileViaUnfold(_ <= 4).toList)

    println("take via unfold")
    println(Stream(1, 2, 3, 5).take(2).toList)
    println(Stream(1, 2, 3, 5).takeViaUnfold(2).toList)

    println("drop")
    println(Stream(1, 2, 3, 5).drop(1).toList)
    println(Stream(1, 2, 3, 5).forAll(_ < 5))
    println(Stream(1, 2, 3, 5).headOption)

    println("map via unfold")
    println(Stream(1, 2, 3, 5).map(_ + 1).toList)
    println(Stream(1, 2, 3, 5).mapViaUnfold(_ + 1).toList)

    println("filter")
    println(Stream(1, 2, 3, 5, 1, 6).filter(_ <= 3).toList)
    println(Stream(1, 2, 3, 5, 1, 6).append(Stream(0, 0, 0)).toList)
    println(Stream().headOption)

    println(Stream(1, 2, 3, 5).flatMap(i => Stream(i, i)).toList)


    println(Stream.from(9).take(10).toList)
    println(Stream.fromViaUnfold(9).take(10).toList)
    println(Stream.fibs().take(10).toList)
    println(Stream.fibsViaUnfold(9).take(10).toList)

    println(Stream.constantViaUnfold(9).take(10).toList)
    println(Stream.onesViaUnfold.take(10).toList)


}