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

    def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")


    def toList: List[A] = this match {
        case Empty => List.empty
        case Cons(h, t) => h() :: t().toList
    }


    def headOption: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))


    def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](empty[B])((a: A, b) => cons(f(a), b))

    def filter(f: A => Boolean): Stream[A] = foldRight[Stream[A]](empty[A])((a: A, b) => if (f(a)) cons(a, b) else b)

    def append[B >: A](s: => Stream[B]): Stream[B] = foldRight[Stream[B]](s)((a, b) => cons(a, b))


    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight[Stream[B]](empty[B])((a, b) => f(a) append b)


}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty
        else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = Stream.cons(1, ones)

    def from(n: Int): Stream[Int] = sys.error("todo")


    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}


object Main extends App {
    println(Stream(1, 2, 3, 5).takeWhile(_ <= 2).toList)
    println(Stream(1, 2, 3, 5, 4).takeWhileViaFoldRight(_ <= 4).toList)
    println(Stream(1, 2, 3, 5).take(3).toList)
    println(Stream(1, 2, 3, 5).drop(1).toList)
    println(Stream(1, 2, 3, 5).forAll(_ < 5))
    println(Stream(1, 2, 3, 5).headOption)
    println(Stream(1, 2, 3, 5).map(_ + 1).toList)
    println(Stream(1, 2, 3, 5, 1, 6).filter(_ <= 3).toList)
    println(Stream(1, 2, 3, 5, 1, 6).append(Stream(0, 0, 0)).toList)
    println(Stream().headOption)

    println(Stream(1, 2, 3, 5).flatMap(i => Stream(i, i)).toList)

}