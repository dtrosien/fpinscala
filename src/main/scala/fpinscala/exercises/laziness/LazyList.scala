package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toListR: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList

  def toList: List[A] =
    @annotation.tailrec
    def go(ll: LazyList[A], acc: List[A]): List[A] =
      ll match
        case Empty => acc.reverse
        case Cons(h, t) => go(t(), h() :: acc)

    go(this, Nil)

  def foldRight[B](z: => B)(f: (A, => B) => B)
      : B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if f(h()) then Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =
    @annotation.tailrec
    def go(ll: LazyList[A], n: Int, acc: LazyList[A]): LazyList[A] =
      if n > 0 then
        ll match
          case Empty => acc
          case Cons(h, t) => go(t(), n - 1, cons(h(), acc))
      else acc

    go(this, n, LazyList.Empty)

  def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this

  def takeWhile1(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => this

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile(p: A => Boolean): LazyList[A] = foldRight(empty)((a, b) => if p(a) then cons(a, b) else empty)

  def headOption: Option[A] = foldRight(None)((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): LazyList[B] = foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def filter(f: A => Boolean): LazyList[A] = foldRight(empty)((a, acc) => if f(a) then cons(a, acc) else acc)

  def append[A2 >: A](l: => LazyList[A2]): LazyList[A2] = foldRight(l)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] = foldRight(empty)((a, acc) => f(a).append(acc))

  def startsWith[A](prefix: LazyList[A]): Boolean =
    zipAll(prefix).takeWhile(_(1).isDefined).forAll((v1, v2) => v1 == v2)

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold(this, that):
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)):
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) => Some((Some(h1()) -> None) -> (t1() -> Empty))
      case (Empty, Cons(h2, t2)) => Some((None -> Some(h2())) -> (Empty -> t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()) -> Some(h2())) -> (t1() -> t2()))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this):
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)):
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this):
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None

  def tails: LazyList[LazyList[A]] =
    unfold(this):
      case l @ Cons(_, t) => Some(l, t())
      case empty => None
    .append(LazyList(empty))

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = cons(a, continually(a))

  def from(n: Int): LazyList[Int] = cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def go(curr: Int, next: Int): LazyList[Int] =
      cons(curr, go(next, curr + next))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty

  lazy val fibsViaUnfold: LazyList[Int] = unfold((0, 1))((curr, next) => Some(curr, (next, curr + next)))

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(n => Some(n, n + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(())(_ => Some(a, ()))

  lazy val onesViaUnfold: LazyList[Int] = unfold(())(_ => Some(1, ()))
