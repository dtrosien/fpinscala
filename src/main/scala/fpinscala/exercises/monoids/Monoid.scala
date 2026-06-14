package fpinscala.exercises.monoids

import fpinscala.exercises.monoids.Monoid.WC.Stub
import fpinscala.exercises.parallelism.Nonblocking.*
import fpinscala.exercises.testing.Prop.Result

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  lazy val intAddition: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 + a2
    def empty: Int = 0

  lazy val intMultiplication: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 * a2
    def empty: Int = 1

  lazy val booleanOr: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 | a2
    def empty: Boolean = false

  lazy val booleanAnd: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 & a2
    def empty: Boolean = true

  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def empty: Option[A] = None

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty = m.empty

  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(a1: A => A, a2: A => A): A => A = a1 andThen a2
    def empty: A => A = identity

  import fpinscala.answers.testing.{Gen, Prop}
  import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    val associativity = Prop
      .forAll(gen ** gen ** gen):
        case a ** b ** c =>
          m.combine(a, m.combine(b, c)) == m.combine(m.combine(a, b), c)
      .tag("associativity")
    val identity = Prop
      .forAll(gen): a =>
        m.combine(a, m.empty) == a && m.combine(m.empty, a) == a
      .tag("identity")
    associativity && identity

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldRight(m.empty)((a, b) => m.combine(a, b))

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldRight(m.empty)((a, b) => m.combine(f(a), b))

  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    // val curriedF = (a: A) => f(a, _)
    foldMap(as, endoMonoid)(f.curried)(acc)

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid))(a => b => f(b, a))(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as.length match
      case i: Int if i == 0 => m.empty
      case i: Int if i == 1 => f(as.head)
      case i: Int =>
        val (l, r) = as.splitAt(i / 2)
        m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]:
    def empty = Par.unit(m.empty)
    def combine(a: Par[A], b: Par[A]) = a.map2(b)(m.combine)

  // we perform the mapping and the reducing both in parallel
  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(as)(f).flatMap: bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))

  opaque type Interval = (Int, Int)

  val orderedMonoid: Monoid[(Boolean, Option[Interval])] = new:
    def combine(a1: (Boolean, Option[Interval]), a2: (Boolean, Option[Interval])) =
      (a1(1), a2(1)) match
        case (Some((leftMin, leftMax)), Some((rightMin, rightMax))) =>
          (a1(0) && a2(0) && leftMax <= rightMin, Some((leftMin, rightMax)))
        case _ =>
          (a1(0) && a2(0), a1(1).orElse(a2(1)))
    val empty = (true, None)

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, orderedMonoid)(i => (true, Some((i, i))))(0)

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  lazy val wcMonoid: Monoid[WC] = new:
    def combine(a1: WC, a2: WC): WC =
      import WC.*
      (a1, a2) match
        case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
        case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
        case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
        case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2, r1 + l2 + r2)

    def empty: WC = Stub("")

  def count(s: String): Int =
    def unstub(s: String) = if s.isBlank() then 0 else 1
    foldMapV(s, wcMonoid)(ss => Stub(s)) match
      case Stub(chars) => unstub(chars)
      case WC.Part(lStub, words, rStub) => unstub(lStub) + words + unstub(rStub)

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)) = (ma.combine(x(0), y(0)), mb.combine(x(1), y(1)))
    val empty = (ma.empty, mb.empty)

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B) = a => mb.combine(f(a), g(a))
    val empty: A => B = a => mb.empty

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(empty): (acc, k) =>
        acc.updated(
          k,
          mv.combine(
            a.getOrElse(k, mv.empty),
            b.getOrElse(k, mv.empty)
          )
        )
    val empty = Map()

  given Monoid[Int] = intAddition

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    import Foldable.given
    as.foldMap(x => Map(x -> 1))

  given _listMonoid[A]: Monoid[List[A]] = listMonoid
end Monoid
