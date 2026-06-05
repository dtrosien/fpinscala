package fpinscala.exercises.testing

import java.util.concurrent.{Executors, ExecutorService}

import annotation.targetName
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import fpinscala.exercises.state.*
import Gen.*
import Prop.*
import Result.*
/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */
opaque type Prop = (MaxSize, TestCases, RNG) => Result
object Prop:
  opaque type SuccessCount = Int
  object SuccessCount:
    extension (x: SuccessCount) def toInt: Int = x
    def fromInt(x: Int): SuccessCount = x

  opaque type TestCases = Int
  object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x

  opaque type MaxSize = Int
  object MaxSize:
    extension (x: MaxSize) def toInt: Int = x
    def fromInt(x: Int): MaxSize = x

  opaque type FailedCase = String
  object FailedCase:
    extension (f: FailedCase) def string: String = f
    def fromString(s: String): FailedCase = s

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)
    // Proved

    def isFalsified: Boolean = this match
      case Passed => false
      case Falsified(_, _) => true
      // case Proved => false

  /* Produce an infinite random lazy list from a `Gen` and a starting `RNG`. */
  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop: (n, rng) =>
    randomLazyList(as)(rng).zip(LazyList.from(0)).take(n).map:
      case (a, i) =>
        try
          if f(a) then Passed else Falsified(a.toString, i)
        catch
          case e: Exception => Falsified(buildMsg(a, e), i)
    .find(_.isFalsified).getOrElse(Passed)

  @targetName("forAllSized")
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    (max, n, rng) =>
      val casesPerSize = (n.toInt - 1) / max.toInt + 1
      val props: LazyList[Prop] =
        LazyList.from(0).take((n.toInt min max.toInt) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map[Prop](p => (max, n, rng) => p(max, casesPerSize, rng)).toList.reduce(_ && _)
      prop(max, n, rng)

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases, RNG) => Result): Prop =
    (_, n, rng) => f(n, rng)

  extension (self: Prop)
    def &&(that: Prop): Prop = (m, n, r) =>
      self(m, n, r) match
        case Passed => that(m, n, r)
        case x => x

    def ||(that: Prop): Prop = (m, n, r) =>
      self(m, n, r) match
        case Falsified(_, _) => that(m, n, r)
        case x => x

    def check(
        maxSize: MaxSize = 100,
        testCases: TestCases = 100,
        rng: RNG = RNG.Simple(System.currentTimeMillis)
    ): Result =
      self(maxSize, testCases, rng)

    def run(
        maxSize: MaxSize = 100,
        testCases: TestCases = 100,
        rng: RNG = RNG.Simple(System.currentTimeMillis)
    ): Unit =
      self(maxSize, testCases, rng) match
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
        // case Proved =>
        //   println(s"+ OK, proved property.")

opaque type SGen[+A] = Int => Gen[A]
object SGen:
  def apply[A](f: Int => Gen[A]): SGen[A] = f
  def unit[A](a: => A): SGen[A] = _ => Gen.unit(a)
  def choose(start: Int, stopExclusive: Int): SGen[Int] = _ => Gen.choose(start, stopExclusive)
  def boolean: SGen[Boolean] = _ => Gen.boolean
  def union[A](g1: Gen[A], g2: Gen[A]): SGen[A] = _ => Gen.union(g1, g2)
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): SGen[A] = _ => Gen.weighted(g1, g2)
  extension [A](self: SGen[A])
    def flatMap[B](f: A => SGen[B]): SGen[B] = n => self(n).flatMap(a => f(a)(n))
    def map[B](f: A => B): SGen[B] = n => self(n).map(f)
    def apply(n: Int): Gen[A] = self(n)

opaque type Gen[+A] = State[RNG, A]
object Gen:
  def unit[A](a: => A): Gen[A] = State.unit(a)
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))
  def boolean: Gen[Boolean] = State(RNG.int).map(n => n < 0)
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if b then g1 else g2)
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    State(RNG.double).flatMap(d => if d < g1Threshold then g1._1 else g2._1)

  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] = State.flatMap(self)(f)
    def maap[B](f: A => B): Gen[B] = State.map(self)(f)
    // We should use a different method name to avoid looping (not 'run')
    def next(rng: RNG): (A, RNG) = self.run(rng)
    def listOfN(n: Int): Gen[List[A]] = State.sequence(List.fill(n)(self))
    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(listOfN)
    def unsized: SGen[A] = _ => self
    def list: SGen[List[A]] = n => self.listOfN(n)
    def nonEmptyList: SGen[List[A]] = n => self.listOfN(n.max(1))
