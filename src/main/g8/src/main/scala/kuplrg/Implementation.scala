package kuplrg

import scala.annotation.tailrec

object Implementation extends Template {

  /** Natural numbers (Z for zero, S for successor) */
  enum Nat extends NatOps:
    case Z
    case S(n: Nat)

    /** Conversiont o integer */
    def toInt: Int =
      @tailrec
      def aux(n: Nat, acc: Int): Int = aux(???, ???)
      ???

    /** Addition */
    def +(that: Nat): Nat =
      @tailrec
      def aux(n: Nat, acc: Nat): Nat = aux(???, ???)
      ???

    /** Multiplication */
    def *(that: Nat): Nat =
      @tailrec
      def aux(n: Nat, acc: Nat): Nat = aux(???, ???)
      ???

    /** Exponentiation */
    def **(that: Nat): Nat =
      @tailrec
      def aux(n: Nat, acc: Nat): Nat = aux(???, ???)
      ???

    /** Check if the number is even */
    def isEven: Boolean = ???

    /** Check if the number is odd */
    def isOdd: Boolean = ???

  object Nat:

    // Constructors
    @tailrec
    def apply(n: Int, default: Nat = Z): Nat =
      if (n < 1) default
      else apply(n - 1, S(default))

  /** Polynomials
    *
    * @example
    *
    * The following denotes `1 + 2x + 3x^2`:
    * {{{
    *   Poly(List(1, 2, 3))
    * }}}
    * The following denotes `-5 + x^4`:
    * {{{
    *   Poly(List(-5, 0, 0, 0, 1))
    * }}}
    */
  case class Poly(coefficients: List[Int]) extends PolyOps:

    /** Evaluation */
    def eval(x: Int): Int = ???

    /** Addition */
    def +(that: Poly): Poly = ???

    /** Scala Multiplication */
    def *(k: Int): Poly = ???

    /** Multiplication */
    def *(that: Poly): Poly = ???

    /** Normalization */
    def normalize: Poly = ???

    /** String representation */
    override def toString: String = (for {
      (c, i) <- coefficients.zipWithIndex
      if c != 0
    } yield i match
      case 0 => s"$c"
      case 1 => s"${c}x"
      case i => s"${c}x^$i"
    ).mkString(" + ")

  object Poly:

    // Constructors
    def apply(coefficients: Int*): Poly = Poly(coefficients.toList)
}
