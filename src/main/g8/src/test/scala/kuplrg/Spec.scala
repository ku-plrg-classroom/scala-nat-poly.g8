package kuplrg

import Implementation.*

class Spec extends SpecBase {
  import Nat.*

  def testUOp[T](
    bound: Int,
    op: Nat => T,
    intOp: Int => T,
  ): Boolean = (0 to bound).forall(i => op(Nat(i)) == intOp(i))

  def testBOp(
    bound: Int,
    op: (Nat, Nat) => Nat,
    intOp: (Int, Int) => Int,
  ): Boolean = (for {
    i <- 0 to bound
    j <- 0 to bound
  } yield (i, j))
    .forall((i, j) => op(Nat(i), Nat(j)) == Nat(intOp(i, j)))

  // tests for the method `toInt` of Nat
  test(Z.toInt, 0)
  test(S(Z).toInt, 1)
  test(S(S(Z)).toInt, 2)
  test(S(S(S(Z))).toInt, 3)
  test(S(S(S(S(Z)))).toInt, 4)
  test(testUOp(1000, _.toInt, identity), true, weight = 5)

  // tests for the method `+` of Nat
  test(Z + Z, Z)
  test(Z + S(Z), S(Z))
  test(S(Z) + Z, S(Z))
  test(S(S(S(Z))) + S(S(Z)), Nat(5))
  test(S(S(S(S(Z)))) + S(S(S(S(Z)))), Nat(8))
  test(testBOp(100, _ + _, _ + _), true, weight = 5)

  // tests for the method `*` of Nat
  test(Z * Z, Z)
  test(S(Z) * S(S(Z)), S(S(Z)))
  test(S(S(Z)) * Z, Z)
  test(S(S(S(Z))) * S(S(Z)), Nat(6))
  test(S(S(S(S(Z)))) * S(S(S(Z))), Nat(12))
  test(testBOp(10, _ * _, _ * _), true, weight = 5)

  // tests for the method `**` of Nat
  test(Z ** Z, S(Z))
  test(S(Z) ** S(S(Z)), S(Z))
  test(S(S(Z)) ** Z, S(Z))
  test(S(S(S(Z))) ** S(S(Z)), Nat(9))
  test(S(S(S(S(Z)))) ** S(S(S(Z))), Nat(64))
  test(testBOp(5, _ ** _, math.pow(_, _).toInt), true, weight = 5)

  // tests for the method `isEven` or `isOdd` of Nat
  test(Z.isOdd, false)
  test(S(Z).isEven, false)
  test(S(S(Z)).isEven, true)
  test(S(S(S(Z))).isOdd, true)
  test(S(S(S(S(Z)))).isEven, true)
  test(testUOp(1000, _.isEven, _ % 2 == 0), true, weight = 2)
  test(testUOp(1000, _.isOdd, _ % 2 == 1), true, weight = 3)

  // tests for the method `eval` of Nat
  test(Poly().eval(3), 0)
  test(Poly(1).eval(2), 1)
  test(Poly(1, 2).eval(4), 9)
  test(Poly(1, 2, 3).eval(1), 6)
  test(Poly(1, 2, 3, 4).eval(5), 586)
  test(Poly(1, 2, 3, 4, 5).eval(2), 129)
  test(Poly(5, 3, 4, 2).eval(5), 370)
  test(Poly(5, 3, 4, 2, 5).eval(7), 12913)
  test(Poly(5, 3, 4, 2, 9, 7).eval(10), 792435)
  test(Poly(5, 3, 6, 2, 3, 4, 2).eval(2), 355)

  // tests for the method `+` of Poly
  test(Poly() + Poly(), Poly())
  test(Poly() + Poly(1), Poly(1))
  test(Poly(1) + Poly(1, 2, 3), Poly(2, 2, 3))
  test(Poly(1, 2) + Poly(1, 2, 3), Poly(2, 4, 3))
  test(Poly(1, 2, 3) + Poly(1), Poly(2, 2, 3))
  test(Poly(1, 2, 3) + Poly(1, 2), Poly(2, 4, 3))
  test(Poly(1, 2, 3) + Poly(1, 2, 3), Poly(2, 4, 6))
  test(Poly(-1, 0, 0, 1) + Poly(1, 0, 0, -1), Poly())
  test(Poly(1, 2, 3) + Poly(1, 2, -3), Poly(2, 4))
  test(Poly(1, 2, 3, 4) + Poly(2, 4, -3, -4), Poly(3, 6))

  // tests for the method `*` of Poly (scalar multiplication)
  test(Poly() * 0, Poly())
  test(Poly(1) * 0, Poly())
  test(Poly(1, 2, 3) * 0, Poly())
  test(Poly() * 1, Poly())
  test(Poly(1) * 1, Poly(1))
  test(Poly(1, 2, 3) * 1, Poly(1, 2, 3))
  test(Poly() * 2, Poly())
  test(Poly(1) * 2, Poly(2))
  test(Poly(1, 2, 3) * 2, Poly(2, 4, 6))
  test(Poly(1, 5, 4, 6, 2, 3) * 10, Poly(10, 50, 40, 60, 20, 30))

  // tests for the method `*` of Poly
  test(Poly() * Poly(), Poly())
  test(Poly() * Poly(1), Poly())
  test(Poly(-2, 1) * Poly(2, 1), Poly(-4, 0, 1))
  test(Poly(1) * Poly(), Poly())
  test(Poly(1, 2) * Poly(1, 2), Poly(1, 4, 4))
  test(Poly(1, 2) * Poly(1, 2, 3), Poly(1, 4, 7, 6))
  test(Poly(1, 2, 3) * Poly(1, 2), Poly(1, 4, 7, 6))
  test(Poly(1, 2, 3) * Poly(1, 2, 3), Poly(1, 4, 10, 12, 9))
  test(Poly(1, 2, 3, 4) * Poly(1, 2), Poly(1, 4, 7, 10, 8))
  test(Poly(3, 5, 2, 5) * Poly(3, 5, 2, 3), Poly(9, 30, 37, 44, 44, 16, 15))

  // tests for the method `normalize` of Poly
  test(Poly().normalize, Poly())
  test(Poly(0).normalize, Poly())
  test(Poly(0, 0).normalize, Poly())
  test(Poly(0, 0, 0).normalize, Poly())
  test(Poly(1).normalize, Poly(1))
  test(Poly(1, 0).normalize, Poly(1))
  test(Poly(1, 0, 0).normalize, Poly(1))
  test(Poly(1, 0, 0, 0).normalize, Poly(1))
  test(Poly(3, 0, 5, 0, 5).normalize, Poly(3, 0, 5, 0, 5))
  test(Poly(5, 0, 6, 0, 0, 0, 0).normalize, Poly(5, 0, 6))

  /* Write your own tests */
}
