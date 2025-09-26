package kuplrg

trait Template {

  type Nat <: NatOps
  trait NatOps {
    def toInt: Int
    def +(that: Nat): Nat
    def *(that: Nat): Nat
    def **(that: Nat): Nat
    def isEven: Boolean
    def isOdd: Boolean
  }

  type Poly <: PolyOps
  trait PolyOps {
    def eval(x: Int): Int
    def +(that: Poly): Poly
    def *(k: Int): Poly
    def *(that: Poly): Poly
    def normalize: Poly
  }
}
