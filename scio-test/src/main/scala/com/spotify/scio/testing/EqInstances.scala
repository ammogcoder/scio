package com.spotify.scio.testing

import cats.kernel.Eq
import shapeless.LowPriority

sealed trait FallbackEqInstances {
  implicit def fallbackEq[A](implicit lp: LowPriority) = new Eq[A] {
    def eqv(x: A, y: A): Boolean =
      (x, y) match {
        // case (null, null) => true
        // case (null, _) | (_, null) => false
        case (x: Array[_], y: Array[_]) => x.sameElements(y)
        case _                          => Eq.fromUniversalEquals[A].eqv(x, y)
      }
  }
}

trait EqInstances extends FallbackEqInstances {
  // == does not compare arrays for value equality, but for reference equality.
  implicit def arrayEq[T](implicit eqT: Eq[T]): Eq[Array[T]] =
    new Eq[Array[T]] {
      def eqv(xs: Array[T], ys: Array[T]): Boolean =
        (xs.length == ys.length) &&
          (xs.zip(ys)).forall { case (x, y) => eqT.eqv(x, y) }
    }
}

object EqInstances extends EqInstances
