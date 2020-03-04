package com.spotify.scio.testing

import cats.kernel.Eq

sealed trait FallbackEqInstances {
  implicit def fallbackEq[A] =
    Eq.fromUniversalEquals[A]
}

trait EqInstances extends FallbackEqInstances {
  // == does not compare arrays for value equality, but for reference equality.
  implicit def arrayEq[T: Eq]: Eq[Array[T]] =
    new Eq[Array[T]] {
      val eq = Eq[T]
      def eqv(xs: Array[T], ys: Array[T]): Boolean =
        (xs.length == ys.length) &&
          (xs.zip(ys)).forall { case (x, y) => eq.eqv(x, y) }
    }
}

object EqInstances extends EqInstances
