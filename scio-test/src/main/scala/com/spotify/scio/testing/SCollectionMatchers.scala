/*
 * Copyright 2019 Spotify AB.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package com.spotify.scio.testing

import java.lang.{Iterable => JIterable}
import java.util.{Map => JMap}

import com.spotify.scio.coders.Coder
import com.spotify.scio.values.SCollection
import com.twitter.chill.Externalizer
import org.apache.beam.sdk.testing.PAssert
import org.apache.beam.sdk.testing.PAssert.{IterableAssert, SingletonAssert}
import org.apache.beam.sdk.transforms.SerializableFunction
import org.apache.beam.sdk.transforms.windowing.BoundedWindow
import org.apache.beam.sdk.util.CoderUtils
import org.scalatest.matchers.{MatchResult, Matcher}
import org.hamcrest.Matchers
import org.hamcrest.MatcherAssert.assertThat

import scala.collection.JavaConverters._
import scala.reflect.ClassTag
import com.twitter.chill.ClosureCleaner
import cats.kernel.Eq

private[testing] case class TestWrapper[T: Eq](value: T) {
  override def toString: String = Pretty.printer.apply(value).render
  override def equals(other: Any): Boolean =
    other match {
      case TestWrapper(o: T) => Eq[T].eqv(value, o)
      case o                 => value.equals(o)
    }
}

object TestWrapper {
  def wrap[T: Coder: Eq](coll: SCollection[T]) =
    coll.map(t => TestWrapper(t))

  implicit def testWrapperCoder[T: Coder: Eq]: Coder[TestWrapper[T]] =
    Coder.gen[TestWrapper[T]]
}

/**
 * Trait with ScalaTest [[org.scalatest.matchers.Matcher Matcher]]s for
 * [[com.spotify.scio.values.SCollection SCollection]]s.
 */
trait SCollectionMatchers {

  sealed trait MatcherBuilder[T] {
    _: Matcher[T] =>

    type From
    type To >: From
    type AssertBuilder = From => To

    def matcher(builder: AssertBuilder): Matcher[T]

    def matcher: Matcher[T] = matcher(identity)
  }

  sealed trait IterableMatcher[T, B] extends MatcherBuilder[T] with Matcher[T] {
    type From = IterableAssert[B]
    type To = From

    override def apply(left: T): MatchResult = matcher(left)
  }

  sealed trait SingleMatcher[T, B] extends MatcherBuilder[T] with Matcher[T] {
    type From = SingletonAssert[B]
    type To = From

    override def apply(left: T): MatchResult = matcher(left)
  }

  /*
  Wrapper for PAssert statements. PAssert does not perform assertions or throw exceptions until
  sc.run() is called. So MatchResult should always match true for "a should $Matcher" cases and
  false for "a shouldNot $Matcher" cases. We also need to run different assertions for positive
  (shouldFn) and negative (shouldNotFn) cases.
   */
  private def m(shouldFn: () => Any, shouldNotFn: () => Any): MatchResult = {
    val isShouldNot = Thread
      .currentThread()
      .getStackTrace
      .filter(_.getClassName.startsWith("org.scalatest."))
      .exists(e => e.getClassName.contains("NotWord") || e.getMethodName == "shouldNot")
    val r = if (isShouldNot) {
      shouldNotFn()
      false
    } else {
      shouldFn()
      true
    }
    MatchResult(r, "", "")
  }

  private def makeFn[T](f: JIterable[T] => Unit): SerializableFunction[JIterable[T], Void] =
    new SerializableFunction[JIterable[T], Void] {
      // delegate serialization to Kryo to avoid serialization issues in tests
      // when a non-serializable object is captured by the closure
      private[this] val impl = Externalizer(f)

      override def apply(input: JIterable[T]): Void = {
        impl.get(input)
        null
      }
    }

  private def makeFnSingle[T](f: T => Unit): SerializableFunction[T, Void] =
    new SerializableFunction[T, Void] {
      // delegate serialization to Kryo to avoid serialization issues in tests
      // when a non-serializable object is captured by the closure
      private[this] val impl = Externalizer(f)

      override def apply(input: T): Void = {
        impl.get(input)
        null
      }
    }

  // Due to  https://github.com/GoogleCloudPlatform/DataflowJavaSDK/issues/434
  // SerDe cycle on each element to keep consistent with values on the expected side
  private def serDeCycle[T: Coder](scollection: SCollection[T]): SCollection[T] = {
    val coder = scollection.internal.getCoder
    scollection
      .map(e =>
        CoderUtils
          .decodeFromByteArray(coder, CoderUtils.encodeToByteArray(coder, e))
      )
  }

  /**
   * SCollection assertion only applied to the specified window,
   * running the checker only on the on-time pane for each key.
   */
  def inOnTimePane[T: ClassTag](window: BoundedWindow)(matcher: MatcherBuilder[T]): Matcher[T] =
    matcher match {
      case value: SingleMatcher[T, _] =>
        value.matcher(_.inOnTimePane(window))
      case value: IterableMatcher[T, _] =>
        value.matcher(_.inOnTimePane(window))
    }

  /** SCollection assertion only applied to the specified window. */
  def inWindow[T: ClassTag, B: ClassTag](
    window: BoundedWindow
  )(matcher: IterableMatcher[T, B]): Matcher[T] =
    matcher.matcher(_.inWindow(window))

  /**
   * SCollection assertion only applied to the specified window across
   * all panes that were not produced by the arrival of late data.
   */
  def inCombinedNonLatePanes[T: ClassTag, B: ClassTag](
    window: BoundedWindow
  )(matcher: IterableMatcher[T, B]): Matcher[T] =
    matcher.matcher(_.inCombinedNonLatePanes(window))

  /**
   * SCollection assertion only applied to the specified window,
   * running the checker only on the final pane for each key.
   */
  def inFinalPane[T: ClassTag, B: ClassTag](
    window: BoundedWindow
  )(matcher: MatcherBuilder[T]): Matcher[T] =
    matcher match {
      case value: SingleMatcher[T, _] =>
        value.matcher(_.inFinalPane(window))
      case value: IterableMatcher[T, _] =>
        value.matcher(_.inFinalPane(window))
    }

  /**
   * SCollection assertion only applied to the specified window.
   * The assertion expect outputs to be produced to the provided window exactly once.
   */
  def inOnlyPane[T: ClassTag, B: ClassTag](
    window: BoundedWindow
  )(matcher: SingleMatcher[T, B]): Matcher[T] =
    matcher.matcher(_.inOnlyPane(window))

  /** SCollection assertion only applied to early timing global window. */
  def inEarlyGlobalWindowPanes[T: ClassTag, B: ClassTag](
    matcher: IterableMatcher[T, B]
  ): Matcher[T] =
    matcher.matcher(_.inEarlyGlobalWindowPanes)

  /** Assert that the SCollection in question contains the provided elements. */
  def containInAnyOrder[T: Coder: Eq](
    value: Iterable[T]
  ): IterableMatcher[SCollection[T], TestWrapper[T]] =
    new IterableMatcher[SCollection[T], TestWrapper[T]] {
      override def matcher(builder: AssertBuilder): Matcher[SCollection[T]] =
        new Matcher[SCollection[T]] {
          val v = Externalizer(value.toSeq.map(x => TestWrapper(x))) // defeat closure
          override def apply(left: SCollection[T]): MatchResult = {
            val matcher = {
              val items = v.get.mkString("\n\t\t", "\n\t\t", "\n")
              def message = s"Expected: iterable with items [$items]"
              val c = Matchers.containsInAnyOrder(v.get: _*)
              Matchers.describedAs(message, c, v.get: _*)
            }

            val f = makeFn[TestWrapper[T]](in => assertThat(in, Matchers.not(matcher)))
            val g = makeFn[TestWrapper[T]](in => assertThat(in, matcher))

            val assertion = builder(PAssert.that(serDeCycle(TestWrapper.wrap(left)).internal))
            m(
              () => assertion.satisfies(g),
              () => assertion.satisfies(f)
            )
          }
        }
    }

  /** Assert that the SCollection in question contains a single provided element. */
  def containSingleValue[T: Coder: Eq](value: T): SingleMatcher[SCollection[T], TestWrapper[T]] =
    new SingleMatcher[SCollection[T], TestWrapper[T]] {
      override def matcher(builder: AssertBuilder): Matcher[SCollection[T]] =
        new Matcher[SCollection[T]] {
          override def apply(left: SCollection[T]): MatchResult = {
            val assertion =
              builder(PAssert.thatSingleton(serDeCycle(TestWrapper.wrap(left)).internal))
            m(
              () => assertion.isEqualTo(TestWrapper[T](value)),
              () => assertion.notEqualTo(TestWrapper[T](value))
            )
          }
        }
    }

  /** Assert that the SCollection in question contains the provided element without making
   *  assumptions about other elements in the collection. */
  def containValue[T: Coder: Eq](value: T): IterableMatcher[SCollection[T], TestWrapper[T]] =
    new IterableMatcher[SCollection[T], TestWrapper[T]] {
      override def matcher(builder: AssertBuilder): Matcher[SCollection[T]] =
        new Matcher[SCollection[T]] {
          override def apply(left: SCollection[T]): MatchResult = {
            val v = Externalizer(TestWrapper[T](value)) // defeat closure
            val (should, shouldNot) = {
              (
                makeFn[TestWrapper[T]](in => assertThat(in, Matchers.hasItem(v.get))),
                makeFn[TestWrapper[T]](in => assertThat(in, Matchers.not(Matchers.hasItem(v.get))))
              )
            }
            val assertion = builder(PAssert.that(serDeCycle(TestWrapper.wrap(left)).internal))
            m(
              () => assertion.satisfies(should),
              () => assertion.satisfies(shouldNot)
            )
          }
        }
    }

  /** Assert that the SCollection in question is empty. */
  val beEmpty: IterableMatcher[SCollection[_], Any] =
    new IterableMatcher[SCollection[_], Any] {
      override def matcher(builder: AssertBuilder): Matcher[SCollection[_]] =
        new Matcher[SCollection[_]] {
          override def apply(left: SCollection[_]): MatchResult =
            m(
              () =>
                builder(PAssert.that(left.asInstanceOf[SCollection[Any]].internal))
                  .empty(),
              () =>
                builder(PAssert.that(left.asInstanceOf[SCollection[Any]].internal))
                  .satisfies(makeFn(in => assert(in.iterator().hasNext, "SCollection is empty")))
            )
        }
    }

  /** Assert that the SCollection in question has provided size. */
  def haveSize(size: Int): IterableMatcher[SCollection[_], Any] =
    new IterableMatcher[SCollection[_], Any] {
      override def matcher(builder: AssertBuilder): Matcher[SCollection[_]] =
        new Matcher[SCollection[_]] {
          override def apply(left: SCollection[_]): MatchResult = {
            val s = size // defeat closure
            val f = makeFn[Any] { in =>
              val inSize = in.asScala.size
              assert(inSize == s, s"SCollection expected size: $s, actual: $inSize")
            }
            val g = makeFn[Any] { in =>
              val inSize = in.asScala.size
              assert(inSize != s, s"SCollection expected size: not $s, actual: $inSize")
            }
            m(
              () =>
                builder(PAssert.that(left.asInstanceOf[SCollection[Any]].internal))
                  .satisfies(f),
              () =>
                builder(PAssert.that(left.asInstanceOf[SCollection[Any]].internal))
                  .satisfies(g)
            )
          }
        }
    }

  /** Assert that the SCollection in question is equivalent to the provided map. */
  def equalMapOf[K: Coder, V: Coder](
    value: Map[K, V]
  ): SingleMatcher[SCollection[(K, V)], JMap[K, V]] =
    new SingleMatcher[SCollection[(K, V)], JMap[K, V]] {
      override def matcher(builder: AssertBuilder): Matcher[SCollection[(K, V)]] =
        new Matcher[SCollection[(K, V)]] {
          override def apply(left: SCollection[(K, V)]): MatchResult =
            m(
              () =>
                builder(PAssert.thatMap(serDeCycle(left).toKV.internal))
                  .isEqualTo(value.asJava),
              () =>
                builder(PAssert.thatMap(serDeCycle(left).toKV.internal))
                  .notEqualTo(value.asJava)
            )
        }
    }

  // TODO: investigate why multi-map doesn't work

  /** Assert that the SCollection in question satisfies the provided function. */
  def satisfy[T: Coder: Eq](
    predicate: Iterable[T] => Boolean
  ): IterableMatcher[SCollection[T], TestWrapper[T]] =
    new IterableMatcher[SCollection[T], TestWrapper[T]] {
      override def matcher(builder: AssertBuilder): Matcher[SCollection[T]] =
        new Matcher[SCollection[T]] {
          override def apply(left: SCollection[T]): MatchResult = {
            val cleanedPredicate = ClosureCleaner.clean(predicate)
            val p: Iterable[TestWrapper[T]] => Boolean = it => cleanedPredicate(it.map(_.value))
            val f = makeFn[TestWrapper[T]](in => assert(p(in.asScala)))
            val g = makeFn[TestWrapper[T]](in => assert(!p(in.asScala)))
            val assertion = builder(PAssert.that(serDeCycle(TestWrapper.wrap(left)).internal))
            m(
              () => assertion.satisfies(f),
              () => assertion.satisfies(g)
            )
          }
        }
    }

  /**
   * Assert that the SCollection in question contains a single element which satisfies the
   * provided function.
   */
  def satisfySingleValue[T: Coder: Eq](
    predicate: T => Boolean
  ): SingleMatcher[SCollection[T], TestWrapper[T]] =
    new SingleMatcher[SCollection[T], TestWrapper[T]] {
      override def matcher(builder: AssertBuilder): Matcher[SCollection[T]] =
        new Matcher[SCollection[T]] {
          override def apply(left: SCollection[T]): MatchResult = {
            val cleanedPredicate = ClosureCleaner.clean(predicate)
            val p: TestWrapper[T] => Boolean = t => cleanedPredicate(t.value)
            val f = makeFnSingle[TestWrapper[T]](in => assert(p(in)))
            val g = makeFnSingle[TestWrapper[T]](in => assert(!p(in)))
            val assertion = builder(
              PAssert.thatSingleton(serDeCycle(TestWrapper.wrap(left)).internal)
            )
            m(
              () => assertion.satisfies(f),
              () => assertion.satisfies(g)
            )
          }
        }
    }

  /** Assert that all elements of the SCollection in question satisfy the provided function. */
  def forAll[T: Coder: Eq](
    predicate: T => Boolean
  ): IterableMatcher[SCollection[T], TestWrapper[T]] = {
    val f = ClosureCleaner.clean(predicate)
    satisfy(_.forall(f))
  }

  /** Assert that some elements of the SCollection in question satisfy the provided function. */
  def exist[T: Coder: Eq](
    predicate: T => Boolean
  ): IterableMatcher[SCollection[T], TestWrapper[T]] = {
    val f = ClosureCleaner.clean(predicate)
    satisfy(_.exists(f))
  }
}
