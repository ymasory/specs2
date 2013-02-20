package org.specs2
package matcher
import text.Quote._
import execute.{Result, Failure}

import scala.util.{ Success => TSuccess, Failure => TFailure, Try }

/**
 * Matchers for capital-T Trys.
 */
trait TryMatchers extends TryBaseMatchers with TryBeHaveMatchers
object TryMatchers extends TryMatchers

private[specs2]
trait TryBaseMatchers {

  /* Success[A] */
  def beTSuccess[A] = new TSuccessMatcher[A]
  def beTSuccess[A](t: =>A) = new Matcher[Try[A]] {
    def apply[S <: Try[A]](value: Expectable[S]) = {
      val expected = t
      result(value.value == TSuccess(t), 
             value.description + " is TSuccess with value " + q(expected),
             value.description + " is not TSuccess with value " + q(expected),
             value)
    }
  }

  def tsuccess[A](t: =>A) = beTSuccess(t)
  def tsuccess[A] = beTSuccess[A]

  /* Failure */

  /* both Succeed or Fail */
}
private[specs2]
trait TryBeHaveMatchers { outer: TryBaseMatchers =>

  implicit def toTryResultMatcher[A](result: MatchResult[Try[A]]) = new TryResultMatcher(result)

  class TryResultMatcher[A](result: MatchResult[Try[A]]) {

    def beTSuccess = result(outer.beTSuccess)
    def beTSuccess(t: =>A) = result(outer.beTSuccess(t))

    def tsuccess = result(outer.beTSuccess)
    def tsuccess(t: =>A) = result(outer.beTSuccess(t))
  }
}

private[specs2]
class TSuccessMatcher[A] extends Matcher[Try[A]] {

  def apply[S <: Try[A]](value: Expectable[S]) = {
    result(
      value.value.map(t => true).getOrElse(false),
      value.description + " is TSuccess[A]",
      value.description + " is not TSuccess[A]",
      value
    )
  }

  def which(f: A => Boolean) = this ^^ { (t: Try[A]) => t filter f }

  def like(f: PartialFunction[A, MatchResult[_]]) = this and partialMatcher(f)

  private def partialMatcher(f: PartialFunction[A, MatchResult[_]]) = new Matcher[Try[A]] {
    def apply[S <: Try[A]](value: Expectable[S]) = {
      val res: Result = value.value match {
        case TSuccess(t) if f.isDefinedAt(t)  => f(t).toResult
        case TSuccess(t) if !f.isDefinedAt(t) => Failure("function undefined")
        case TFailure(_)                      => Failure("no match")
      }
      result(
        res.isSuccess,
        value.description + " is TSuccess[A] and " + res.message,
        value.description + " is TSuccess[A] but " + res.message,
        value
      )
    }
  }
}
