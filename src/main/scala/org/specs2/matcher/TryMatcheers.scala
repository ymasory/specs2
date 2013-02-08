package org.specs2
package matcher
import text.Quote._
import execute.{Result, Failure}

import scala.util.{ Success => TSuccess, Failure => TFailure, Try }

/**
 * Matchers for capital-T Trys.
 */
trait TryMatchers extends TryBaseMatchers //with TryBeHaveMatchers
object TryMatchers extends TryMatchers

private[specs2]
trait TryBaseMatchers {

  /* Success[A] */
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
  def beTSuccess[A] = new TSuccessMatcher[A]
  def tsuccess[A] = beTSuccess[A]

  /* Failure */
  // def beNone = new Matcher[Try[Any]] {
  //   def apply[S <: Try[Any]](value: Expectable[S]) = {
  //     result(value.value == None,
  //            value.description + " is None",
  //            value.description + " is not None",
  //            value)
  //   }
  // }
  // def none = beNone
  // def beAsNoneAs[A](other: =>Try[A]) = new Matcher[Try[A]] {
  //   def apply[S <: Try[A]](a: Expectable[S]) = {
  //     val b = other
  //     result(
  //       a.value == None && b == None || a.value != None && b != None,
  //       a.description + " is None as well",
  //       if (a.value == None) b + " is not None" else a.description + " is not None",
  //       a
  //     )
  //   }
  // }
  // def asNoneAs[A](other: =>Try[A]) = beAsNoneAs(other)
}
private[specs2]
trait TryBeHaveMatchers { outer: TryBaseMatchers =>

  implicit def toTryResultMatcher[A](result: MatchResult[Try[A]]) = new TryResultMatcher(result)
  class TryResultMatcher[A](result: MatchResult[Try[A]]) {

    /* Success */
    def beTSuccess = result(outer.beTSuccess)
    def beTSuccess(t: =>A) = result(outer.beTSuccess(t))

    /* Failure */
    // def beNone = result(outer.beNone)
    // def tsuccess = result(outer.beTSuccess)
    // def tsuccess(t: =>A) = result(outer.beTSuccess(t))
    // def none = result(outer.beNone)
    // def asNoneAs(other: =>Try[A]) = result(beAsNoneAs(other))
  }
}

private[specs2]
class TSuccessMatcher[A] extends Matcher[Try[A]] {
  def apply[S <: Try[A]](value: Expectable[S]) = {
    result(value.value.map(t => true).getOrElse(false),
           value.description + " is TSuccess[A]",
           value.description + " is not TSuccess[A]",
           value)
  }
  def which(f: A => Boolean) = this ^^ { (t: Try[A]) => t filter f }
  def like(f: PartialFunction[A, MatchResult[_]]) = this and partialMatcher(f)

  private def partialMatcher(f: PartialFunction[A, MatchResult[_]]) = new Matcher[Try[A]] {
    def apply[S <: Try[A]](value: Expectable[S]) = {
      val res: Result = value.value match {
        case TSuccess(t) if f.isDefinedAt(t)  => f(t).toResult
        case TSuccess(t) if !f.isDefinedAt(t) => Failure("function undefined")
        case None                         => Failure("no match")
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
