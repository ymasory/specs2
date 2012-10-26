package org.specs2
package control

import mutable.Specification
import Functions._
import execute._

class FunctionsSpec extends Specification {

  "a byname function can be transformed into a strict one" >> {
    def byNameFunction(u: =>Unit) {}
    var parameter = "not evaluated"
    toStrictFunction1(byNameFunction){ parameter = "evaluated" }

    "The byname function has become a strict one" <==> (parameter === "evaluated")
  }

  "functions can be or-ed with ||" >> {
    val f1: String => Boolean = (_:String).length < 3
    val f2: String => Boolean = (_:String).length < 5

    (f1 || f2)("abcdefg") must beFalse
    (f1 || f2)("abc")     must beTrue
    (f1 || f2)("abcd")    must beTrue
    (f2 || f1)("ab")      must beTrue
  }
  "functions can be and-ed with &&" >> {
    val f1: String => Boolean = (_:String).length < 3
    val f2: String => Boolean = (_:String).length < 5

    (f1 && f2)("abcdefg") must beFalse
    (f1 && f2)("abc")     must beFalse
    (f1 && f2)("abcd")    must beFalse
    (f2 && f1)("ab")      must beTrue
  }
  "functions can be negated with !" >> {
    val f1: String => Boolean = (_:String).length < 3

    (!f1)("abcdefg") must beTrue
    (!f1)("ab")      must beFalse
  }


  implicit def isEquivalent(description: String): IsEquivalent = new IsEquivalent(description)
  class IsEquivalent(description: String) {
    def ==>[T : AsResult](result: =>T) = <==>(result)
    def <==>[T : AsResult](result: =>T) = checkResultFailure {
      val r = ResultExecution.execute(AsResult(result))
      r match {
        case i if i.isError || i.isFailure => i.mapMessage(m => negate(description)+" because "+m)
        case other      => other.mapMessage(m => description+" <=> "+m)
      }
    }
  }

  def negate(description: String) =
    Map(
      "is"  -> "is not",
      "has" -> "has not",
      "have" -> "have not",
      "must" -> "must not",
      "should" -> "should not").
      foldLeft(description) { case (res, (oldValue, newValue)) =>
        res.replace(" "+oldValue+" ", " "+newValue+" ")
      }

}
