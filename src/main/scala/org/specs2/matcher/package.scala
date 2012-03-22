package org.specs2

package object matcher {

  type IsEmpty[T] = T => Any { def isEmpty: Boolean }
  type IsOrdered[T] = T => Ordered[T]
  
  import MustExpectations._
  import StringMatchers._

  implicit def ToReturns[T](t: =>MatchResult[T]): Returns[T] = new Returns(t)
  class Returns[T](t: =>MatchResult[T]) {
    def returns(m: String) = t must contain(m) ^^ { (m: MatchResult[T]) => m.message }
  }
}