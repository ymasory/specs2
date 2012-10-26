package org.specs2
package control
import Throwablex._
import matcher._
import execute.{ResultExecution, Result, AsResult}
import specification.{Outside, BeforeAfterAround, Before}
import matcher.MatchPending

class IncludeExcludeStackTraceFilterSpec extends Specification with examples { def is =

  "A stacktrace can be filtered"                                                                                        ^
    "by declaring 'exclude' patterns"                                                                                   ^
      "leaving the elements not matching the patterns"                                                                  ! g1 .e1^
      "filtering out the elements matching the patterns"                                                                ! g1 .e2^
                                                                                                                        p^
    "by declaring 'include' patterns"                                                                                   ^
      "leaving the elements matching the patterns"                                                                      ! g1 .e3^
      "filtering out the elements not matching the patterns"                                                            ! g1 .e4^
                                                                                                                        endp^
  "A IncludedExcludedStackTraceFilter can be created from a string"                                                     ^
    "the default pattern is i1,i2/e1,e2 where i are include tags and e are exclude tags"                                ! g2 .e1^
                                                                                                                        p^
  "From an existing IncludedExcludedStackTraceFilter"                                                                   ^
    "we can add more include patterns, using the includeAlso method"                                                    ! g3 .e1^
    "we can add more exclude patterns, using the excludeAlso method"                                                    ! g3 .e2^
                                                                                                                        p^
  "A StackTraceFilter, when filtering an exception should"                                                              ^
    "retain the exception cause"                                                                                        ! g4 .e1^
    "retain the exception type"                                                                                         ! g4 .e2^
                                                                                                                        end
}

trait examples extends implementation with grouped {

  "patterns" - new g1 {
    println("creating it")
    e1 = filter(stacktrace("t1", "a", "com.t1.other"))      (excludeTrace("t1", "t2")) must not containMatch("t1")
    e2 = filter(stacktrace("t1", "t3", "a", "com.t1.other"))(excludeTrace("t1", "t2")) must containMatch("t3")
    e3 = filter(stacktrace("t1", "a", "com.t1.other"))      (includeTrace("t1", "t2")) must containMatch("t1")
    e4 = filter(stacktrace("t1", "t3", "a", "com.t1.other"))(includeTrace("t1", "t2")) must not containMatch("t3")
  }

  "from string" - new g2 {
    e1 = IncludeExcludeStackTraceFilter.fromString("i1,i2/e1,e2") must_==
         IncludeExcludeStackTraceFilter(Seq("i1", "i2"), Seq("e1", "e2"))
  }

  "from an existing filter" - new g3 {
    val defaultFilter = DefaultStackTraceFilter

    e1 = filter(stacktrace("org.specs2", "t1"))(defaultFilter.includeAlso("t1", "t2")) must not containMatch("specs2")
    e2 = filter(stacktrace("org.specs2", "t1"))(defaultFilter.excludeAlso("t1"))       must not containMatch("t1")
  }

  "exceptions" - new g4 {
    e1 = {
      val cause = new Exception("bang")
      DefaultStackTraceFilter.apply(new Exception("boom", cause)).getCause must_== cause
    }

    e2 = DefaultStackTraceFilter.apply(new IllegalArgumentException("ohnoes")).getClass.getName ===
        "java.lang.IllegalArgumentException"
  }
}

trait groups { outer =>

  trait g1 extends group {  }
  trait g2 extends group {  }
  trait g3 extends group {  }
  trait g4 extends group {  }

  var (g1, g2, g3, g4) = (() => new g1 {}: group,
                          () => new g2 {}: group,
                          () => new g3 {}: group,
                          () => new g4 {}: group)

  implicit def namedGroup(s: String): NamedGroup = new NamedGroup(s)
  class NamedGroup(s: String) {
    def -(g: =>group) =
      if      (g.isInstanceOf[g1]) { g1 = () => g }
      else if (g.isInstanceOf[g2]) { g2 = () => g }
      else if (g.isInstanceOf[g3]) { g3 = () => g }
      else if (g.isInstanceOf[g4]) { g4 = () => g }
  }
}

trait grouped { outer =>

  trait g1 extends group { outer.g1 = this }
  trait g2 extends group { outer.g2 = this }
  trait g3 extends group { outer.g3 = this }
  trait g4 extends group { outer.g4 = this }

  var (g1: group, g2: group, g3: group, g4: group) = (new g1 {}, new g2 {}, new g3 {}, new g4 {})

  implicit def namedGroup(s: String): NamedGroup = new NamedGroup(s)
  class NamedGroup(s: String) {
    def -(g: group) = g
  }
}

case class group(name: String = "") extends Expectations with Matchers with BeforeAfterAround {
  private val pending = MatchPending(" -- PENDING", Expectable(()))
  var e1: AnyAsResult = pending
  var e2: AnyAsResult = pending
  var e3: AnyAsResult = pending
  var e4: AnyAsResult = pending

  def before {}
  def after {}
  def around[T <% Result](a: =>T): Result = a

  implicit def anyToAnyResult[T : AsResult](t: =>T): AnyAsResult = AnyAsResult(() => apply(AsResult(t)))
}

case class AnyAsResult(t: () => Result)

object AnyAsResult {
  implicit def anyResultAsResult[T]: AsResult[AnyAsResult] = new AsResult[AnyAsResult] {
    def asResult(code: =>AnyAsResult): Result = code.t()
  }
}

trait implementation extends Specification {
  def stacktrace(st: String*) =  st.map(stackTraceElement(_))

  /** filter a stacktrace */
  def filter(stacktrace: Seq[StackTraceElement])(f: IncludeExcludeStackTraceFilter) =
    f(stacktrace).map(_.toString)
}
