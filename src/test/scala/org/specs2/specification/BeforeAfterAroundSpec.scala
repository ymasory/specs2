package org.specs2
package specification

import io.MockOutput
import execute.Result
import _root_.org.specs2.mutable.{Specification => Spec}

class BeforeAfterAroundSpec extends Specification with Grouped { def is =

 "The `Before/After/Around Example` traits are used to automatically insert contexts around examples bodies"            ^
 "a spec can define a Before context that is used for each example"                                                     ^
   "in a mutable spec"                                                                                                  ! g1.e1 ^
   "also in an acceptance spec"                                                                                         ! g1.e2 ^
   "defined on several fragments"                                                                                       ! g1.e3 ^
   "even for a mutable spec"                                                                                            ! g1.e4 ^
                                                                                                                        p^
 "a spec can define an After context that is used for each example"                                                     ! g2.e1 ^
 "a spec can define an Around context that is used for each example"                                                    ! g2.e2 ^
 "a spec can define a BeforeAfter context that is used for each example"                                                ! g2.e3 ^
 "a spec can define a BeforeAfterAround context that is used for each example"                                          ! g2.e4 ^
 "a spec can define a implicit context that is used for each example"                                                   ! g2.e5 ^
 "a mutable spec can define a implicit context that is used for each example"                                           ! g2.e6 ^
                                                                                                                        end

  "before" - new g1 {
    e1 := executeContains(
      new Spec with BeforeExample with MockOutput {
        def before { println("before") }
        "ex1" ! success
      }, "before")

    e2 := executeContains(
      new Specification with BeforeExample with MockOutput {
        def before { println("before") }
        def is = "ex1" ! success
      }, "before")

    e3 := executeContains(
      new Specification with MockOutput {
        object withBefore extends BeforeEach { def before { println("before") } }
        def is = withBefore(spec)
        def spec =
          "this should"     ^
            "ex1" ! success ^
            "ex2" ! success
      }, "before", "before")

    e4 := executeContains(
      new Spec with MockOutput {
        object withBefore extends BeforeEach { def before { println("before") } }
        override def is = withBefore(super.is)
        "ex1" ! { 1 must_== 2 }
      }, "before")
  }

  "other" - new g2 {
    e1 := executeContains(
    new Spec with AfterExample with MockOutput {
      def after { println("after") }
      "ex1" ! success
    },"after")

    e2 := executeContains(
      new Spec with AroundExample with MockOutput {
        def around[R <% Result](r: =>R) = { println("around"); r }
        "ex1" ! success
      },"around")

    e3 := executeContains(
      new Spec with BeforeAfterExample with MockOutput {
        def before { println("before") }
        def after { println("after") }
        "ex1" ! success
      },"before", "after")


    e4 := executeContains(
      new Spec with BeforeAfterAroundExample with MockOutput {
        def before { println("before") }
        def after { println("after") }
        def around[R <% Result](r: =>R) = { println("around"); r }
        "ex1" ! success
      }, "before", "around", "after")

    e5 := executeContains(
      new Specification with MockOutput {
        implicit val c: Context = new BeforeAfter {
          def before { println("before") }
          def after { println("after") }
        }
        def is = "ex1" ! { println("ex1"); ok }
      }, "before", "ex1", "after")

    e6 := executeContains(
      new Spec with MockOutput {
        implicit val c: Context = new BeforeAfter {
          def before { println("before") }
          def after { println("after") }
        }
        "ex1" in { println("ex1"); ok }
      }, "before", "ex1", "after")
  }

  def executeContains(s: SpecificationStructure with MockOutput, messages: String*) = {
    FragmentExecution.executeBodies(s.content).view.force
    s.messages must contain(messages.map(lazyfy(_)):_*).inOrder
  }

}