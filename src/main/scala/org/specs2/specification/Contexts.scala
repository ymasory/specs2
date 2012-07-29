package org.specs2
package specification

import execute.Result

/**
 * This trait is only used when we wish to write after actions in unit specifications like this
 *
 * "an example" ! {
 *   1 must_== 1
 * }.after(clean)
 */
trait Contexts {
  /**
   * add a before action to any kind of result
   */
  implicit def doBefore(t: =>Result) = new BeforeResult(t)
  class BeforeResult(t: =>Result) {
    def before(action: => Unit) = new Before {
      def before = action
    }.apply(t)
  }

  /**
   * add an Around function to any kind of result
   */
  implicit def doAround(t: =>Result) = new AroundResult(t)
  class AroundResult(t: =>Result) {
    def around(f: Result => Result) = new Around {
      def around(r: =>Result): Result = f(r)
    }.apply(t)
  }

  /**
   * add an after action to any kind of result
   */
  implicit def doAfter(t: =>Result) = new AfterResult(t)
  class AfterResult(t: =>Result) {
    def after(action: => Unit) = new After {
      def after = action
    }.apply(t)
  }

  protected[specs2] val defaultContext = new Context { def apply(a: =>Result): Result = a }

}

/**
 * Use this trait to deactivate the Contexts implicits
 */
trait NoContexts extends Contexts {
  override def doBefore(t: =>Result) = super.doBefore(t)
  override def doAround(t: =>Result) = super.doAround(t)
  override def doAfter(t: =>Result) = super.doAfter(t)
}

object Contexts extends Contexts