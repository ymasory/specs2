package org.specs2
package specification

import execute.{Executable, Result}

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
  implicit def doBefore[T : Executable](t: =>T) = new BeforeResult(t)
  class BeforeResult[T : Executable](t: =>T) {
    def before(action: => Unit) = new Before {
      def before = action
    }.apply(t)
  }

  /**
   * add an Around function to any kind of result
   */
  implicit def doAround[T : Executable](t: =>T) = new AroundResult(t)
  class AroundResult[T : Executable](t: =>T) {
    def around(f: Result => Result) = new Around {
      def around[R](r: =>R)(implicit conv: R => Result): Result = f(conv(r))
    }.apply(t)
  }

  /**
   * add an after action to any kind of result
   */
  implicit def doAfter[T : Executable](t: =>T) = new AfterResult(t)
  class AfterResult[T : Executable](t: =>T) {
    def after(action: => Unit) = new After {
      def after = action
    }.apply(t)
  }

  protected[specs2] val defaultContext = new Context { def apply[T : Executable](a: =>T): Result = a }

}

/**
 * Use this trait to deactivate the Contexts implicits
 */
trait NoContexts extends Contexts {
  override def doBefore[T : Executable](t: =>T) = super.doBefore(t)
  override def doAround[T : Executable](t: =>T) = super.doAround(t)
  override def doAfter[T : Executable](t: =>T) = super.doAfter(t)
}

object Contexts extends Contexts