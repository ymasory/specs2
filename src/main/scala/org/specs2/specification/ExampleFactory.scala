package org.specs2
package specification

import execute.{AsResult, Result}
import matcher.MatchResult
import text.MarkupString
import control.Functions._

/**
 * this trait defines methods for creating Examples
 */
private[specs2]
trait ExampleFactory {
  /** @return an Example, using a function taking the example description as an input */
  def newExample[T : AsResult](s: String, function: String => T): Example = newExample(s, function(s))
  /** @return an Example, using anything that can be translated to a Result, e.g. a Boolean */
	def newExample[T : AsResult](s: String, t: =>T): Example = newExample(Example(s, t))
  /** @return an Example, using a function taking the example description as an input */
  def newExample(s: String, gt: GivenThen): Example = newExample(Example(RegexStep.strip(s), gt.extract(s)))
  /** @return an Example, using anything that can be translated to a Result, e.g. a Boolean */
  def newExample[T : AsResult](s: MarkupString, t: =>T): Example = newExample(Example(s, t))
  /** @return an Example, using anything that can be translated to a Result, e.g. a Boolean */
	def newExample(e: Example): Example
}

/**
 * Default implementation for the example factory trait just creating an Example object
 */
private[specs2]
class DefaultExampleFactory extends ExampleFactory {
  def newExample(e: Example): Example = e
}

/**
 * Decorated creation where the body of the example can be surrounded with a given context if necessary
 * @see mutable.BeforeExample
 */
private[specs2]
class DecoratedExampleFactory(factory: =>ExampleFactory, function: Context) extends ExampleFactory {
  override def newExample[T : AsResult](s: String, t: =>T) = factory.newExample(s, function(t)(toStrictFunction1(AsResult(_))))
  def newExample(e: Example) = e
}
