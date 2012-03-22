package org.specs2
package execute

/**
 * Trait for anything that can be executed to return a Result
 */
trait IsExecutable {
  /** @return a Result */
  def execute: Result
  /** modify the result to return */
  def map(f: Result => Result): IsExecutable = this
}
