package org.specs2

package object execute {
  type Executable[T] = T => Result
}
