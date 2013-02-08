package org.specs2
package matcher

import scala.util.{ Success => TSuccess, Failure => TFailure, Try }

class TryMatchersSpec extends Specification { def is =
                                                                                                                        """
  The TryMatchers trait provides matchers to check capital-T Try instances.
                                                                                                                        """^
                                                                                                                        p^
  "beTSuccess checks if an element is TSuccess(_)"                                                                      ^
  { TSuccess(1) must beTSuccess }                                                                                       ^
  { TSuccess(1) must beTSuccess(1) }                                                                                    ^
  { TSuccess(1) must beTSuccess.which(_ > 0) }                                                                          ^
  // { TSuccess(1) must beTSuccess.like { case a if a > 0 => ok } }                                                        ^
  // { TSuccess(1) must not be TSuccess(2) }                                                                               ^
  // { None must not be some }                                                                                             ^
  // { None must not be some(2) }                                                                                          ^
  //                                                                                                                       p^
  // "beNone checks if an element is None"                                                                                 ^
  // { None must beNone }                                                                                                  ^
  // { Some(1) must not be none }                                                                                          ^
  //                                                                                                                       p^
  // "beAsNoneAs checks if 2 values are None at the same time"                                                             ^
  // { None must beAsNoneAs(None) }                                                                                        ^
  // { Some(1) must beAsNoneAs(Some(2)) }                                                                                  ^
                                                                                                                        end
}
