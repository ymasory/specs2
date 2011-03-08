package org.specs2
package text
import java.lang.StringBuilder
/**
 * This trait does CamelCase transformations on names
 */
private[specs2]
trait CamelCase {

  implicit def camelCased(s: String) = new CamelCased(s)
  class CamelCased(s: String) {
    def camelCaseToWords = {
	  if (s.length >= 1)
	    s.drop(1).foldLeft(new StringBuilder(s.take(1).map(_.toLower))) { (res, cur) =>
          res.append(if (cur.isUpper) (" " + cur.toLower) else cur)
        }.toString
	  else s
	}
  }
}

private[specs2]
object CamelCase extends CamelCase