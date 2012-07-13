package org.specs2
package io

/**
 * Location of a Fragment in a file
 */
class Location {
  private val location = FromSource.location(fragmentFilter)
  def file: String = location.fileName
  def lineNumber: Int = location.lineNumber
  override def toString = location.fullLocation
  override def equals(a: Any) = a match {
    case l: Location => l.toString == this.toString
    case other       => false
  }

  private def isFragmentDefinition(s: String) =
    Seq("Described",
        "InExample"
  ).map("org.specs2.mutable.FragmentsBuilder$"+_).exists(s.contains)

  private def fragmentFilter = (st: Seq[StackTraceElement]) => {
    // for a mutable specification we drop the stacktrace until there are fragments
    // definitions then we drop the definition calls
    if (isMutableSpecification(st))
      st.dropWhile(s => !isFragmentDefinition(s.toString)).
         dropWhile(s => isFragmentDefinition(s.toString))
    else {
      val fragmentsMethods = Seq("org.specs2", ".textStart", ".textFragment", ".fragmentsFragments")
      val (start, end) = st.span(s => fragmentsMethods.exists(s.toString.contains))
      if (lastContains(start, "$up(FragmentsBuilder.scala:150")) takeHead(end, st(0), lineOffset = 1)
      else if (lastContains(start, "$up"))                       takeHead(end, st(0))
      else                                                       end  
    }
  }

  private def lastContains(st: Seq[StackTraceElement], method: String) = st.lastOption.map(_.toString.contains(method)).getOrElse(false)
  private def takeHead(st: Seq[StackTraceElement], defaultValue: StackTraceElement, lineOffset: Int = 0) = {
    val stElement = st.headOption.getOrElse(defaultValue)
    Seq(new StackTraceElement(stElement.getClassName, stElement.getMethodName, stElement.getFileName, stElement.getLineNumber+lineOffset))
  }

  private def isMutableSpecification(st: Seq[StackTraceElement]) = st.exists(_.toString.contains("org.specs2.mutable.FragmentsBuilder"))
}
