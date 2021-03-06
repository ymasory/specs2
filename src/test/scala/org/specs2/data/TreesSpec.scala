package org.specs2
package data

import org.specs2.internal.scalaz._
import Trees._
import Scalaz.{node, leaf}
import text.Trim._
import matcher.DataTables
import specification.Grouped

class TreesSpec extends Specification with DataTables with Grouped { def is =

  "A tree can be pruned by providing a function mapping nodes to Option[Node]"                                          ^
    "if a node is mapped to Some(n), it stays in the tree"                                                              ! g1.e1^
    "if a node is mapped to None, it is removed from the tree"                                                          ^
      "if it's a leaf"                                                                                                  ! g1.e2^
      "if it's a subtree"                                                                                               ! g1.e3^
      "even the root of the tree"                                                                                       ! g1.e4^
                                                                                                                        endp^
  "A TreeLoc can"                                                                                                       ^
    "return its size"                                                                                                   ! g2.e1^
    "be added a new child"                                                                                              ! g2.e2^
                                                                                                                        p^
  "A Tree can be flattenLeft to avoid SOF"                                                                              ! g3.e1^
                                                                                                                        end

  "pruning" - new g1 {
    val prune = (i: Int) => if (i % 2 == 0) Some(i) else None

    e1 := pruneAndDraw(tree1, prune) must beTree(
      "0",
      "|",
      "`- 2",
      "   |",
      "   `- 2")

    e2 := pruneAndDraw(tree2, prune) must beTree(
      "0",
      "|",
      "`- 2")

    e3 := pruneAndDraw(tree3, prune) must beTree("0")
    e4 := pruneAndDraw(tree4, prune) must beTree("None")
  }

  "TreeLoc functions" - new g2 {
    e1 := {
     "tree"  | "size" |>
      tree   ! 6      |
      tree1  ! 3      |
      tree2  ! 3      |
      tree3  ! 3      | { (tree, size) => tree.loc.size must_== size }
    }
    // note that the TreeLoc stays at the root after the addition of the child node
    e2 := tree1.loc.addChild(3).tree.drawTree must beTree(
      "0",
      "|",
      "+- 2",
      "|  |",
      "|  `- 2",
      "|",
      "`- 3")
  }

  "Robustness" - new g3 {
    e1 := {
      val tree = tree3.loc.addChild(4).tree
      tree.flattenLeft.toSeq aka "flattenLeft" must_== tree.flatten.toSeq
    }
  }

 /**
  *  the tree is:
  *
  *  0
  *  |
  *  +- 2
  *  |  |
  *  |  `- 1
  *  |
  *  +- 3
  *  |  |
  *  |  `- 4
  *  |
  *  `- 5
  */
  def tree = node(0, node(2, leaf(1)) :: node(3, leaf(4)) :: leaf(5) :: Nil)

  def tree1 = node(0, node(2, leaf(2)))
  def tree2 = node(0, node(2, leaf(1)))
  def tree3 = node(0, node(1, leaf(2)))
  def tree4 = node(1, node(2, leaf(1)))
  def tree5 = node(0, Stream.cons(leaf(3), node(2, leaf(2))))


  def pruneAndDraw(tree: Tree[Int], f: Int => Option[Int]) = tree.prune(f).map(_.drawTree).getOrElse("None\n")
  def beTree(s: String*) = be_==(s.mkString("", "\n", "\n"))
  
  implicit def anyToStream[A](a: A): Stream[A] = Stream(a)
  implicit def listToStream[A](a: List[A]): Stream[A] = a.toStream
}
