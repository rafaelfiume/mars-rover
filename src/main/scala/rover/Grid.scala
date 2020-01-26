package rover

import scala.annotation.tailrec

object Grid {

  type Node = (Int, Int)
  type Edges = List[Node]
  type Grid = Seq[(Node, Edges)]

  def apply(rows: Int, columns: Int): Grid = for {
      r     <- 0 until rows
      c     <- 0 until columns
      left  = (r, if (c-1 < 0) columns-1 else c-1)
      up    = (if (r-1 < 0) rows-1 else r-1, c)
      down    = (if (r+1 >= rows) 0 else r+1, c)
      right = (r, if (c+1 >= columns) 0 else c+1)
    } yield (r, c) -> List(left, up, down, right)

  def shortestPath(start: Node, end: Node, grid: Grid): List[Node] = {
    type Parents = Map[Node, Node]

    def pathTo(node: Node, through: Parents): List[Node] = through.get(node) match {
      case None => List.empty
      case Some(parent) => parent :: pathTo(parent, through)
    }

    def nodesParent(nodes: List[Node], parent: Node): Parents = {
      nodes.foldLeft(Map.empty[Node, Node]) { case (parents, n) => parents.updated(n, parent) }
    }

    @tailrec
    def bdf(searched: List[Node],
            queue: List[Node],
            parents: Parents): Parents = queue match {
      case Nil => parents
      case x :: _ if x == end => parents
      case x :: xs =>
        val updatedSearched = x :: searched
        val nodeNeighbours = neighbours(x, grid).filterNot(n => updatedSearched.contains(n) || queue.contains(n))
        val neighboursParent = nodesParent(nodeNeighbours, parent = x)
        bdf(updatedSearched, xs ++ nodeNeighbours, parents ++ neighboursParent)
    }

    val neighboursFromStart = neighbours(start, grid)
    val startNeighboursParent = nodesParent(neighboursFromStart, parent = start)

    val parents = bdf(List(start), neighboursFromStart, startNeighboursParent)
    val pathToEnd = pathTo(end, through = parents)
    if (pathToEnd.isEmpty) List.empty else (end :: pathToEnd).reverse
  }

  def neighbours(node: Node, grid: Grid): List[Node] = grid
    .find { case (n, _) => n == node }
    .map (_._2)
    .getOrElse (List.empty)
}
