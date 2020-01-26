package rover

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
}
