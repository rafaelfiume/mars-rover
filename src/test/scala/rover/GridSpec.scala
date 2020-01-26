package rover

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import rover.Grid._

class GridSpec extends AnyFlatSpec with Matchers {

  markup {"""
Rover will always try to turn 'left' first, then 'up', 'down' and, finally, 'right'.

|-------|-------|-------|-------|-------|
| (0,0) | (0,1) | (0,2) | (0,3) | (0,4) |
|-------|-------|-------|-------|-------|
| (1,0) | (1,1) | (1,2) | (1,3) | (1,4) |
|-------|-------|-------|-------|-------|
| (2,0) | (2,1) | (2,2) | (2,3) | (2,4) |
|-------|-------|-------|-------|-------|

  """ }
  private val aGrid = Grid(3, 5)

  "Grid apply" should "create a grid (adjacency list) with R x C size" in {
    aGrid shouldBe Vector(
      (0,0) -> List((0,4), (2,0), (1,0), (0,1)), (0,1) -> List((0,0), (2,1), (1,1), (0,2)), (0,2) -> List((0,1), (2,2), (1,2), (0,3)), (0,3) -> List((0,2), (2,3), (1,3), (0,4)), (0,4) -> List((0,3), (2,4), (1,4), (0,0)),
      (1,0) -> List((1,4), (0,0), (2,0), (1,1)), (1,1) -> List((1,0), (0,1), (2,1), (1,2)), (1,2) -> List((1,1), (0,2), (2,2), (1,3)), (1,3) -> List((1,2), (0,3), (2,3), (1,4)), (1,4) -> List((1,3), (0,4), (2,4), (1,0)),
      (2,0) -> List((2,4), (1,0), (0,0), (2,1)), (2,1) -> List((2,0), (1,1), (0,1), (2,2)), (2,2) -> List((2,1), (1,2), (0,2), (2,3)), (2,3) -> List((2,2), (1,3), (0,3), (2,4)), (2,4) -> List((2,3), (1,4), (0,4), (2,0))
    )
  }

  "shortestPath" should "find the shortest path from one point of the grid to the other" in {
    shortestPath(start = (0,0), end = (2,4), aGrid) shouldBe List((0,0), (0,4), (2,4))
    shortestPath(start = (1,4), end = (0,1), aGrid) shouldBe List((1,4), (0,4), (0,0), (0,1))
    shortestPath(start = (2,0), end = (1,1), aGrid) shouldBe List((2,0), (1,0), (1,1))
    shortestPath(start = (2,4), end = (2,1), aGrid) shouldBe List((2,4), (2,0), (2,1))
    shortestPath(start = (2,2), end = (0,4), aGrid) shouldBe List((2,2), (0,2), (0,3), (0,4))
  }

  it should "return an empty path when there's no path to the target" in {
    shortestPath(start = (0,0), end = (8,8), aGrid) shouldBe empty
    shortestPath(start = (8,8), end = (0,0), aGrid) shouldBe empty
  }

  "neighbours" should "return all the neighbours of a node" in {
    neighbours((1,2), aGrid) shouldBe List((1,1), (0,2), (2,2), (1,3))
  }

}
