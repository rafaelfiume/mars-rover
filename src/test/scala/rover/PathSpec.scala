package rover

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import rover.Path.shortestPath

class PathSpec extends AnyFlatSpec with Matchers {

  private val aGrid = Grid(rows = 3, columns = 5)

  "shortestPath" should "find the shortest path from one point of the grid to the other" in {
    shortestPath(start = (0,0), end = (2,4), aGrid) shouldBe List((0,0), (0,4), (2,4))
    shortestPath(start = (1,4), end = (0,1), aGrid) shouldBe List((1,4), (0,4), (0,0), (0,1))
    shortestPath(start = (2,0), end = (1,1), aGrid) shouldBe List((2,0), (1,0), (1,1))
    shortestPath(start = (2,4), end = (2,1), aGrid) shouldBe List((2,4), (2,0), (2,1))
    shortestPath(start = (2,2), end = (0,4), aGrid) shouldBe List((2,2), (0,2), (0,3), (0,4))
  }

  it should "find shortest distance when there are obstacles and the target node is reachable" in {
    val grid = Grid(4, 3, Some(List((1,1), (1,2))))

    val result = shortestPath(start = (2,2), end = (0,1), grid)

    result shouldBe List((2,2), (2,1), (3,1), (0,1))
  }

  it should "return an empty path when there's no path to the target" in {
    shortestPath(start = (0,0), end = (8,8), aGrid) shouldBe empty
    shortestPath(start = (8,8), end = (0,0), aGrid) shouldBe empty
  }

}
