package rover

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import rover.Edges.printPath
import rover.Path.shortestPath

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
  "Grid apply" should "create a grid (adjacency list) with R x C size" in {
    val aGrid = Grid(rows = 3, columns = 5)

    aGrid shouldBe Vector(
      (0,0) -> List((0,4), (2,0), (1,0), (0,1)), (0,1) -> List((0,0), (2,1), (1,1), (0,2)), (0,2) -> List((0,1), (2,2), (1,2), (0,3)), (0,3) -> List((0,2), (2,3), (1,3), (0,4)), (0,4) -> List((0,3), (2,4), (1,4), (0,0)),
      (1,0) -> List((1,4), (0,0), (2,0), (1,1)), (1,1) -> List((1,0), (0,1), (2,1), (1,2)), (1,2) -> List((1,1), (0,2), (2,2), (1,3)), (1,3) -> List((1,2), (0,3), (2,3), (1,4)), (1,4) -> List((1,3), (0,4), (2,4), (1,0)),
      (2,0) -> List((2,4), (1,0), (0,0), (2,1)), (2,1) -> List((2,0), (1,1), (0,1), (2,2)), (2,2) -> List((2,1), (1,2), (0,2), (2,3)), (2,3) -> List((2,2), (1,3), (0,3), (2,4)), (2,4) -> List((2,3), (1,4), (0,4), (2,0))
    )
  }

  markup {"""
A Grid may have obstacles:
            |-------|-------|-------|
            | (0,0) | (0,1) | ##### |
            |-------|-------|-------|
            | (1,0) | (1,1) | ##### |
            |-------|-------|-------|
            | ##### | (2,1) | (2,2) |
            |-------|-------|-------|
  """ }
  it should "have no connections where there are obstacles" in {
    val aGridWithObstacles = Grid(rows = 3, columns = 3, obstacles = Some(List((2,0), (0,2), (1,2))))

    aGridWithObstacles shouldBe Vector(
      (0,0) -> List((1,0), (0,1)), (0,1) -> List((0,0), (2,1), (1,1)),
      (1,0) -> List((0,0), (1,1)), (1,1) -> List((1,0), (0,1), (2,1)),
                                   (2,1) -> List((1,1), (0,1), (2,2)), (2,2) -> List((2,1))
    )
  }

  "printPath" should "print the path from one node to the other" in {
    printPath(List((0,0), (0,4), (2,4))) shouldBe "(0,0) -> (0,4) -> (2,4)"
    printPath(List.empty) shouldBe ""
  }
}
