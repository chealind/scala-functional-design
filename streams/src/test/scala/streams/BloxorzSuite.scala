package streams

import org.junit.Assert.{assertEquals, assertNotNull}
import org.junit._

class BloxorzSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal, "Invalid block position: b1=" + block.b1 + ", b2=" + block.b2) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level0 extends SolutionChecker {
    val level: String =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level: String =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


  @Test def `terrain function level 1 (10pts)`(): Unit =
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(4, 4)), "4,4")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }

  @Test def `find char level 1 (10pts)`(): Unit =
    new Level1 {
      assertEquals(Pos(1, 1), startPos)
      assertEquals(Pos(4, 7), goal)
    }

  @Test def `find neighbors with history`(): Unit =
    new Level1 {
      assertEquals(Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ), neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).to(Set))
    }

  @Test def `find new neighbors only`(): Unit =
    new Level1 {
      val neighbors: LazyList[(Block, List[Move])] = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ).to(LazyList)
      val explored = Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))

      val expected = Set((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))

      assertEquals(expected, newNeighborsOnly(neighbors, explored).to(Set))
    }

  @Test def `optimal solution for level 0`(): Unit =
    new Level0 {
      assertEquals(Block(goal, goal), solve(solution))
    }

  @Test def `optimal solution for level 1 (5pts)`(): Unit =
    new Level1 {
      assertEquals(Block(goal, goal), solve(solution))
    }


  @Test def `optimal solution length for level 1 (5pts)`(): Unit =
    new Level1 {
      assertEquals(optsolution.length, solution.length)
    }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(1000 * 1000)
}
