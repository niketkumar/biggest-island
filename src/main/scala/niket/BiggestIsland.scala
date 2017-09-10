package niket

import scala.collection.mutable

object BiggestIsland extends App {

  val matrix1 = Array(
    Array(0, 0, 1, 0, 1),
    Array(0, 1, 0, 1, 1),
    Array(0, 0, 0, 1, 0),
    Array(0, 0, 0, 0, 0),
    Array(1, 0, 0, 0, 0),
    Array(1, 0, 1, 0, 0)
  )

  val isls: mutable.Set[mutable.Set[Cell]] = islands(matrix1)
  println(s"Islands: $isls")
  println(s"Bridge Cell: ${bridgeCell(isls, matrix1)}")

  case class Cell(row: Int, col: Int) {
    def isValid(input: Array[Array[Int]]): Boolean = row >= 0 && row < input.length && col >= 0 && col < input.head.length

    def value(input: Array[Array[Int]]): Int = input(row)(col)
  }

  def loadIsland(island: mutable.Set[Cell], cell: Cell, input: Array[Array[Int]], visited: mutable.Set[Cell]): Unit = {
    island.add(cell)
    visited.add(cell)
    for (one <- nextOnes(cell, input, visited)) loadIsland(island, one, input, visited)
  }

  def nextOnes(cell: Cell, input: Array[Array[Int]], visited: mutable.Set[Cell]): List[Cell] = {
    List(
      Cell(cell.row + 1, cell.col),
      Cell(cell.row - 1, cell.col),
      Cell(cell.row, cell.col - 1),
      Cell(cell.row, cell.col + 1),
      Cell(cell.row + 1, cell.col - 1),
      Cell(cell.row + 1, cell.col + 1),
      Cell(cell.row - 1, cell.col - 1),
      Cell(cell.row - 1, cell.col + 1)
    )
      .filter(c => c.isValid(input) && c.value(input) == 1 && !visited.contains(c))
      .foldLeft(List[Cell]()) {
        case (ones, c) => c :: ones
      }
  }

  def islands(input: Array[Array[Int]]) = {
    val visited = mutable.Set[Cell]()
    val result = mutable.Set[mutable.Set[Cell]]()
    for (row <- input.indices) {
      for (col <- input.head.indices) {
        if (input(row)(col) == 1) {
          val cell = Cell(row, col)
          if (!visited.contains(cell)) {
            val island = mutable.Set[Cell]()
            loadIsland(island, cell, input, visited)
            result.add(island)
          }
        }
      }
    }
    result
  }

  def bridgeCell(islands: mutable.Set[mutable.Set[Cell]], input: Array[Array[Int]]): Cell = {
    val cellIslandMap = islands.foldLeft(Map[Cell, mutable.Set[Cell]]()) {
      case (map, island) => island.foldLeft(map) {
        case (m, cell) => m + (cell -> island)
      }
    }
    var bridge: Cell = null
    var biggestIsland = mutable.Set[Cell]()
    var dummy = mutable.Set[Cell]()
    for (row <- input.indices) {
      for (col <- input.head.indices) {
        if (input(row)(col) == 0) {
          val zero = Cell(row, col)
          val ones = nextOnes(zero, input, dummy)
          val uniqueIslands: Seq[mutable.Set[Cell]] = ones.map(cell => cellIslandMap(cell)).distinct
          if (uniqueIslands.size > 1) {
            val mergedIsland: mutable.Set[Cell] = uniqueIslands.reduce((is1, is2) => is1 ++ is2)
            if (mergedIsland.size > biggestIsland.size) {
              bridge = zero
              biggestIsland = mergedIsland
            }
          }
        }
      }
    }
    bridge
  }

}
