import java.io.File

import org.scalatest.FunSuite
import com.github.tototoshi.csv._

import scala.io.Source

class TSPTest extends FunSuite {
  val resourcesPath = "src/main/resources"

  def createAdjacencyMatrix(edges: List[(Int, Int, Int)], size: Int): Array[Array[Int]] = {
    val adjacencyMatrix = Array.fill[Int](size, size)(Int.MaxValue)

    edges.foreach {
      case (u, v, weight) => {
        adjacencyMatrix(u)(v) = weight
        adjacencyMatrix(v)(u) = weight
      }
    }

    adjacencyMatrix
  }

  test("TSP example 1") {
      val size = 4
      val matrix = createAdjacencyMatrix(List((0, 1, 10), (0, 2, 15), (0, 3, 20),
        (1, 2, 35), (1, 3, 25),
        (2, 3, 30)), size)
      val tsp = TSP(size, matrix)
      val solution = tsp.solveTSP()
      val expected = (Array(0,1,3,2), 80)
      assert(solution._1.sameElements(expected._1) && solution._2 == expected._2)
    }

  test("TSP example 2 - 5 cities") {
      val reader = CSVReader.open(new File(resourcesPath+"/five_cities.csv"))
      val matrix: Array[Array[Int]] = reader.all().map(row => row.map(el => el.toDouble.toInt).toArray).toArray
      for(i <- 0 until matrix.size; j <- 0 until matrix.size; if matrix(i)(j) == 0)
        matrix(i)(j) = Int.MaxValue
      val size = 5
      val tsp = TSP(size, matrix)
      val solution = tsp.solveTSP()
      val expected = (Array(0,2,1,4,3),19)
      assert(solution._1.sameElements(expected._1) && solution._2 == expected._2)
  }

  test("TSP example 3 - 17 cities") {
    val reader = CSVReader.open(new File(resourcesPath+"/seventeen_cities.csv"))
    val matrix: Array[Array[Int]] = reader.all().map(row => row.map(el => el.toDouble.toInt).toArray).toArray
    for(i <- 0 until matrix.size; j <- 0 until matrix.size; if matrix(i)(j) == 0)
      matrix(i)(j) = Int.MaxValue
    val size = 17
    val tsp = TSP(size, matrix)
    val solution = tsp.solveTSP()
    val expected = (Array(0,3,12,6,7,5,16,13,14,2,10,9,1,4,8,11,15),2085)
    assert(solution._1.sameElements(expected._1) && solution._2 == expected._2)
  }
}
