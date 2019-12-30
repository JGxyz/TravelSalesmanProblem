case class TSP(size: Int, graph: Array[Array[Int]]) {

  var minWeight: Int = Int.MaxValue
  var solution: Array[Int] = new Array[Int](size)
  var visited: Array[Boolean] = Array.fill[Boolean](size)(false)
  var partialSolution: Array[Int] = new Array[Int](size)

  def solveTSP(): (Array[Int], Int) = {
    visited(0) = true
    helpTSP(0, 1, 0)
    (solution, minWeight)
  }

  private def getUnvisited(counter: Int): Array[Int] = {
    (0 until size).filter(index => !visited(index) || partialSolution(0) == index || partialSolution(counter - 1) == index).toArray
  }

  private def helpTSP(node: Int, counter: Int, currentWeight: Int): Unit = {
    if (counter == size) {
      val lastWeight = graph(0)(partialSolution(counter - 1))
      if (lastWeight < Int.MaxValue) {
        if (currentWeight + lastWeight < minWeight) {
          solution = partialSolution.clone()
          minWeight = currentWeight + lastWeight
        }
      }
    } else {
      val constraint = KruskalAlgorithm.minimumSpannigTreeWeight(graph, getUnvisited(counter))
      (0 until size).filter(vertex => !visited(vertex) && graph(vertex)(node) < Int.MaxValue).foreach(vertex =>
        if (constraint >= minWeight) return
        else {
          visited(vertex) = true
          partialSolution(counter) = vertex
          helpTSP(vertex, counter + 1, currentWeight + graph(vertex)(node))
          visited(vertex) = false
        }
      )
    }
  }
}
