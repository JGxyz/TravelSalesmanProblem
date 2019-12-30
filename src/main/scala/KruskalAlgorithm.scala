import java.util

import scala.collection.mutable

object KruskalAlgorithm {
  def minimumSpannigTreeWeight(graph: Array[Array[Int]], nodes: Array[Int]): Int = {
    var minWeight = 0
    val priorityQueue = mutable.PriorityQueue[(Int, Int, Int)]()(Ordering.by[(Int, Int, Int), Int](_._3).reverse)
    val unionFind = UnionFind(nodes.size)
    val nodesMap = nodes.zipWithIndex.toMap
    val edges = new util.LinkedList[(Int, Int, Int)]()

    nodes.foreach(r =>
      nodes.foreach(c =>
        if (c > r) priorityQueue.addOne((r, c, graph(r)(c)))));

    priorityQueue.dequeueAll[(Int, Int, Int)].foreach {
      case (u, v, weight) if !unionFind.connected(nodesMap(u), nodesMap(v)) => {
        minWeight += weight
        edges.add((u, v, weight))
        unionFind.union(nodesMap(u), nodesMap(v))
      }
      case _ => ()
    }

    minWeight
  }
}
