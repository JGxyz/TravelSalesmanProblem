import java.util.{InputMismatchException, Scanner}

object Application {
  def printRow(row: Array[Int]) = {
    row.foreach { case el if el != Int.MaxValue => print(el + " ")
    case _ => print(0 + " ")
    }
    println()
  }

  def printCities() = {
    println("Choose city: ")
    Distances.cities.zipWithIndex.foreach{case (city, index) => {
      println(index+" - "+city)
    }}
  }

  def main(args: Array[String]): Unit = {
    while(true) {
      val scanner = new Scanner(System.in)
      var num = -1
      val size = Distances.cities.size

      while (num == -1 || num < 3 || num > size) {
        try {
          println("Specify number of cities (3-" + size + "):")
          num = scanner.nextInt()
        } catch {
          case _: InputMismatchException => {}
        }
      }

      var matrix: Array[Array[Int]] = null
      val cities: Array[Int] = new Array(num)

      if (num < size) {
        val taken: Array[Boolean] = new Array(size)
        for (j <- 0 until size)
          taken(j) = false

        for (i <- 0 until num) {
          printCities()
          var chosen = -1
          while (chosen < 0 || chosen >= size || taken(chosen)) {
            println("Write number: ")
            try {
              chosen = scanner.nextInt()
            } catch {
              case _: InputMismatchException => {}
            }
          }
          taken(chosen) = true
          cities(i) = chosen
        }

        matrix = Distances.getMatrixForCities(cities)

        println("Chosen cities: ")
        cities.foreach(city => println(Distances.cities(city)))
      } else {
        Distances.cities.foreach(println)
        matrix = Distances.distances
      }


      val tsp = TSP(num, matrix)
      val solution = tsp.solveTSP()

      println("Mnimal Distances Sum: " + solution._2)

      println("Hamiltonian Cycle: ")
      if (num < size) {
        solution._1.foreach(i => {
          print(Distances.cities(cities(i)).toUpperCase() + " - ")
        })
        println(Distances.cities(cities(0)).toUpperCase())
      }
      else {
        solution._1.foreach(i => {
          print(Distances.cities(i).toUpperCase()+" - ")
        })
        println(Distances.cities(0).toUpperCase())
      }

    }
  }


}
