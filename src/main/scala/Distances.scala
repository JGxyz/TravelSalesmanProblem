object Distances {
    val distances = Array(Array(Int.MaxValue,345,379,327,342,429,408,214,295,428,237,574,177,476),
    Array(345,Int.MaxValue,268,143,159,326,366,375,181,108,287,233,226,234),
    Array(379,268,Int.MaxValue,395,414,62,102,246,107,233,156,426,205,149),
      Array(327,143,395,Int.MaxValue,20,456,486,435,294,245,369,288,284,377),
      Array(342,159,414,20,Int.MaxValue,475,505,454,313,258,389,287,304,393),
      Array(429,326,62,456,475,Int.MaxValue,69,273,169,280,196,467,259,169),
      Array(408,366,102,486,505,69,Int.MaxValue,227,192,335,171,527,252,236),
      Array(214,375,246,435,454,273,227,Int.MaxValue,223,409,100,597,153,386),
      Array(295,181,107,294,313,169,192,223,Int.MaxValue,188,124,380,119,183),
      Array(428,108,233,245,258,280,335,409,188,Int.MaxValue,311,196,279,145),
      Array(237,287,156,369,389,196,171,100,124,311,Int.MaxValue,501,93,288),
      Array(574,233,426,288,287,467,527,597,380,196,501,Int.MaxValue,454,309),
      Array(177,226,205,284,304,259,252,153,119,279,93,454,Int.MaxValue,301),
      Array(476,234,149,377,393,169,236,386,183,145,288,309,301,Int.MaxValue))

  val cities = Array("Białystok", "Bydgoszcz", "Częstochowa", "Gdańsk", "Gdynia", "Katowice", "Kraków", "Lublin", "Łódź", "Poznań",
    "Radom", "Szczecin", "Warszawa", "Wrocław")

  def getMatrixForCities(selected: Array[Int]): Array[Array[Int]] = {
    val selectedRows = distances.zipWithIndex.filter{ case (_, index) => selected.contains(index)}.map(el => el._1)
    selectedRows.map(row => row.zipWithIndex.filter{case (_, index) => selected.contains(index)}.map(el => el._1))
  }

}
