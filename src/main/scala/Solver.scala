package sdoku

type SdokuCell = Array[Array[Int]]
type Coord = (Int, Int, Int, Int, List[Int])
trait Solver {
  def SolveSdoku(sdoku : SdokuCell) : SdokuCell
  
}

object Solver{
  def MakeSdoku : SdokuCell = {
    val sdoku : SdokuCell = Array.ofDim[Int](9, 9)
    var bSuccess = false
    while(!bSuccess){
        for (i <- 0 until 9) {
            for (j <- 0 until 9) {
                sdoku(i)(j) = 0
            }
        }
    }
    sdoku
  }
  def TestSdoku : SdokuCell = {
    val sdoku : SdokuCell = Array.ofDim[Int](9, 9)
    sdoku(0)(1) = 9
    sdoku(0)(5) = 6
    sdoku(0)(8) = 5
    sdoku(1)(1) = 3
    sdoku(1)(3) = 4
    sdoku(1)(4) = 5
    sdoku(1)(7) = 8
    sdoku(2)(0) = 4
    sdoku(2)(5) = 2
    sdoku(3)(5) = 4
    sdoku(4)(0) = 3
    sdoku(4)(3) = 7
    sdoku(4)(4) = 9
    sdoku(4)(8) = 2
    sdoku(5)(1) = 8
    sdoku(5)(6) = 1
    sdoku(6)(0) = 7
    sdoku(6)(3) = 5
    sdoku(6)(4) = 3
    sdoku(6)(8) = 9
    sdoku(7)(4) = 6
    sdoku(8)(2) = 9
    sdoku(8)(7) = 2
    sdoku
  }
  def MakeEmptyList(sdoku : SdokuCell) : List[Coord] = {
    val coordList = (0 to 8).toList.flatMap(a => (0 to 8).toList.map(b => (a, b)).filter(a => sdoku(a._1)(a._2) == 0))
    coordList.map(coord => (coord._1, coord._2, (coord._1 / 3).toInt, (coord._2 / 3).toInt, GetAvailableNumber(sdoku, coord._1, coord._2)))
  }
  def GetAvailableNumber(sdoku : SdokuCell, i : Int, j : Int) : List[Int] = {
    val cellX = (j / 3).toInt * 3
    val cellY = (i / 3).toInt * 3
    val availListX = (0 to 8).toList.map(a => (i, a)).filter(a => j != a._2)
    val availListY = (0 to 8).toList.map(a => (a, j)).filter(a => i != a._1)
    val availList = (0 to 2).toList.flatMap(a => (0 to 2).toList.map(b => (cellY + a, cellX + b))).filter(a => i != a._1 || j != a._2)
    (1 to 9).toList.filter(aa => (availListX ::: availListY ::: availList).foldLeft(true)((b, a) => b && sdoku(a._1)(a._2) != aa))
  }
  def PrintSdoku(sdoku : SdokuCell) : Unit = {
  for (i <- 0 until 9) {
      for (j <- 0 until 9) {
        print(sdoku(i)(j).toString() + " ")
      }
      println()
    }
  }
}