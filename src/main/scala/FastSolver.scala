package sdoku

object FastSolver extends Solver{
  def SolveSdoku(sdoku : SdokuCell) : SdokuCell = {
    val emptyList = Solver.MakeEmptyList(sdoku)
    val result = SolveSdoku1(sdoku, emptyList)
    println(result._1)
    result._2
  }
  
  def SolveSdoku1(sdoku : SdokuCell, emptyList : List[Coord]) : (Int, SdokuCell) = {
   if emptyList.isEmpty then (1, sdoku) else{
      val sdokuTemp = sdoku.map(_.clone)
      val availList = Solver.GetAvailableNumber(sdokuTemp, emptyList.head._1, emptyList.head._2)
      if availList.isEmpty then (0, Array[Array[Int]]()) else{
        availList.foldLeft(0, Array[Array[Int]]()) ((b, a) => {
          sdokuTemp(emptyList.head._1)(emptyList.head._2) = a
          val tmp = SolveSdoku1(sdokuTemp, emptyList.tail)
          if tmp._1 == 1 then tmp else b
          /*if b._1 + tmp._1 != 1 then (b._1 + tmp._1, Array[Array[Int]]()) else{
            if b._1 == 1 then b else tmp
          }*/
        })
      }
    }
  }
  
}
