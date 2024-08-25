package sdoku

object FastSolver extends Solver{
  def SolveSdoku(sdoku : SdokuCell) : SdokuCell = {
    val emptyList = Solver.MakeEmptyList(sdoku)
    val result = SolveSdoku1(sdoku, emptyList)
    println(result._1)
    result._2
  }
  
  def RemoveTrivialCell(sdoku : SdokuCell, emptyList: List[Coord], res : List[Coord]) : (Option[SdokuCell], List[Coord]) = {
    if emptyList.isEmpty then (Some(sdoku), res) 
    else{
      val sdokuTemp = sdoku.map(_.clone)
      val availList = Solver.GetAvailableNumber(sdokuTemp, emptyList.head._1, emptyList.head._2)
      if availList.isEmpty then (None, Nil)
      else if availList.length == 1 then {
        sdokuTemp(emptyList.head._1)(emptyList.head._2) = availList.head
        RemoveTrivialCell(sdokuTemp, res ::: emptyList.tail, Nil)
      }
      else{
        RemoveTrivialCell(sdokuTemp, emptyList.tail, res ::: List(emptyList.head))
      }
    }
    
  }
  def SolveSdoku1(sdoku : SdokuCell, emptyList : List[Coord]) : (Int, SdokuCell) = {
    //val sdokuTemp = sdoku.map(_.clone)
    val (sdokuTemp, emptyListTemp) = RemoveTrivialCell(sdoku, emptyList, List())
    sdokuTemp match{
      case Some(s) => {
        if emptyListTemp.isEmpty then (1, s) 
        else{
          val availList = Solver.GetAvailableNumber(s, emptyListTemp.head._1, emptyListTemp.head._2)
          if availList.isEmpty then (0, Array[Array[Int]]()) else{
            availList.foldLeft(0, Array[Array[Int]]()) ((b, a) => {
              s(emptyListTemp.head._1)(emptyListTemp.head._2) = a
              val tmp = SolveSdoku1(s, emptyListTemp.tail)
              if tmp._1 == 1 then tmp else b
            })
          }
        }
      }
      case None => (0, Array[Array[Int]]())
    }
    
  }
  
}
