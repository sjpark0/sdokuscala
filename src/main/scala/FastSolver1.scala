package sdoku

object FastSolver1 extends Solver{
  def SolveSdoku(sdoku : SdokuCell) : SdokuCell = {
    val emptyList = Solver.MakeEmptyList(sdoku)
    val assignList = Nil

    val result = SolveSdoku1(assignList, emptyList)
    Assign(sdoku, result._2)
  }
  
  def Assign(sdoku : SdokuCell, assignList : List[Coord]) : SdokuCell = {
    val sdokuTemp = sdoku.map(_.clone)
    assignList.foreach(coord => sdokuTemp(coord._1)(coord._2) = coord._5.head)
    sdokuTemp
  }
  
  def UpdateAvailList(assignElem : Coord, emptyList : List[Coord]) : List[Coord] = {
    emptyList.map(elem => {
      if elem._1 == assignElem._1 || elem._2 == assignElem._2 || (elem._3 == assignElem._3 && elem._4 == assignElem._4) then
        (elem._1, elem._2, elem._3, elem._4, elem._5.filter(_ != assignElem._5.head))
      else elem
    })
  }

  def AssignUniqueElem(assignList : List[Coord], resultEmptyList : List[Coord], emptyList : List[Coord]) : (List[Coord], List[Coord]) = {
    if emptyList.isEmpty then (assignList, resultEmptyList) else{
      val availList = emptyList.head._5
      if availList.isEmpty then (Nil, Nil) else {
        if availList.length == 1 then AssignUniqueElem(emptyList.head :: assignList, Nil, UpdateAvailList(emptyList.head, resultEmptyList ::: emptyList.tail))
        else AssignUniqueElem(assignList, emptyList.head :: resultEmptyList, emptyList.tail)
      }
    }
  }

  def SolveSdoku1(assignList : List[Coord], emptyList : List[Coord]) : (Int, List[Coord]) = {
    val (assignList1, emptyList1) = AssignUniqueElem(assignList, Nil, emptyList)

    if assignList1.isEmpty then (0, List[Coord]()) else {
      if emptyList1.isEmpty then (1, assignList1) else{
        val availList = emptyList1.head._5
        if availList.isEmpty then (0, List[Coord]()) else{
          availList.foldLeft(0, List[Coord]()) ((b, a) => {
            val assignElem : Coord = (emptyList1.head._1, emptyList1.head._2, emptyList1.head._3, emptyList1.head._4, List(a))
            val tmp = SolveSdoku1(assignElem :: assignList1, UpdateAvailList(assignElem, emptyList1.tail))
            if tmp._1 == 1 then tmp else b
          })
        }
      }
    }
    /*if emptyList1.isEmpty then (1, assignList1) else{
      val availList = emptyList1.head._5
      if availList.isEmpty then (0, List[Coord]()) else{
        availList.foldLeft(0, List[Coord]()) ((b, a) => {
          val assignElem : Coord = (emptyList1.head._1, emptyList1.head._2, emptyList1.head._3, emptyList1.head._4, List(a))
          val tmp = SolveSdoku1(assignElem :: assignList1, UpdateAvailList(assignElem, emptyList1.tail))
          if tmp._1 == 1 then tmp else b
        })
      }
    }*/
  }    
}