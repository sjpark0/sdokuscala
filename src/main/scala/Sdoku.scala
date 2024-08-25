package sdoku

object Main extends App:
  //val sdoku = Solver.MakeSdoku
  val sdoku = Solver.TestSdoku
  Solver.PrintSdoku(sdoku)

  //val sdokuTemp : SdokuCell = Array.ofDim[Int](9, 9)
  //sdoku.copyToArray(sdokuTemp)
  /*val sdokuTemp = sdoku.map(_.clone)
  sdoku(0)(0) = 1
  sdoku(0)(2) = 8
  sdoku(0)(3) = 3

  Solver.PrintSdoku(sdoku)
  Solver.PrintSdoku(sdokuTemp)*/
  val solved = NaiveSolver.SolveSdoku(sdoku)
  Solver.PrintSdoku(solved)
  
  val solved1 = FastSolver.SolveSdoku(sdoku)
  Solver.PrintSdoku(solved1)
  /*sdoku(0)(0) = 1
  sdoku(0)(2) = 8
  sdoku(0)(3) = 3
  println(Solver.GetAvailableNumber(sdoku, 0, 4))*/
  val solved2 = FastSolver1.SolveSdoku(sdoku)
  Solver.PrintSdoku(solved2)
end Main