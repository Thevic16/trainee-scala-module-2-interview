import scala.annotation.tailrec

object PascalTriangle extends App {
  /*
   The following pattern of numbers is called Pascal’s triangle.
       1  0,0
      1 1  1,0 | 1,1
     1 2 1  2,0 | 2,1 | 2,2
    1 3 3 1 3,0 | 3,1 | 3,2 | 3,3
   1 4 6 4 1 4,0 | 4,1 | 4,2 | 4,3 | 4,4
   The numbers at the edge of the triangle are all 1, and each number inside
   the triangle is the sum of the two numbers above it. Write a function that computes
   the elements of Pascal’s triangle by means of a recursive process.
   Do this exercise by implementing the pascal function in Main.scala, which takes a
   column c and a row r, counting from 0 and returns the number at that spot in the triangle.
   For example, pascalRec(2,0)=1,pascalRec(2,1)=2 and pascalRec(3,1)=3
   */
  def pascalPrint(level: Int) = {
    for (row <- 0 to level) {
      for (col <- 0 to row)
        print(pascalRec(row, col) + " ")
      println()
    }
  }

  @tailrec
  def pascalRec(row: Int, col: Int, dic: List[List[Int]] = List(List(1), List(1,1))): Int = {

//    def go(row: Int, col: Int, acc1: Int, acc2: Int): Int = {
//      if (row == col || col == 0) acc1 + acc2 // los extremos
//      else go(row, col, acc1+1, acc2+1)
//    }

    //if (row == col || col == 0) 1 // los extremos
    //else pascalRec(row-1, col-1) + pascalRec(row-1, col)
    @tailrec
    def helper(l: List[Int], acc: List[Int]= List(1)): List[Int] = {
      if(l.size == 1) acc :+ 1
      else helper(l.tail, acc :+ l.head + l.tail.head)
    }

    if(dic.size > row) dic(row)(col)
    else pascalRec(row, col, dic :+ helper(dic.last))
  }
  pascalPrint(4)
}
