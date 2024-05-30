package assignments

import assignments.Thief_data._

object Thief_test extends App {

//  println("Enter a string containing 0 and 1 only : ")
//  val str = scala.io.StdIn.readLine()
//
//  val result1 = numberOfFlips(str)
//  println("Number of Flips : " + result1)
//
//  val result2 = numberOfFlips2(str)
//  println("Number of Flips : " + result2)

  val str1 = "1010110001001001011010110010101011100"
  println(numberOfFlips2(str1) == 25)

  val str2 = "01101100101001110011010101011101101110101101101"
  println(numberOfFlips2(str2) == 32)

}
