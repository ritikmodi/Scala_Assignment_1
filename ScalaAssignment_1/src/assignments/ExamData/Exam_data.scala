package assignments

import scala.io.Source

case class Data(k: Int, l: Int, m: Int)

object Exam_data extends App {

  val source = Source.fromFile("src/assignments/ExamData/exam.csv")
  val sourceLines = source.getLines()
//  val finalList = sourceLines.map(_.split("\t")).toArray.foreach(_.foreach(_.split(",")))

//  println(finalList)
//  source.close()

//  val scalaFile = Source.fromFile("src/Assignments/exam.txt")
//  val scalaFileContents = scalaFile.getLines()

  val listOfData = sourceLines.map { line =>
    val values = line.trim.split(",").toList
    Data(values(0).toInt, values(1).toInt, values(2).toInt)
  }.toList

  val all_results = for {
    list <- listOfData
    result = result_calculator(list)
  } yield  result

  val result_list = all_results
  println(result_list)

  def result_calculator(data: Data): String =
    if (data.k * data.l <= data.m) "YES"
    else "NO"

  source.close()

}









//package assignments
//
//import scala.io.Source
//
//case class Data(k: Int, l: Int, m: Int)
//
//object Exam_data extends App {
//
//  val scalaFile = Source.fromFile("src/Assignments/exam.txt")
//  val scalaFileContents = scalaFile.getLines()
//
//  val datalist = for {
//    line <- scalaFileContents
//    values = line.trim.split(", ").toList
//  } yield values
//
//  val lists = datalist.toList
//  println(lists)
//
//  val all_results = for {
//    list <- lists
//    result = result_calculator(list.head.toInt, list.tail.head.toInt, list.tail.tail.head.toInt)
//  } yield  result
//
//  val result_list = all_results.toList
//  println(result_list)
//
//  def result_calculator(k: Int, l: Int, m: Int): String =
//    if (k*l <= m) "YES"
//    else "NO"
//
//  scalaFile.close()
//
//}
