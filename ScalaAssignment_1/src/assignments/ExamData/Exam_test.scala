package assignments

import assignments.Exam_data.result_calculator

object Exam_test extends App {

  val testList: List[Int] = List(10, 5, 50)
  val dataList = Data(testList(0), testList(1), testList(2))

  val result = result_calculator(dataList)
  println(result == "YES")
}
