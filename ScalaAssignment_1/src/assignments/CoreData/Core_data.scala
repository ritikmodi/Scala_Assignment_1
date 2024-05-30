package assignments

import scala.io.Source

object Core_data extends App {

  val scalaFile = Source.fromFile("src/assignments/CoreData/CoreData.txt")
  val scalaFileContents = scalaFile.getLines()

  val data = for {
    line <- scalaFileContents
    //values = line.split(", ").toList
  } yield line

  val datalist = data.toList
  //println(datalist)

  val Array(num, repeat) = datalist.head.split(" ")

//  println(num)
//  println(repeat)

  val core_digit_original = coreDigitNew(num)
//  println(core_digit_original)

  val final_number = core_digit_original * repeat.toInt

  val final_result = coreDigitNew(final_number.toString)
  println(final_result)


  def coreDigitNew(number: String): Int = {

    def coreDigitNewHelper(list: List[Char], sum: Int): Int = {
      //println(sum)
      if (list.isEmpty && sum < 10) sum
      else if (list.isEmpty) coreDigitNewHelper(sum.toString.toList, 0)
      else coreDigitNewHelper(list.tail, sum + (list.head - '0'))

    }
    coreDigitNewHelper(number.toList, 0)
  }

  scalaFile.close()

}
