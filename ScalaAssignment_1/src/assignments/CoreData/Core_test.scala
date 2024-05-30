package assignments

import assignments.Core_data._

object Core_test extends App {

//  println("Enter a number : ")
//  val number = scala.io.StdIn.readLine()
//
//  println("Enter repetitions : ")
//  val repetitions = scala.io.StdIn.readLine()

    val input = "9785 4"
    val Array(number, repetitions) = input.split(" ")
//    println(number)
//    println(repetitions)

    val core_digit_original = coreDigitNew(number)
//    println(core_digit_original)

    val final_number = core_digit_original * repetitions.toInt
    val result = coreDigitNew(final_number.toString)

//    println("Core Digit : " + result)
    println(result == 8)
}
