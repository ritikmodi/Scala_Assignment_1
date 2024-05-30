package assignments

import scala.io.Source

object Thief_data extends App {

  val scalaFile = Source.fromFile("src/assignments/ThiefData/thief.txt")
  val scalaFileContents = scalaFile.getLines()

  val datalist = for {
    line <- scalaFileContents
    //values = line.split(", ").toList
  } yield  line

  val lists = datalist.toList
  //println( lists )

  val toggles = for {
    list <- lists
    result = numberOfFlips(list)
  } yield  result

  val togglesList = toggles
  println(togglesList)

  def numberOfFlips(str: String): Int = {
    var count = 0
    var isOpen = true

    for(s <- str) {
      if((s == '0' && isOpen) || (s == '1' && !isOpen)) {
        count += 1
        isOpen = !isOpen
      }
    }
    count
  }

  def numberOfFlips2(str: String): Int = {
    var count = 0
    var previousState = str(0)
    if(previousState == '0')
      count += 1

    for(s <- str) {
      if(s != previousState) {
        count += 1
        previousState = s
      }
    }
    count
  }

  scalaFile.close()
}
