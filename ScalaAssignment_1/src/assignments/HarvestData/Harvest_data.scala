package assignments

import scala.io.Source
import java.io.{File, FileReader}
import scala.collection.MapView

case class Price(fruit: String, date: String, price: Double)
case class Harvest(collector: String, date: String, fruit: String, amount: Double)

object Harvest_data extends App {

  val separator = "-"

  def earnings(harvestList: List[Harvest], priceList: List[Price]): List[(String, String, String, Double)] = {

    for {
      harvest <- harvestList
      price <- priceList
      if harvest.fruit == price.fruit && harvest.date == price.date
    } yield (harvest.collector, harvest.date, harvest.fruit, harvest.amount * price.price)
  }


  def bestCollectorQuantityByMonth(harvestList: List[Harvest]): MapView[String, String] = {
    val collectionsByMonth = harvestList.groupBy(entry => entry.date.substring(5, 7))

    val bestCollectorQuantityByMonth = collectionsByMonth.mapValues { group =>
      val collectedAmounts = group.groupBy(entry => entry.collector).mapValues(group => group.map(entry => entry.amount).sum)
      val maxCollector = collectedAmounts.maxBy { case (_, amount) => amount }
      maxCollector._1

    }
    bestCollectorQuantityByMonth
  }


  def bestCollectorByFruit(harvestList: List[Harvest]): MapView[String, String] = {

    val groupedCollectors = harvestList.groupBy(entry => entry.collector)

    val collectorFruitAmounts = groupedCollectors.mapValues { group =>
      val fruitAmounts = group.groupBy(entry => entry.fruit).mapValues(group => group.map(entry => entry.amount).sum)
      val maxFruitCollector = fruitAmounts.maxBy { case (_, amount) => amount }

      maxFruitCollector._1
    }
    collectorFruitAmounts
  }


  def bestCollectorByFruit_2(harvestList: List[Harvest]): MapView[String, String] = {

    val groupedCollectors = harvestList.groupBy(entry => entry.fruit)

    val collectorFruitAmounts = groupedCollectors.mapValues { group =>
      val fruitAmounts = group.groupBy(entry => entry.collector).mapValues(group => group.map(entry => entry.amount).sum)
      val maxFruitCollector = fruitAmounts.maxBy { case (_, amount) => amount }

      maxFruitCollector._1
    }
    collectorFruitAmounts
  }


  def bestProfitableFruitByMonth(earnings: List[(String, String, String, Double)]): MapView[String, String] = {
    val earningsByMonth = earnings.groupBy(entry => entry._2.split("-")(1))

    val bestFruitByMonth = earningsByMonth.mapValues { group =>
      val fruitEarnings = group.groupBy(entry => entry._3).mapValues(group => group.map(entry => entry._4).sum)
      val maxEarning = fruitEarnings.maxBy { case (_, earnings) => earnings }
      maxEarning._1

    }
    bestFruitByMonth
  }


  def bestProfitableFruitOverall(earnings: List[(String, String, String, Double)]): String = {
    val fruitEarnings = earnings.groupBy(entry => entry._3).mapValues(group => group.map(entry => entry._4).sum)
    val maxEarning = fruitEarnings.maxBy { case (_, earnings) => earnings }
    maxEarning._1
  }


  def leastProfitableFruitByMonth(earnings: List[(String, String, String, Double)]): MapView[String, String] = {
    val earningsByMonth = earnings.groupBy(entry => entry._2.split("-")(1))

    val leastFruitByMonth = earningsByMonth.mapValues { group =>
      val fruitEarnings = group.groupBy(entry => entry._3).mapValues(group => group.map(entry => entry._4).sum)
      val minEarning = fruitEarnings.minBy { case (_, earnings) => earnings }
      minEarning._1

    }
    leastFruitByMonth
  }


  def leastProfitableFruitOverall(earnings: List[(String, String, String, Double)]): String = {
    val fruitEarnings = earnings.groupBy(entry => entry._3).mapValues(group => group.map(entry => entry._4).sum)
    val minEarning = fruitEarnings.minBy { case (_, earnings) => earnings }
    minEarning._1
  }


  def collectorContributionsByMonth(earnings: List[(String, String, String, Double)]): MapView[String, String] = {

    val earningsByMonth = earnings.groupBy(entry => entry._2.split("-")(1))

    val bestCollectorByMonth = earningsByMonth.mapValues { group =>
      val collectorEarnings = group.groupBy(entry => entry._1).mapValues(group => group.map(entry => entry._4).sum)
      val maxEarning = collectorEarnings.maxBy { case (_, earnings) => earnings }
      maxEarning._1

    }
    bestCollectorByMonth
  }


  def collectorContributions(earnings: List[(String, String, String, Double)]): String = {

    val collectorEarnings = earnings.groupBy(entry => entry._1).mapValues(group => group.map(entry => entry._4).sum)
    val maxEarning = collectorEarnings.maxBy { case (_, earnings) => earnings }
    maxEarning._1

    //    val collectorContributions = earnings.groupBy(entry => entry._1).mapValues(group => group.map(entry => entry._4).sum)
    //    val sortedCollectors = collectorContributions.toList.sortBy { case (_, earnings) => -earnings }
    //    sortedCollectors
  }


  val harvest = Source.fromFile("src/assignments/HarvestData/harvest.csv")
  val harvest_input = harvest.getLines().drop(1)

  val harvest_datalist = harvest_input.map { line =>

    val values = line.trim.split(",").toList
    Harvest(values(0), values(1), values(2), values(3).toDouble)
  }.toList

//  harvest_datalist.foreach(println)

  //  val prices_filePath = "src/Assignments/prices.txt"
  //  val prices_input  = Source.fromFile( prices_filePath ).getLines()

  val prices = Source.fromFile("src/assignments/HarvestData/prices.csv")
  val prices_input = prices.getLines().drop(1)
  //  sourceLines.map(_.split("\t")).toArray.foreach(_.foreach(println))


  val prices_datalist = prices_input.map { line =>
    val values = line.trim.split(",").toList
    Price(values(0), values(1), values(2).toDouble)
  }.toList

//    prices_datalist.foreach(println)


  val total_earnings = earnings(harvest_datalist, prices_datalist)
  //  println(total_earnings)

  //  total_earnings.foreach(println)


  val monthwise_bestCollectorByAmount = bestCollectorQuantityByMonth(harvest_datalist)
  println("Month-wise Best Collector By Amount ----> ")
  monthwise_bestCollectorByAmount.foreach(println)

  println()
  println()
  println()


  val bestCollectorByFruit = bestCollectorByFruit_2(harvest_datalist)
  println("Best Collector By Fruit ----> ")
  bestCollectorByFruit.foreach(println)

  println()
  println()
  println()

  val monthwise_bestProfitableFruit = bestProfitableFruitByMonth(total_earnings)
  println("Month-wise Best Profitable Fruit ----> ")
  monthwise_bestProfitableFruit.foreach(println)

  println()
  println()
  println()


  val overall_bestProfitableFruit = bestProfitableFruitOverall(total_earnings)
  println("Overall Best Profitable Fruit ----> " + overall_bestProfitableFruit)

  println()
  println()
  println()

  val monthwise_leastProfitableFruit = leastProfitableFruitByMonth(total_earnings)
  println("Month-wise Least Profitable Fruit ----> ")
  monthwise_leastProfitableFruit.foreach(println)

  println()
  println()
  println()

  val overall_leastProfitableFruit = leastProfitableFruitOverall(total_earnings)
  println("Overall Least Profitable Fruit ----> " + overall_leastProfitableFruit)

  println()
  println()
  println()

  val monthwise_bestCollector = collectorContributionsByMonth(total_earnings)
  println("Month-wise Best Collector ----> ")
  monthwise_bestCollector.foreach(println)

  println()
  println()
  println()


  val overall_bestCollector = collectorContributions(total_earnings)
  println("Overall Best Collector ----> " + overall_bestCollector)

  harvest.close()
  prices.close()

}








//  def readFile(path: String): List[List[String]] = {
//    val reader = CSVReader.open(new File(path))
//    val data = reader.all()
//    reader.close()
//    data
//  }


//  import org.apache.commons.csv._
//
//  def readFile(path: String): List[List[String]] = {
//    val reader = new CSVReader(new FileReader(path))
//    val data = reader.readAll()
//    reader.close()
//    data
//  }



//  val harvest_csv = readFile("harvest.csv").map(list => Harvest(list(1), list(2), list(3), list(4).toDouble))
//  println("Harvest Data from csv ----> ")
//  harvest_csv.foreach(println)
//
//
//  val prices_csv = readFile("prices.csv").map(list => Price(list(1), list(2), list(3).toDouble))
//  println("Prices Data from csv ----> ")
//  prices_csv.foreach(println)




//  val harvest_filePath = "src/Assignments/harvest.txt"
//  val harvest_input  = Sou"rce.fromFile( harvest_filePath ).getLines()
//
//  val harvest_datalist = for {
//    line <- harvest_input
//    values = line.split(" ").toList
//  } yield  values
//
////  println(datalist)
//
//  val harvest_lists = harvest_datalist.toList

//  println(harvest_lists.foreach(println))

//  println( harvest_lists )

//  harvest_lists.map(list => Harvest(list(0), list(1), list(2), list(3).toDouble))




//  val prices_filePath = "src/Assignments/prices.txt"
//  val prices_input  = Source.fromFile( prices_filePath ).getLines()
//
//  val prices_datalist = for {
//    line <- prices_input
//    values = line.split(" ").toList
//  } yield  values
//
//  //  println(prices_datalist)
//
//  val prices_lists = prices_datalist.toList
//  //  println( prices_lists )
//
////  println(prices_lists.foreach(println))
//
////  val string_lists = prices_lists.foreach(map(list => Price(list(0), list(1), list(2).toDouble)))
//
//  val string_lists = prices_lists.flatMap(list => list)
//
////  println(string_lists)
//
//  val separated_strings = string_lists.map(strings => strings.split(" "))
//
////  separated_strings.foreach(println)
//
//  val final_lists = separated_strings.map(strings => Price(strings(0), strings(1), strings(2).toDouble))
//
//  final_lists.foreach(println)


//  prices_lists.map(list => Price(list(0), list(1), list(2).toDouble))