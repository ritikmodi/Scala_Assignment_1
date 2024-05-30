package assignments

import scala.annotation.tailrec

abstract class Comparator[T] {
  def compare(o1: T, o2: T): Int
}

abstract class MyList[+T] {

  def apply(index: Int): Option[T]
  def head: T
  def tail: MyList[T]
  def isEmpty: Boolean
  def size: Int
  def sort[A >:T] (comparator: Comparator[A]): MyList[A]
  def binarySearch[A >:T](comparator: Comparator[A], target: A): Option[Int]
  def printElements: String
}

object Empty extends MyList[Nothing] {

  override def apply(index: Int): Option[Nothing] = None
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyList[Nothing] = Empty
  def isEmpty: Boolean = true
  def size: Int = 0
  def sort[A >: Nothing] (comparator: Comparator[A]): MyList[A] = Empty
  def binarySearch[A >: Nothing](comparator: Comparator[A], target: A): Option[Int] = Option(-1)
  def printElements: String = ""
}

case class NonEmptyList[+T](h:T, t: MyList[T]) extends MyList[T] {

  def apply(index: Int): Option[T] = {
    if(index < 0) None
    else if(index == 0) Some(head)
    else t.apply(index - 1)
  }

  def head: T = h
  def tail: MyList[T] = t
  def isEmpty: Boolean = false

  def size: Int = {

    @tailrec
    def size_helper(mylist: MyList[T], length: Int): Int = {
      if (mylist.isEmpty) length
      else size_helper(mylist.tail, length + 1)
    }

    size_helper(this, 0)
  }

//  def sort(comparator: Comparator[T]): Seq[T] = ???

   def sort[A >:T] (comparator: Comparator[A]): MyList[A] = {

     def insert(x: A, mylist: MyList[A]): MyList[A] = {
       if (mylist.isEmpty) NonEmptyList(x, Empty)
       else if (comparator.compare(x, mylist.head) < 0) NonEmptyList(x, mylist)
       else NonEmptyList(mylist.head, insert(x, mylist.tail))
     }

     val sortedList = t.sort(comparator)
     insert(this.head, sortedList)
   }


////    def swap[T](mylist: Seq[T], i: Int, j: Int): Seq[T] = {
////      val temp = mylist(i)
////      mylist.updated(i, mylist(j)).updated(j, temp)
////    }
//
//     def bubble_sort(mylist: MyList[A], length: Int):MyList[A]  = {
//       if(length <= 1) mylist
//       else bubble_sort(mylist.tail, length-1)
//     }
//
//     bubble_sort(this, this.size)
//   }
//    def sort_helper_bubble[T](mylist: Seq[T], length: Int): MyList[T] = {
//      if (length <= 1) mylist
//      else {
//        var newList = mylist
//        for (i <- 0 until length - 1) {
//
//          var first: A = newList(i)
//          var second: A = newList(i+1)
//          if (comparator.compare(first, second) > 0)
//            newList = swap(newList, i, i + 1)
//        }
////        sort_helper_bubble(newList, length - 1)
//      }
//      //mylist
//      sort_helper_bubble(mylist, length - 1)
//          //new NonEmptyList[T](sort_helper_bubble[T](mylist, mylist.size), mylist.size)
//    }



  //      val t = List(1,2,3,4,5)
  //
  //      t match {
  //        case Nil =>
  //        case h :: t => binary_search_helper(t, index -  1)
  //      }
  //
  List(1,2,3,4).zipWithIndex
  //    //  val resMap = Map(1 -> 0, 2 -> 2, 3 -> 2, 4 -> 3, 67 -> 4)
  //     // val index = resMap(target)


//    def binarySearch[A >:T](comparator: Comparator[A], target: A): Option[Int] = ???


  def binarySearch[A >:T](comparator: Comparator[A], target: A): Option[Int] = {

    def binary_search_helper(mylist: MyList[A], low: Int, high: Int, target: A): Option[Int] = {

      if (low > high) Some(-1)

      else {
        val mid = (low + high) / 2
        comparator.compare(mylist(mid).get, target) match {
          case n if n < 0 => binary_search_helper(mylist, mid + 1, high, target)
          case n if n > 0 => binary_search_helper(mylist, low, mid - 1, target)
          case _ => Some(mid)

        }
      }
    }
    binary_search_helper(this, 0, this.size - 1, target)
  }


  override def printElements: String =
    if(t.isEmpty) "" + h
    else h + " " + t.printElements

}

object comparator_main extends App {

  val list = new NonEmptyList(4, new NonEmptyList(2, new NonEmptyList(3, Empty)))

  println(list.printElements)
  println(list.tail.tail.head)
  println(list.isEmpty)
  println(list.size)

  val sortedlist2 = list.sort( new Comparator[Int] {
    override def compare(o1: Int, o2: Int): Int = o1 - o2
  })
  println(sortedlist2.printElements)


  val target = 4
  val target_result = sortedlist2.binarySearch(new Comparator[Int] { override def compare(o1: Int, o2: Int): Int = o1 - o2}, target)

  println(target_result)

}
















//package Assignments


import scala.annotation.tailrec


//abstract class Comparator[T] {
//  def compare(o1: T, o2: T): Int
//}
//
//abstract class MyList[T] {
//  //  def apply(mid: Int): T
//
//  def head: T
//  def tail: MyList[T]
//  def isEmpty: Boolean
//  def size(myList: MyList[T]): Int
//  def sort(comparator: Comparator[T]): MyList[T]
//  def binarySearch(comparator: Comparator[T], myList: MyList[T], target: T): Option[Int]
//  def printElements: String
//  override def toString: String = "[" + printElements + "]"
//}
//
//object EmptyList extends MyList[Nothing] {
//
//  //  def apply(mid: Int): Nothing = throw new NoSuchElementException
//  override def head: Nothing = throw new NoSuchElementException
//  override def tail: MyList[Nothing] = EmptyList
//  def isEmpty: Boolean = true
//  def size(myList: MyList[Nothing]): Int = 0
//  def sort(comparator: Comparator[Nothing]): MyList[Nothing] = EmptyList
//  def binarySearch(comparator: Comparator[Nothing], mylist: MyList[Nothing], target: Nothing): Option[Int] = Option(-1)
//  def printElements: String = ""
//}
//
//case class NonEmptyList[T](h:T, t:MyList[T]) extends MyList[T] {
//  //  override def apply(mid: Int): T = mylist._mid
//  def head: T = h
//  def tail: MyList[T] = t
//  def isEmpty: Boolean = false
//
//  def size(myList: MyList[T]): Int = {
//
//    @tailrec
//    def size_helper(mylist: MyList[T], length: Int): Int = {
//      if (mylist.isEmpty) length
//      else size_helper(mylist.tail, length + 1)
//    }
//
//    size_helper(myList, 0)
//  }
//
//  //  def sort(comparator: Comparator[T]): Seq[T] = ???
//
//  def sort(comparator: Comparator[T]): MyList[T] = {
//
//    def swap[T](mylist: Seq[T], i: Int, j: Int): Seq[T] = {
//      val temp = mylist(i)
//      mylist.updated(i, mylist(j)).updated(j, temp)
//    }
//
//
//    def sort_helper_bubble[T](mylist: Seq[T], length: Int): MyList[T] = {
//      if (length <= 1) mylist
//      else {
//        var newList = mylist
//        for (i <- 0 until length - 1) {
//
//          var first: T = newList(i)
//          if (comparator.compare(first, newList(i + 1)) > 0)
//            newList = swap(newList, i, i + 1)
//        }
//        //        sort_helper_bubble(newList, length - 1)
//      }
//      //mylist
//      sort_helper_bubble(mylist, length - 1)
//      //new NonEmptyList[T](sort_helper_bubble[T](mylist, mylist.size), mylist.size)
//    }
//
//
//
//  }
//
//  def binarySearch(comparator: Comparator[T], mylist: MyList[T], target: T): Option[Int] = {
//
//    def binary_search_helper(mylist: MyList[T], low: Int, high: Int, target: T): Option[Int] = {
//
//      if (low >= high) Some(-1)
//
//      else {
//        val mid = (low + high) / 2
//        comparator.compare(mylist(mid), target) match {
//          case n if n < 0 => binary_search_helper(mylist, mid + 1, high, target)
//          case n if n > 0 => binary_search_helper(mylist, low, mid - 1, target)
//          case _ => Some(mid)
//
//        }
//      }
//      //binary_search_helper(mylist, 0, mylist.size - 1, target)
//    }
//    binary_search_helper(mylist, 0, size(mylist) - 1, target)
//  }
//
//
//  override def printElements: String =
//    if(t.isEmpty) "" + h
//    else h + " " + t.printElements
//
//
//}
//
//
//object comparator_main extends App {
//  val list = new NonEmptyList(1, new NonEmptyList(2, new NonEmptyList(3, EmptyList)))
//
//  println(list.tail.tail.head)
//
//  println(list.isEmpty)
//  println(list.size(list))
//  println(list.toString)
//
//  //    val sortedList = list.sort(new Comparator[Int] { (o1: Int, o2: Int) => o1.compare(o2)})
//  //    println(sortedList)
//
//  val sortedlist2 = list.sort( new Comparator[Int] {
//    override def compare(o1: Int, o2: Int): Int = o1 - o2
//  })
//
//  val target = 2
//  val target_result = list.binarySearch(new Comparator[Int] { def compare(o1: Int, o2: Int): Int = o1 - o2}, sortedlist2, target)
//
//  println(target_result)
//
//}
