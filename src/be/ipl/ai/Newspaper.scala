package be.ipl.ai

import JaCoP.scala._
import JaCoP.constraints._

object Newspaper extends jacop {

  def main(args: Array[String]) {

    val durations = List[List[Int]](List[Int](60, 30, 2, 5), List[Int](25, 75, 3, 10), List[Int](10, 15, 5, 30), List[Int](1, 1, 1, 90));
    val names = List[String]("Algy", "Berty", "Charlie", "Digby");
    val result = newspaperProblem(names, List[Int](0, 15, 15, 60), List[String]("Financial Times", "Guardian", "Express", "Sun"), durations)
    printResult(result _1, result _2, result _3, names)
  }

  def printResult(max: Int, result: List[List[IntVar]], endTimes: List[List[IntVar]], names: List[String]) = {
    println("Durée maximum : " + max + " minutes");
    for (i <- List.range(0, result.size)) {
      println(names(i) + " : ")
      for (j <- List.range(0, result(i).size)) {
        val newspaper = result(i).sort((a, b) => a.value() < b.value())(j)
        println("\t \t Commence le " + newspaper.id + " en " + newspaper.value() + " et fini en " + endTimes(i).sort((a, b) => a.value() < b.value())(j).value + " ")

      }
      println;
      println
    }
  }

  def newspaperProblem(readers: List[String], wakeUpTimes: List[Int], newspapers: List[String], durations: List[List[Int]]): (Int, List[List[IntVar]], List[List[IntVar]]) = {
    val allDurations = for (i <- List.range(0, readers.size)) yield for (j <- List.range(0, durations(i).size)) yield new IntVar("Dur_" + durations(i)(j), durations(i)(j), durations(i)(j))
    val startTimes = for (i <- List.range(0, readers.size)) yield setTasks(newspapers, wakeUpTimes(i), allDurations(i));
    val nbNewsPaper = newspapers.size;
    val nbReaders = readers.size;
    val result = JSSP(startTimes, allDurations, nbNewsPaper, nbReaders)
    val max = result._2.flatten.foldLeft(0)((a, b) => if (a > b.value()) a else b.value())
    return (max, result._1, result _2);
  }
  def setTasks(newspapers: List[String], min: Int, durations: List[IntVar]): List[IntVar] = {
    val one = IntVar("One", 1, 1);

    val max = durations.map(_.value()).foldLeft(0)(_ + _)
    val tasks = for (i <- List.range(0, newspapers.size)) yield new IntVar(newspapers(i), min, min + max);
    val ressources = for (i <- List.range(0, newspapers.size)) yield one;
    //Cumulatives
    cumulative(tasks, durations.toArray, ressources, one);
    return tasks;
  }

  def JSSP(startTimes: List[List[IntVar]], durations: List[List[IntVar]], nbNewsPaper: Int, nbReaders: Int): (List[List[IntVar]], List[List[IntVar]]) = {

    //endtimes
    val endTimes = for (i <- List.range(0, startTimes.size)) yield for (j <- List.range(0, startTimes(i).size)) yield (durations(i)(j) + startTimes(i)(j));
    //Constraint
    for (curRd <- 0 to nbReaders - 1)
      for (curNp <- 0 to nbNewsPaper - 1) {
        for (otherRd <- 0 to nbReaders - 1) {
          if (otherRd != curRd) {
            //println(curTask + " -> " + otherTask)
            OR(endTimes(curRd)(curNp) #<= startTimes(otherRd)(curNp), endTimes(otherRd)(curNp) #<= startTimes(curRd)(curNp));
          }
        }

      }
    def printSol(): Unit = {
      for (curRd <- 0 to nbReaders - 1) {
        for (curNp <- 0 to nbNewsPaper - 1) {
          print(startTimes(curRd)(curNp).id + " " + startTimes(curRd)(curNp).value + " -> " + endTimes(curRd)(curNp).value + " ")
        }
        println;
      }
    }
    minimize(search(startTimes.foldLeft(List[IntVar]())((b, a) => a ::: b), first_fail, indomain_middle), max(endTimes.foldLeft(List[IntVar]())((b, a) => a ::: b)));
    printSol
    return (startTimes, endTimes);
  }
}
