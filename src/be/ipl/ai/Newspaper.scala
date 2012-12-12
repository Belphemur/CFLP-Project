package be.ipl.ai

import JaCoP.scala._
import JaCoP.constraints._

object Newspaper extends jacop {

  def main(args: Array[String]) {

    val durations = List[List[Int]](List[Int](60, 30, 2, 5), List[Int](25, 75, 3, 10), List[Int](10, 15, 5, 30), List[Int](1, 1, 1, 90));
    newspaperProblem(List[String]("Algy", "Berty", "Charlie", "Digby"), List[Int](0, 15, 15, 60), List[String]("Financial Times", "Guardian", "Express", "Sun"), durations)
  }
  def newspaperProblem(readers: List[String], wakeUpTimes: List[Int], newspapers: List[String], durations: List[List[Int]]): (Int, List[List[IntVar]]) = {
    val allDurations = for (i <- List.range(0, readers.size)) yield for (j <- List.range(0, durations(i).size)) yield new IntVar("Dur_" + durations(i)(j), durations(i)(j), durations(i)(j))
    val startTimes = for (i <- List.range(0, readers.size)) yield setTasks(newspapers, wakeUpTimes(i), readers(i), allDurations(i));
    val nbNewsPaper = newspapers.size;
    val nbReaders = readers.size;
    val result = JSSP(startTimes, allDurations, nbNewsPaper, nbReaders)
    var max = result._2.flatten.foldLeft(0)((a, b) => if (a > b.value()) a else b.value())
    return (max, result._1);
  }
  def setTasks(newspapers: List[String], min: Int, name: String, durations: List[IntVar]): List[IntVar] = {
    val one = IntVar("One", 1, 1);

    val max = durations.map(_.value()).foldLeft(0)(_ + _)
    val tasks = for (i <- List.range(0, newspapers.size)) yield new IntVar(name + "_" + newspapers(i), min, min + max);
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
