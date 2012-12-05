package be.ipl.ai

import JaCoP.scala._
import JaCoP.constraints._

object Newspaper extends jacop {

  def main(args: Array[String]) {

    val durations = List[List[Int]](List[Int](60, 30, 2, 5), List[Int](25, 75, 3, 10), List[Int](10, 15, 5, 30), List[Int](1, 1, 1, 90));
    newspaperProblem(List[String]("Algy", "Berty", "Charlie", "Digby"), List[Int](0, 15, 15, 60), List[String]("Financial Times", "Guardian", "Express", "Sun"), durations)
  }
  def newspaperProblem(readers: List[String], wakeUpTimes: List[Int], newspapers: List[String], durations: List[List[Int]]): List[Unit] = {
    var startTimes = Array[IntVar]();
    var allDurations = List[IntVar]();
    for (i <- 0 to readers.size - 1) {
      var dur = List[IntVar]();
      val tempDur = durations(i);
      for (j <- 0 to tempDur.size - 1) {
        dur = dur ::: List[IntVar](tempDur(j));
      }
      allDurations = allDurations ::: dur;
      startTimes = startTimes ::: setTasks(newspapers, wakeUpTimes(i), readers(i), dur);
    }
    val nbNewsPaper = newspapers.size;
    val nbReaders = readers.size;
    val result = JSSP(startTimes, allDurations, nbNewsPaper, nbReaders)
    return List();
  }
  def setTasks(newspapers: List[String], min: Int, name: String, durations: Array[IntVar]): List[IntVar] = {
    val one = IntVar("One", 1, 1);
    def intVarSum(toSum: Array[IntVar]): Int = {
      var i = 0
      var sum = 0;
      while (i < toSum.length) {
        sum += toSum(i).value();
        i += 1
      }
      return sum;
    }
    val max = intVarSum(durations);
    val tasks = for (i <- List.range(0, newspapers.size)) yield new IntVar(name + "_" + newspapers(i), min, min + max);
    val ressources = for (i <- List.range(0, newspapers.size)) yield one;
    //Cumulatives
    cumulative(tasks, durations, ressources, one);
    return tasks;
  }

  def JSSP(startTimes: Array[IntVar], durations: Array[IntVar], nbNewsPaper: Int, nbReaders: Int): List[IntVar] = {

    //endtimes
    val endTimes = for (i <- List.range(0, durations.size)) yield (startTimes(i) + durations(i));
    //Constraint
    for (curNp <- 0 to nbNewsPaper - 1)
      for (curRd <- 0 to nbReaders - 1) {
        val curTask = curNp + (curRd * nbReaders);
        for (otherRd <- 0 to nbReaders - 1) {
          if (otherRd != curRd) {
            val otherTask = curNp + (otherRd * nbReaders);
            //println(curTask + " -> " + otherTask)
            OR(endTimes(curTask) #<= startTimes(otherTask), endTimes(otherTask) #<= startTimes(curTask));
          }
        }

      }
    def printSol(): Unit = {
      var i = 0;
      for (v <- startTimes) {
        print(v.id + " " + v.value)
        print(" -> " + endTimes(i).value() + " ")
        i = i + 1;
        if (i % nbNewsPaper == 0)
          println();
      }
      println()
    }
    minimize(search(startTimes, first_fail, indomain_middle), max(endTimes));
    printSol
    return startTimes;
  }
}
