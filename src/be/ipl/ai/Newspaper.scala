package be.ipl.ai

import JaCoP.scala._
import JaCoP.constraints._

object Newspaper extends jacop {
  //Data
  val limit = IntVar("limit", 1, 1);
  val one = IntVar("One", 1, 1);
  //NewsPapers
  val FT = IntVar("FT", 0, 0);
  val Guardian = IntVar("Guardian", 0, 0);
  val Express = IntVar("Express", 0, 0);
  val Sun = IntVar("Sun", 0, 0);
  //Data
  val newspapers = Array[IntVar](FT, Guardian, Express, Sun);
  val nbNewsPaper = newspapers.size;
  val nbReaders = 4;
  def main(args: Array[String]) {

    //Durations
    val algyDur = Array[IntVar](60, 30, 2, 5);
    val bertyDur = Array[IntVar](25, 75, 3, 10)
    val charlieDur = Array[IntVar](10, 15, 5, 30)
    val digbyDur = Array[IntVar](1, 1, 1, 90)

    //All together
    val papers = setTasks(newspapers, 0, "Algy", algyDur) ::: setTasks(newspapers, 15, "Berty", bertyDur) ::: setTasks(newspapers, 15, "Charlie", charlieDur) ::: setTasks(newspapers, 30, "Digby", digbyDur);
    val dur = algyDur ::: bertyDur ::: charlieDur ::: digbyDur;
    JSSP(papers, dur);

  }

  def intVarSum(toSum: Array[IntVar]): Int = {
    var i = 0
    var sum = 0;
    while (i < toSum.length) {
      sum += toSum(i).value();
      i += 1
    }
    return sum;
  }
  def setTasks(newspapers: Array[IntVar], min: Int, name: String, durations: Array[IntVar]): Array[IntVar] = {
    val max = intVarSum(durations);
    val tasks = for (i <- List.range(0, newspapers.size)) yield new IntVar(name + "_" + newspapers(i).id, min, min + max);
    val ressources = for (i <- List.range(0, newspapers.size)) yield one;
    //Cumulatives
    cumulative(tasks, durations, ressources, limit);
    return tasks;
  }

  def JSSP(startTimes: Array[IntVar], durations: Array[IntVar]): Boolean = {

    //endtimes
    val endTimes = for (i <- List.range(0, durations.size)) yield (startTimes(i) + durations(i));
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
    val result = minimize(search(startTimes, first_fail, indomain_middle), max(endTimes));
    printSol
    return result;
  }
}