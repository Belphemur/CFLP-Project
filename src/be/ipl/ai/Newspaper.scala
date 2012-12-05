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
  def main(args: Array[String]) {

    //Durations
    val algyDur = Array[IntVar](60, 30, 2, 5);
    val bertyDur = (Array[IntVar](75, 3, 25, 10))
    val charlieDur = (Array[IntVar](5, 15, 10, 30))
    val digbyDur = (Array[IntVar](90, 1, 1, 1))

    //All together
    val papers = setTasks(newspapers, 0, 97, "Algy", algyDur) ::: setTasks(newspapers, 15, 97, "Berty", bertyDur) ::: setTasks(newspapers, 15, 97, "Charlie", charlieDur) ::: setTasks(newspapers, 30, 97, "Digby", digbyDur);
    val dur = algyDur ::: bertyDur ::: charlieDur ::: digbyDur;
    JSSP(papers, dur);

  }

  def setTasks(newspapers: Array[IntVar], shift: Int, max: Int, name: String, durations: Array[IntVar]): Array[IntVar] = {
    val shifted = for (i <- List.range(0, newspapers.size)) yield new IntVar(name + "_" + newspapers(i).id, shift, max);
    val ressources = for (i <- List.range(0, newspapers.size)) yield one;
    //Cumulatives
    cumulative(shifted, durations, ressources, limit);
    return shifted;
  }

  def JSSP(newspapers: Array[IntVar], durations: Array[IntVar]): Boolean = {

    //Constraints
    alldistinct(newspapers);

    //endtimes
    val endTimes = for (i <- List.range(0, durations.size)) yield (newspapers(i) + durations(i));

    def printSol(): Unit = {
      var i = 0;
      for (v <- newspapers) {
        print(v.id + " " + v.value)
        print(" -> " + endTimes(i).value() + " ")
        i = i + 1;
        if (i % nbNewsPaper == 0)
          println();
      }
      println()
    }
    val result = minimize(search(newspapers, first_fail, indomain_middle), max(endTimes));
    printSol
    return result;
  }
}