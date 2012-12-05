package be.ipl.ai

import JaCoP.scala._
import JaCoP.constraints._

object Newspaper extends jacop {
  def main(args: Array[String]) {

    //Data

    //NewsPapers
    val FT = IntVar("FT", 0, 0);
    val Guardian = IntVar("Guardian", 0, 0);
    val Express = IntVar("Express", 0, 0);
    val Sun = IntVar("Sun", 0, 0);

    //Data
    val newspapers = Array[IntVar](FT, Guardian, Express, Sun);
    val durations = Array[IntVar](60, 30, 2, 5);
    val papers = setTasks(newspapers, 0, 97, "Algy") ::: setTasks(newspapers, 15, 97, "Berty") ::: setTasks(newspapers, 15, 97, "Charlie") ::: setTasks(newspapers, 30, 97, "Digby");
    val dur = durations ::: (Array[IntVar](75, 3, 25, 10)) ::: (Array[IntVar](5, 15, 10, 30)) ::: (Array[IntVar](90, 1, 1, 1));
    JSSP(papers, dur);

  }

  def setTasks(newspapers: Array[IntVar], shift: Int, max: Int, name: String): Array[IntVar] = {
    val shifted = for (i <- List.range(0, newspapers.size)) yield new IntVar(name + "_" + newspapers(i).id, shift, max);
    return shifted;
  }

  def JSSP(newspapers: Array[IntVar], durations: Array[IntVar]): Boolean = {
    val one = IntVar("One", 1, 1);
    val ressources = for (i <- List.range(0, durations.size)) yield one;
    val limit = IntVar("limit", 1, 1);

    //Constraints
    alldistinct(newspapers);

    //endtimes
    val endTimes = for (i <- List.range(0, durations.size)) yield (newspapers(i) + durations(i));

    //Cumulatives
    cumulative(newspapers, durations, ressources, limit);
    def printSol(): Unit = {
      var i = 0;
      for (v <- newspapers) {
        print(v.id + " " + v.value)
        print(" -> " + endTimes(i).value() + " ")
        i = i + 1;
      }
      println()
    }
    val result = minimize(search(newspapers, first_fail, indomain_middle), max(endTimes));
    printSol
    return result;
  }
}