package be.ipl.ai

import JaCoP.scala._
import JaCoP.constraints._

object Newspaper extends jacop {
  def main(args: Array[String]) {

    //Data

    //NewsPapers
    val FT = IntVar("FT", 0, 97);
    val Guardian = IntVar("Guardian", 0, 97);
    val Express = IntVar("Express", 0, 97);
    val Sun = IntVar("Sun", 0, 97);

    //Data
    val newspapers = Array[IntVar](FT, Guardian, Express, Sun);
    val durations = Array[IntVar](60, 30, 2, 5);
    val one = IntVar("One", 1, 1);
    val ressources = for (i <- List.range(0, durations.size)) yield one;
    val limit = IntVar("Algy", 1, 1); //Only Algy as reader

    //Constraints
    alldistinct(newspapers);
    max(newspapers);

    //Cumulatives
    cumulative(newspapers, durations, ressources, limit);
    def printSol(): Unit = {
      for (v <- newspapers) print(v.id + " " + v.value + " ")
      println()
    }
    val result = minimize(search(newspapers, first_fail, indomain_middle), sum(durations), printSol);
  }
}