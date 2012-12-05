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
    JSSP(newspapers, durations);
    JSSP(addTime(newspapers,15,97), Array[IntVar](75,3,25,10));
    JSSP(addTime(newspapers,15,97), Array[IntVar](5,15,10,30));
    JSSP(addTime(newspapers,30,97), Array[IntVar](90,1,1,1));
   }
  
  def addTime(newspapers : Array[IntVar], shift : Int, max : Int) : Array[IntVar] = {
    val shifted =  for (i <- List.range(0, newspapers.size)) yield new IntVar(newspapers(i).id,shift,max);
    return shifted;
  }
  
  def JSSP(newspapers : Array[IntVar] , durations : Array[IntVar]) : Boolean = {
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