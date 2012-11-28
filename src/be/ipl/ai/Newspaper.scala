package be.ipl.ai

import JaCoP.scala._

object Newspaper extends jacop {
  def main(args: Array[String]) {

    //Data

    //NewsPapers
    val FT = IntVar("FT", 1, 4);
    val Guardian = IntVar("Guardian", 1, 4);
    val Express = IntVar("Express", 1, 4);
    val Sun = IntVar("Sun", 1, 4);

    //Data
    val newspapers = Array { FT; Guardian; Express; Sun };
    val durations = Array {
      IntVar("60", 60, 60);
      IntVar("30", 30, 30);
      IntVar("2", 2, 2);
      IntVar("2", 2, 2)
    };
    val one = IntVar("One", 1, 1);
    val ressources = for (i <- List.range(0, durations.size)) yield one;
    val limits = IntVar("Algy", 1, 1); //Only Algy as reader

    //Constraints
    alldifferent(newspapers);
  }
}