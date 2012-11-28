package be.ipl.ai

import JaCoP.scala._

object Newspaper extends jacop {
  def main(args: Array[String]) {
    //NewsPapers
    val FT = IntVar("FT", 1, 4);
    val Guardian = IntVar("Guardian", 1, 4);
    val Express = IntVar("Express", 1, 4);
    val Sun = IntVar("Sun", 1, 4);

    //Data
    val newspapers = { FT; Guardian; Express; Sun };
    val durations = {
      IntVar("60", 60, 60);
      IntVar("30", 30, 30);
      IntVar("2", 2, 2);
      IntVar("2", 2, 2)
    };
    val limits = IntVar("Algy", 1, 1); //Only Algy as reader
  }
}