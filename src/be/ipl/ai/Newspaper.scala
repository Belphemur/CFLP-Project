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
    val durations = { 60; 30; 2; 5 };
  }
}