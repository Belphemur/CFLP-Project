package be.ipl.ai

import JaCoP.scala._
import JaCoP.constraints._

object Newspaper extends jacop {

  def main(args: Array[String]) {

    val durations = List[List[Int]](List[Int](60, 30, 2, 5), List[Int](25, 75, 3, 10), List[Int](10, 15, 5, 30), List[Int](1, 1, 1, 90));
    newspaperProblem(List[String]("Algy", "Berty", "Charlie", "Digby"), List[Int](0, 15, 15, 60), List[String]("Financial Times", "Guardian", "Express", "Sun"), durations)
  }
  def newspaperProblem(readers: List[String], wakeUpTimes: List[Int], newspapers: List[String], durations: List[List[Int]]): List[IntVar] = {
    val allDurations = for (i <- 0 to readers.size - 1) yield for (j <- 0 to durations(i).size - 1) yield new IntVar("Dur_" + durations(i)(j), durations(i)(j), durations(i)(j))
    val startTimes = for (i <- 0 to readers.size - 1) yield setTasks(newspapers, wakeUpTimes(i), readers(i), allDurations(i));
    val nbNewsPaper = newspapers.size;
    val nbReaders = readers.size;
    val result = JSSP(startTimes, allDurations, nbNewsPaper, nbReaders)
    //    var max = 0;
    //    var generalResultReader = List[IntVar]();
    //    for (i <- 0 to result.size - 1) {
    //      var perReader = List[IntVar]();
    //      val res = result(i);
    //      if (res.value > max)
    //        max = res.value
    //      perReader = perReader ::: List[IntVar](res);
    //      if (i + 1 % nbNewsPaper == 0) {
    //        generalResultReader = generalResultReader ::: perReader;
    //        perReader = List[IntVar]();
    //      }
    //    }
    //
    //    return List[IntVar](max) ::: generalResultReader;
    return List[IntVar]();
  }
  def setTasks(newspapers: List[String], min: Int, name: String, durations: IndexedSeq[IntVar]): IndexedSeq[IntVar] = {
    val one = IntVar("One", 1, 1);

    val max = durations.map(_.value()).foldLeft(0)(_ + _)
    val tasks = for (i <- List.range(0, newspapers.size)) yield new IntVar(name + "_" + newspapers(i), min, min + max);
    val ressources = for (i <- List.range(0, newspapers.size)) yield one;
    //Cumulatives
    cumulative(tasks, durations.toArray, ressources, one);
    return tasks.toIndexedSeq;
  }

  def JSSP(startTimes: IndexedSeq[IndexedSeq[IntVar]], durations: IndexedSeq[IndexedSeq[IntVar]], nbNewsPaper: Int, nbReaders: Int): IndexedSeq[IndexedSeq[IntVar]] = {

    def concact(toConcact: IndexedSeq[IndexedSeq[IntVar]]): List[IntVar] = {
      var times = List[IntVar]();
      for (i <- List.range(0, toConcact.size))
        for (j <- 0 to toConcact(i).size - 1)
          times = times ::: List[IntVar](toConcact(i)(j))
      return times;
    }
    //endtimes
    val endTimes = for (i <- List.range(0, startTimes.size)) yield for (j <- 0 to startTimes(i).size - 1) yield (durations(i)(j) + startTimes(i)(j));
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
    minimize(search(concact(startTimes), first_fail, indomain_middle), max(concact(endTimes.toIndexedSeq)));
    printSol
    return startTimes;
  }
}
