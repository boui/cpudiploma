package com.boui.cpudiploma

import util.Random
import org.paukov.combinatorics.Factory
import scala.collection.JavaConverters._
import annotation.tailrec
import scala.math._
import com.boui.cpudiploma.GraphGenerator.Graph
import collection.mutable

trait AlgorithmSettings {
  def k: Int

  def n: Int

  def timeTransfer: Double

  def timeTesting: Double

  def ttr: Int

  def tst: Int
}

object GraphGenerator {
  val random = new Random()

  case class Graph(adjacency: Array[Array[Boolean]]) {
    def size = adjacency.length

    private val Inf = 10000

    def minWithInf(a: Int, b: Int) = {
      (a, b) match {
        case (x, y) if (x >= Inf) => y
        case (x, y) if (y >= Inf) => x
        case _ => min(a, b)
      }
    }

    lazy val distances: Array[Array[Int]] = {
      val dist = Array.fill[Int](size, size)(Inf)
      for (i <- 0 until size; j <- 0 until size) {
        dist(i)(j) = if (adjacency(i)(j) == true) 1 else Inf
      }
      for (i <- 0 until size) {
        dist(i)(i) = 0
      }

      for (k <- 0 until size; i <- 0 until size; j <- 0 until size) {
        dist(i)(j) = minWithInf(dist(i)(j), dist(i)(k) + dist(k)(j))
      }
      dist
    }

    private def matrixToString[T](m: Array[Array[T]], t: (T) => String) = m
      .map(_.map(t(_))
      .mkString(" "))
      .mkString("\n")

    override def toString: String = matrixToString(adjacency, {
      item: Boolean => if (item) "1" else "0"
    })

    def distancesToString: String = matrixToString(distances, {
      item: Int => "%3d".format(item)
    })
  }

  object Graph {

    /**
     * wave algorithm with painting and queue
     */
    def isBound(graph: Graph): Boolean = {
      val visited = Array.ofDim[Boolean](graph.size)
      val neighbours = collection.mutable.Queue[Int](0)
      while (!neighbours.isEmpty) {
        val item = neighbours.dequeue()
        visited(item) = true
        neighbours.enqueue(graph.adjacency(item)
          .zipWithIndex
          .filter(i => i._1 && !visited(i._2))
          .map(_._2): _*)
      }
      visited.forall(_ == true)
    }

    /**
     * Try-fail algorithms, builds complete graph then removes
     * edges until
     */
    def generateRemoveUntil(size: Int) = {
      val graph = Graph(Array.fill[Boolean](size, size)(true))
      for (i <- 0 until size) {
        graph.adjacency(i)(i) = false
      }
      var failsLeft = 2
      while (failsLeft >= 0) {
        val i = random.nextInt(graph.size - 1) + 1
        val j = random.nextInt(i)
        graph.adjacency(i)(j) = false
        graph.adjacency(j)(i) = false
        if (!isBound(graph)) {
          graph.adjacency(i)(j) = true
          graph.adjacency(j)(i) = true
          failsLeft -= 1
        }
      }
      graph
    }

    /**
     * Full graph
     */
    def generateFull(size: Int) = {
      val graph = Graph(Array.fill[Boolean](size, size)(true))
      for (i <- 0 until size) {
        graph.adjacency(i)(i) = true
      }
      graph
    }

    /**
     * Circle graph
     */
    def generateCircle(size: Int) = {
      val graph = Graph(Array.fill[Boolean](size, size)(false))
      for (i <- 0 until (size - 1)) {
        graph.adjacency(i)(i + 1) = true
        graph.adjacency(i + 1)(i) = true
      }
      graph.adjacency(size - 1)(0) = true;
      graph.adjacency(0)(size - 1) = true;
      graph
    }

  }

}

trait PermutationsIteratorComponent {

  case class Solution(cpus: Array[Set[Int]], t: Double)

  def genPermutationsForCpu(permutation: Set[Int], cpuIndex: Int) = {
    permutation.map(cpu => if (cpu >= cpuIndex) cpu + 1 else cpu)
  }

  def iterate(k: Int, size: Int, cnt: Array[Set[Int]], evaluateCurrent: () => Unit): Unit
}

trait AllPermutationsIteratorComponent extends PermutationsIteratorComponent {
  def buildPermutations(k: Int, size: Int) = {
    val initialVector = Factory.range(size)
    val generator = Factory.createSimpleCombinationGenerator(initialVector, k)
    generator.generateAllObjects().asScala.toList.map {
      item =>
        item.asScala.map(_.asInstanceOf[Int] - 1).toSet
    }
  }

  def iterate(k: Int, size: Int, cnt: Array[Set[Int]], evaluateCurrent: () => Unit) {
    val seedPermutations = buildPermutations(k, size - 1)

    def iterateForLevel(level: Int) {
      if (level == size) {
        evaluateCurrent()
      } else {
        val permutations = seedPermutations.map(permutation => genPermutationsForCpu(permutation, level))
        for (permutation <- permutations) {
          cnt(level) = permutation
          iterateForLevel(level + 1)
        }
      }
    }

    iterateForLevel(0)
  }
}

trait RandomPermutationsIteratorComponent extends PermutationsIteratorComponent {
  def totalToTest: Int

  private val rnd = new Random

  def genPermutation(k: Int, n: Int) = {
    @tailrec
    def improveSet(current: Set[Int]): Set[Int] = {
      if (current.size == k) {
        current
      } else {
        val nextItem = rnd.nextInt(n)
        improveSet(current + nextItem)
      }
    }

    improveSet(Set())
  }

  def iterate(k: Int, size: Int, cnt: Array[Set[Int]], evaluateCurrent: () => Unit) {
    for (_ <- 0 until totalToTest) {
      for (i <- 0 until size) {
        cnt(i) = genPermutationsForCpu(genPermutation(k, size - 1), i)
      }
      evaluateCurrent()
    }
  }
}

trait SolutionEvaluator {
  def graph: Graph

  def settings: AlgorithmSettings

  def getTestTime(cnt: Iterable[Set[Int]])(op: Iterable[Double] => Double) = {
    val whoItest = (0 until settings.n).map(cpu => cnt.zipWithIndex.filter(_._1.contains(cpu)).map(_._2))
    val tTr = whoItest.zipWithIndex.map(i => i._1.map(j => graph.distances(i._2)(j)).sum * settings.timeTransfer)
    val tEv = whoItest.map(i => i.size * settings.timeTesting)
    op(tTr.zip(tEv).map(i => i._1 + i._2 + settings.timeTesting))
  }
}

abstract sealed class OperationMode {
  def aggregationOperation: Iterable[Double] => Double
}

case object Parallel extends OperationMode {
  def aggregationOperation: (Iterable[Double]) => Double = _.max
}

case object Sequential extends OperationMode {
  def aggregationOperation: (Iterable[Double]) => Double = _.sum
}

trait AlgorithmRunner extends SolutionEvaluator {
  self: PermutationsIteratorComponent =>


  def graph: Graph

  def settings: AlgorithmSettings

  def operationMode: OperationMode

  def run {
    def findSolution(): Solution = {
      val cnt = Array.ofDim[Set[Int]](settings.n)
      val optimal = Array.ofDim[Set[Int]](settings.n)
      var optimalValue = Double.MaxValue

      def storeOptimal {
        cnt.copyToArray(optimal)
      }

      def evaluateCurrent {
        val tTst = getTestTime(cnt)(operationMode.aggregationOperation)
        if (tTst < optimalValue) {
          optimalValue = tTst
          storeOptimal
        }
      }

      iterate(settings.k, settings.n, cnt, evaluateCurrent _)
      val s = Solution(optimal, optimalValue)
      s
    }


    val solution = findSolution()
    println("Solution: " + solution.cpus.toList)
    println("Time: " + solution.t)
  }
}

trait OptimalTesters extends SolutionEvaluator {
  def graph: Graph

  def settings: AlgorithmSettings

  def run() {
    val solutions = for (i <- 0 until settings.n) yield {
      (0 until settings.k).foldLeft(Set[Int]()) {
        (foundCpus, itemIndex) =>
          val nextOptimalTester = graph.distances(i)
            .view
            .zipWithIndex
            .filter(_._2 != i)
            .filter(cpu => !foundCpus.contains(cpu._2))
            .sortBy(_._1)
            .map(_._2)
            .head
          foundCpus + nextOptimalTester
      }
    }
    val maxTestingTime = getTestTime(solutions)(_.sum)
    println("Solution: " + solutions.toList)
    println("Time: " + maxTestingTime)
  }
}


abstract sealed trait Cell

case class TD(ttl: Int) extends Cell

//X
case class TT(td: Int, ttl: Int) extends Cell

// o
case class P(td: Int, tt: Int, ttl: Int) extends Cell

//~
case class E() extends Cell

//empty


trait CoolTesters extends SolutionEvaluator {
  def graph: Graph

  def settings: AlgorithmSettings

  def run() {
    val arrayOfAllStates = mutable.MutableList[Array[Cell]]()

    @tailrec
    def step(solution: Array[Set[Int]], state: Array[Cell], random: Boolean, level: Int): Array[Set[Int]] = {
      val newTTLUPDState = updateTTL(state)
      if (!isFinal(solution)) {
        arrayOfAllStates.+=(state)
//        println("_______________")
//        println("recursion level " + level)
//        println("SOLUTION ON LEVEL:" + solution.mkString(" , "))
//        println("STATE ON LEVEL:" + state.mkString(" , "))
//        println("UPDATED STATE " + newTTLUPDState.mkString(" , ") + "\n")

        val next = selectNext(newTTLUPDState, solution, freeToTest(newTTLUPDState, solution, mutable.MutableList[Int]()), mutable.MutableList[Int](), random);
        step(next._1, next._2,
//          true
          level % 2 == 0
          , level + 1)
      } else {
        solution
      }
    }

    def selectNext(state: Array[Cell], solution: Array[Set[Int]], free: Array[(Array[Int], Int)], exclude: mutable.MutableList[Int], r: Boolean)
    : (Array[Set[Int]], Array[Cell]) = {
      val freeNoSolved = free.filter(x => solution(x._2).size < settings.k)
//      println("Free Not Solved" + freeNoSolved.mkString("[", ",", "]"))
      if (free.size >= 1 && freeNoSolved.size >= 1) {
        val t = freeNoSolved(selectProcessor(r, freeNoSolved))
        val td = t._2
//        println("TD = " + td)
        //not td, not present in solution already
        val ttList = free.filter(x => x._2 != td).filter(x => !solution(td).contains(x._2))
//        println(ttList.mkString("ttList = [", ",", "]"))

        if (ttList.size > 0) {
          val ttT = ttList(selectProcessor(r, ttList))
          val ttTuple = (ttT._1(td), ttT._2)
          val tt = ttTuple._2
          val dist = ttTuple._1

          val ttl = 2 * settings.ttr
          val path: Array[Int] =
            if (dist > 1) {
              findMinPath(td, tt, mutable.MutableList[Int](), state)
            }
            else {
              Array(td, tt)
            }

          if (path.isEmpty) {
//            println("Currently unreachable vertex :" + td)
            exclude.+=(td)
          } else {


//            println("=>STATE BEFORE UPD:  " + state.mkString("[", " , ", "]"));
            path.foreach(
              p => {
                state(p) = p match {
                  case `td` => TD(ttl)
                  case `tt` => TT(td, ttl)
                  case _ => {
//                    println("P selected")
                    P(td, tt, ttl)
                  }
                }
              }
            )
//            println("=>STATE AFTER UPD:" + state.mkString("[", " , ", "]"));
            solution(td) = solution(td) + tt
          }
        } else {
          exclude.+=(td)
        }
      }

      val newFree = freeToTest(state, solution, exclude)

      val newFreeNoSolved = newFree.filter(x => solution(x._2).size < settings.k)
//      println("NEW FREE: " + newFree.mkString("[", " , ", "]"))
//      println("--------")
      //      && !isFinal(solution)
      if (newFree.size > 1 && newFreeNoSolved.size > 0) {
        selectNext(state, solution, newFree, exclude, r)
      }
      (solution, state)
    }

    def selectProcessor(r: Boolean, plist: Array[(Array[Int], Int)]): Int = {
      if (r && plist.size > 1) {
        val a = Random.nextInt(plist.size - 1)
//        println("selected " + a)
        a
      } else {
//        println("selected 0")
        0
      }
    }

    def freeToTest(state: Array[Cell], solution: Array[Set[Int]], exclude: mutable.MutableList[Int]): Array[(Array[Int], Int)] = {
//      println("WILL BE EXCLUDED:" + exclude.mkString("[", ",", "]"))
      val zippedDistances = graph.distances.zipWithIndex
      zippedDistances.filter {
        t => {
          state(t._2) match {
            case E() => true
            case _ => false
          }
        }
      }.filter(x => !exclude.contains(x._2))
    }


    def findMinPath(start: Int, end: Int, path: mutable.MutableList[Int], state: Array[Cell]): Array[Int] = {
      if (start != end) {
//        println("Looking for path:[" + start + " to " + end + "]=" + graph.distances(start).mkString(","))
        val filteredDist = graph.distances(start)
          .zipWithIndex
          .filter(x => graph.adjacency(start)(x._2))
          .filter(x => x._2 != start)
          .filter(x => !path.contains(x._2))
          .filter {
          x => {
            state(x._2) match {
              case E() => true
              case _ => false
            }
          }
        }


        if (filteredDist.size > 0) {
          val zero = filteredDist(0)
          val minIndex = filteredDist.foldLeft(zero) {
            (acc, x) => (graph.minWithInf(acc._1, x._1), acc._2)
          }._2
          findMinPath(minIndex, end, path.+=(minIndex), state)
        } else {
//          println("NO PATH ((")
          Array[Int]()
        }

      }
      path.+=(end).toArray
    }

    def updateTTL(state: Array[Cell])
    : Array[Cell] = {
      state.map {
        cell => cell match {
          case P(td, tt, ttl) => {
            val t = ttl - settings.ttr
            if (t <= 0) E() else P(td, tt, t)
          }
          case x@TD(ttl) => {
            val t = ttl - settings.ttr
            if (t <= 0) E() else TD(t)
          }
          case TT(td, ttl) => {
            val t = ttl - settings.ttr
            if (t <= 0) E() else TT(td, t)
          }
          case E() => E()
        }
      }
    }

    def isFinal(rez: Iterable[Set[Int]]) = {
//      println("testing solution to be final:" + rez.mkString(",") + " | k = " + settings.k)
      val res = rez.foldLeft(true) {
        (x, y) => x && y.size == settings.k
      }
//      println("is final?:" + res)
      res
    }

    val rez = (0 to settings.n - 1).map(_ => Set[Int]()).toArray
    val state: Array[Cell] = Array.fill(settings.n)(E())


      val st = step(rez, state, true, 0)



    println("Solution: " + st.map(print _))
//    arrayOfAllStates.reverse.zipWithIndex.map(x => println(x._1.map {
//      t => t match {
//        case E() => " "
//        case TD(_) => "x"
//        case TT(_, _) => "o"
//        case P(_, _, _) => "~"
//      }
//    }.mkString("|", " ", "|"+x._2)))
//    println(arrayOfAllStates.size)

    println("Detailed information")
    arrayOfAllStates.reverse.zipWithIndex.map(x => println(x._1.map {
      t => "%8s".format(t.toString)
    }.mkString("|", " ", "|"+x._2)))

  }

}

object Main {
  def main(argv: Array[String]) {
    val settings =

      (1 to 1).map{x=>
        new AlgorithmSettings {
          def n = 50
          def timeTesting = 0.2
          def k = 10
          def timeTransfer = 0.1
          def ttr = 1
          def tst = 1
      }
      }




    settings.map {
      x=> {
//        println("___")
//        println("n = "+x.n+"; \n k = "+x.k)
        val algorithmSettings = x
//

// val generatedGraph = GraphGenerator.Graph.generateRemoveUntil(algorithmSettings.n)
// println("Random remove Graph:\n" + generatedGraph)
//        println("Distances:\n" + generatedGraph.distancesToString)
        //

        val generatedFullGraph = GraphGenerator.Graph.generateFull(algorithmSettings.n)
        //    println("Full Graph:\n" + generatedFullGraph)
        //    println("Distances:\n" + generatedFullGraph.distancesToString)

//        val generatedCircleGraph = GraphGenerator.Graph.generateCircle(algorithmSettings.n)
//                println("Circle Graph:\n" + generatedCircleGraph)
//                println("Distances:\n" + generatedCircleGraph.distancesToString)

//        println("ALGORYTHM #4 Task Planning")
//            println("Generated with removals graph")
//            val coolPermutationsRunner = new CoolTesters {
//              def settings = algorithmSettings
//              def graph = generatedGraph
//              def operationMode = Parallel
//            }
//            coolPermutationsRunner.run

        val allRunner = new CoolTesters {
          def settings = algorithmSettings
          def graph = generatedFullGraph
          def operationMode = Parallel
        }

        val startAll = System.currentTimeMillis()
        allRunner.run
        val timeSpentAll = System.currentTimeMillis() - startAll
        println("Timestamp:"+timeSpentAll)


//        val circleRunner = new CoolTesters {
//          def settings = algorithmSettings
//          def graph = generatedCircleGraph
//          def operationMode = Parallel
//        }
//
//        val startCircle = System.currentTimeMillis()
//        circleRunner.run
//        val timeSpentCircle = System.currentTimeMillis() - startCircle
//        println("Cool tester on Circle graph: "+timeSpentCircle + "  millis")



//        println("ALGORYTHM #1 ")
        //        println("Generated with removals")
        //        val allPermutationsRunner = new AlgorithmRunner with AllPermutationsIteratorComponent {
        //          def settings = algorithmSettings
        //          def graph = generatedGraph
        //          def operationMode = Parallel
        //        }
        //        allPermutationsRunner.run
        ////
        //        println("Full graph")
        //        val fullPermutationsRunner = new AlgorithmRunner with AllPermutationsIteratorComponent {
        //          def settings = algorithmSettings
        //          def graph = generatedFullGraph
        //          def operationMode = Parallel
        //        }
        //        fullPermutationsRunner.run
        //
        //        println("Circle graph")
        //        val circlePermutationsRunner = new AlgorithmRunner with AllPermutationsIteratorComponent {
        //          def settings = algorithmSettings
        //          def graph = generatedCircleGraph
        //          def operationMode = Parallel
        //        }
        //        circlePermutationsRunner.run
        //
//        println("ALGORYTHM #2 Random permutations")
        //        println("Generated with removals")
        //        val randomPermutationsRunner = new AlgorithmRunner with RandomPermutationsIteratorComponent{
        //          def settings = algorithmSettings
        //          def graph = generatedGraph
        //          def operationMode = Sequential
        //          def totalToTest = 20000
        //        }
        //        randomPermutationsRunner.run
        //
//        println("Full graph")
//        val randomPermutationsFullRunner = new AlgorithmRunner with RandomPermutationsIteratorComponent{
//          def settings = algorithmSettings
//          def graph = generatedFullGraph
//          def operationMode = Sequential
//          def totalToTest = 20000
//        }
//        randomPermutationsFullRunner.run
//
//        println("Circle graph")
//        val randomPermutationsCircleRunner = new AlgorithmRunner with RandomPermutationsIteratorComponent{
//          def settings = algorithmSettings
//          def graph = generatedCircleGraph
//          def operationMode = Sequential
//          def totalToTest = 20000
//        }
//        randomPermutationsCircleRunner.run
//
//        println("ALGORYTHM #3 Optimal")
        //
        //        println("Generated with removals")
        //        val optimalTestersAlgorithm = new OptimalTesters{
        //          def settings = algorithmSettings
        //          def graph: Graph = generatedGraph
        //        }
        //        optimalTestersAlgorithm.run
        //
//        println("Full graph")
//        val optimalTestersFullAlgorithm = new OptimalTesters{
//          def settings = algorithmSettings
//          def graph: Graph = generatedFullGraph
//        }
//        optimalTestersFullAlgorithm.run
//
//        println("Circle graph")
//        val optimalTestersCircleAlgorithm = new OptimalTesters{
//          def settings = algorithmSettings
//          def graph: Graph = generatedCircleGraph
//        }
//                optimalTestersCircleAlgorithm.run
      }

    }

  }
}
