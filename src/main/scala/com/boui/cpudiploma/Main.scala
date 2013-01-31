package com.boui.cpudiploma

import util.Random
import org.paukov.combinatorics.Factory
import scala.collection.JavaConverters._
import annotation.tailrec
import scala.math._
import com.boui.cpudiploma.GraphGenerator.Graph

trait AlgorithmSettings{
  def k:Int
  def n:Int
  def timeTransfer:Double
  def timeTesting:Double
}

object GraphGenerator{
  val random = new Random()

  case class Graph(adjacency:Array[Array[Boolean]]){
    def size = adjacency.length

    private val Inf = 10000

    def minWithInf(a:Int, b:Int)={
      (a, b) match {
        case (x,y) if (x>=Inf) => y
        case (x,y) if (y>=Inf) => x
        case _ => min(a,b)
      }
    }

    lazy val distances:Array[Array[Int]] = {
      val dist = Array.fill[Int](size,size)(Inf)
      for (i<-0 until size; j<-0 until size){
        dist(i)(j)= if (adjacency(i)(j)==true) 1 else Inf
      }
      for (i<-0 until size){
        dist(i)(i)=0
      }

      for(k<-0 until size;i<-0 until size; j<-0 until size){
        dist(i)(j) = minWithInf(dist(i)(j), dist(i)(k)+dist(k)(j))
      }
      dist
    }

    private def matrixToString[T](m: Array[Array[T]], t: (T)=>String)=m
      .map(_.map(t(_))
      .mkString(" "))
      .mkString("\n")

    override def toString: String = matrixToString(adjacency, {item:Boolean=>if (item) "T" else "F"})

    def distancesToString: String = matrixToString(distances, {item:Int=>"%3d".format(item)})
  }

  object Graph{

    /**
     *     wave algorithm with painting and queue
     */
    def isBound(graph:Graph):Boolean={
      val visited = Array.ofDim[Boolean](graph.size)
      val neighbours = collection.mutable.Queue[Int](0)
      while (!neighbours.isEmpty){
        val item = neighbours.dequeue()
        visited(item) = true
        neighbours.enqueue(graph.adjacency(item)
          .zipWithIndex
          .filter(i=>i._1 && !visited(i._2))
          .map(_._2):_*)
      }
      visited.forall(_==true)
    }

    /**
     * Try-fail algorithms, builds complete graph then removes
     * edges until
     */
    def generate(size:Int) =  {
      val graph = Graph(Array.fill[Boolean](size,size)(true))
      for (i<-0 until size){
        graph.adjacency(i)(i)=false
      }
      var failsLeft = 5
      while(failsLeft>=0){
        val i =  random.nextInt(graph.size-1)+1
        val j = random.nextInt(i)
        graph.adjacency(i)(j) = false
        graph.adjacency(j)(i) = false
        if (!isBound(graph)){
          graph.adjacency(i)(j) = true
          graph.adjacency(j)(i) = true
          failsLeft -= 1
        }
      }
      graph
    }
  }
}

trait PermutationsIteratorComponent{
  case class Solution(cpus: Array[Set[Int]], t:Double)

  def genPermutationsForCpu(permutation:Set[Int], cpuIndex:Int)= {
    permutation.map(cpu=>if (cpu>=cpuIndex) cpu+1 else cpu)
  }

  def iterate(k:Int, size:Int, cnt:Array[Set[Int]], evaluateCurrent: ()=>Unit):Unit
}

trait AllPermutationsIteratorComponent extends PermutationsIteratorComponent{
  def buildPermutations(k:Int, size: Int)={
    val initialVector = Factory.range(size)
    val generator = Factory.createSimpleCombinationGenerator(initialVector, k)
    generator.generateAllObjects().asScala.toList.map{item=>
      item.asScala.map(_.asInstanceOf[Int]-1).toSet
    }
  }

  def iterate(k: Int, size: Int, cnt:Array[Set[Int]], evaluateCurrent: ()=> Unit){
    val seedPermutations = buildPermutations(k, size-1)

    def iterateForLevel(level:Int){
      if (level==size){
        evaluateCurrent()
      }else{
        val permutations = seedPermutations.map(permutation=>genPermutationsForCpu(permutation, level))
        for (permutation<-permutations){
          cnt(level) = permutation
          iterateForLevel(level+1)
        }
      }
    }

    iterateForLevel(0)

  }
}

trait RandomPermutationsIteratorComponent extends PermutationsIteratorComponent{
  def totalToTest: Int
  private val rnd = new Random

  def genPermutation(k:Int, n:Int)={
    @tailrec
    def improveSet(current:Set[Int]):Set[Int]={
      if (current.size==k){
        current
      }else{
        val nextItem = rnd.nextInt(n)
        improveSet(current+nextItem)
      }
    }

    improveSet(Set())
  }

  def iterate(k: Int, size: Int, cnt: Array[Set[Int]], evaluateCurrent: () => Unit) {
    for (_<-0 until totalToTest){
      for (i<-0 until size){
        cnt(i) = genPermutationsForCpu(genPermutation(k,size-1),i)
      }
      evaluateCurrent()
    }
  }
}

trait SolutionEvaluator{
  def graph:Graph
  def settings:AlgorithmSettings

  def getTestTime(cnt:Iterable[Set[Int]])(op:Iterable[Double]=>Double)={
    val whoItest = (0 until settings.n).map(cpu=>cnt.zipWithIndex.filter(_._1.contains(cpu)).map(_._2))
    val tTr = whoItest.zipWithIndex.map(i=>i._1.map(j=>graph.distances(i._2)(j)).sum*settings.timeTransfer)
    val tEv = whoItest.map(i=>i.size*settings.timeTesting)
    op(tTr.zip(tEv).map(i=>i._1+i._2+settings.timeTesting))
  }
}

abstract sealed class OperationMode{
  def aggregationOperation:Iterable[Double]=>Double
}
case object Parallel extends OperationMode{
  def aggregationOperation: (Iterable[Double]) => Double = _.max
}
case object Sequential extends OperationMode{
  def aggregationOperation: (Iterable[Double]) => Double = _.sum
}

trait AlgorithmRunner extends SolutionEvaluator{ self: PermutationsIteratorComponent=>
   import GraphGenerator._

   def graph:Graph
   def settings:AlgorithmSettings
   def operationMode:OperationMode

   def run{

     def findSolution():Solution={
       val cnt = Array.ofDim[Set[Int]](settings.n)
       val optimal = Array.ofDim[Set[Int]](settings.n)
       var optimalValue = Double.MaxValue

       def storeOptimal{
         cnt.copyToArray(optimal)
       }

       def evaluateCurrent{
         val tTst = getTestTime(cnt)(operationMode.aggregationOperation)
         if (tTst<optimalValue){
           optimalValue = tTst
           storeOptimal
         }
       }

       iterate(settings.k, settings.n, cnt, evaluateCurrent _)

       Solution(optimal,optimalValue)
     }

     val solution = findSolution()
     println("Solution: "+solution.cpus.toList)
     println("Time: "+solution.t)
   }
}

trait OptimalTesters extends SolutionEvaluator{
  def graph:Graph
  def settings:AlgorithmSettings

  def run(){
    val solutions = for (i<-0 until settings.n) yield {
      (0 until settings.k).foldLeft(Set[Int]()){(foundCpus, itemIndex)=>
        val nextOptimalTester = graph.distances(i)
          .view
          .zipWithIndex
          .filter(_._2!=i)
          .filter(cpu=> !foundCpus.contains(cpu._2))
          .sortBy(_._1)
          .map(_._2)
          .head
        foundCpus + nextOptimalTester
      }
    }
    val maxTestingTime = getTestTime(solutions)(_.sum)
    println("Solution: "+solutions.toList)
    println("Time: "+maxTestingTime)
  }
}

object Main{
  def main(argv: Array[String]){
    val algorithmSettings = new AlgorithmSettings {
      def n = 6
      def timeTesting = 0.2
      def k = 3
      def timeTransfer = 0.1
    }

    val generatedGraph = GraphGenerator.Graph.generate(algorithmSettings.n)
    println("Graph:\n"+generatedGraph)
    println("Distances:\n"+generatedGraph.distancesToString)

    val allPermutationsRunner = new AlgorithmRunner with AllPermutationsIteratorComponent{
      def settings = algorithmSettings
      def graph = generatedGraph
      def operationMode = Parallel
    }
    allPermutationsRunner.run

    val randomPermutationsRunner = new AlgorithmRunner with RandomPermutationsIteratorComponent{
      def settings = algorithmSettings
      def graph = generatedGraph
      def operationMode = Sequential
      def totalToTest = 20000
    }
    randomPermutationsRunner.run

    val optimalTestersAlgorithm = new OptimalTesters{
      def settings = algorithmSettings
      def graph: Graph = generatedGraph
    }
    optimalTestersAlgorithm.run
  }
}