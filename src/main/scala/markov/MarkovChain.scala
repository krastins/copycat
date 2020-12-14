package markov

import shapeless.{Nat, Sized, _}

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.util.Random

case class MarkovChain[S, N <: Nat](transitions: Map[Sized[Seq[S], N], TransitionCount[S]] = Map[Sized[Seq[S], N], TransitionCount[S]]()) {
  
  def addTransition(from: Sized[Seq[S], N], to: S): MarkovChain[S, N] =
    MarkovChain(transitions.updatedWith(from)(maybeOld =>
      maybeOld.orElse(Some(TransitionCount.empty[S]))
        .map(_.addTransition(to))))

  def transitionProbability(from: Sized[Seq[S], N], to: S): Double =
    transitions.get(from).map(_.getProbability(to)).getOrElse(0)

  def transitionsFor(state: Sized[Seq[S], N]): List[(S, Double)] =
    transitions.get(state).map(_.toList).getOrElse(List[(S, Double)]())

  def states(): Iterable[Sized[Seq[S], N]] = transitions.keys

  def generateNext(s: Sized[Seq[S], N]): Option[S] = {
    val probabilities: List[S] = for {
      (state, probability) <- transitionsFor(s)
      frequency = Math.round(probability * 100).toInt
      nextState <- List.fill(frequency)(state)
    } yield nextState

    probabilities.lift(probabilities.length match {
      case 0 => 0
      case nonZero => Random.nextInt(nonZero)
    })
  }

  @tailrec
  final def generateSequence(from: Sized[Seq[S], N], length: Int, sequence: List[S] = List.empty[S]): List[S] =
    if (length > 0)
      generateNext(from) match {
        case Some(next: S) =>
          val nextFrom: Sized[Seq[S], N] = Sized.wrap(from.toList.tail :+ next)
          generateSequence(nextFrom, length - 1, from.unsized.head :: sequence)
        case None => sequence.reverse
      }
    else sequence.reverse
}

case class TransitionCount[S] (transitionCount: Map[S, Int]) {
  private def incrementCount(count: Option[Int]): Option[Int] =
    count.map(_ + 1).orElse(Some(1))

  def addTransition(state: S): TransitionCount[S] =
    TransitionCount(transitionCount.updatedWith(state)(incrementCount))

  def getCount(state: S): Int = transitionCount.getOrElse(state, 0)

  def totalCount(): Double = transitionCount.values.sum

  def getProbability(state: S): Double =
    getCount(state).toDouble / totalCount()

  def toList: List[(S, Double)] = {
    transitionCount.toList.map(transition =>
      transition.copy(_2 = transition._2.toDouble / totalCount()))
  }
}

object TransitionCount {
  def empty[S]: TransitionCount[S] = new TransitionCount(Map[S, Int]())
}

