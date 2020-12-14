package markov

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.util.Random

case class MarkovChain[S](transitions: Map[S, TransitionCount[S]] = Map[S, TransitionCount[S]]()) {

  def addTransition(from: S, to: S): MarkovChain[S] =
    MarkovChain(transitions.updatedWith(from)(maybeOld =>
      maybeOld.orElse(Some(TransitionCount.empty[S]))
        .map(_.addTransition(to))))

  def transitionProbability(from: S, to: S): Double =
    transitions.get(from).map(_.getProbability(to)).getOrElse(0)

  def transitionsFor(state: S): List[(S, Double)] =
    transitions.get(state).map(_.toList).getOrElse(List[(S, Double)]())

  def states(): Iterable[S] = transitions.keys

  def generateNext(s: S): Option[S] = {
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

  // TODO: Higher order chains
  @tailrec
  final def generateSequence(from: S, length: Int, sequence: List[S] = List.empty[S]): List[S] =
    if (length > 0)
      generateNext(from) match {
        case Some(next) => generateSequence(next, length - 1, from :: sequence)
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

