package markov

import shapeless.{Nat, Sized}
import shapeless.Nat.{_1, _2, _3, _4}

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.util.Random

case class MarkovChain[S, N <: Nat](transitions: Map[Sized[Seq[S], N], TransitionCount[S]]) {

  def addTransition(from: Sized[Seq[S], N], to: S): MarkovChain[S, N] =
    MarkovChain(transitions.updatedWith(from)(maybeOld =>
      maybeOld.orElse(Some(TransitionCount.empty[S]))
        .map(_.addTransition(to))))

  def transitionProbability(from: Sized[Seq[S], N], to: S): Double =
    transitions.get(from).map(_.getProbability(to)).getOrElse(0)

  def transitionsFor(state: Sized[Seq[S], N]): Option[List[(S, Double)]] =
    transitions.get(state).map(_.toList)

  // TODO: Think about a nicer way to do this
  def fallbackTransitions(state: Sized[Seq[S], N]): List[(S, Double)] = {
    if(state.size > 1) {
      transitions.filter { case (k: Sized[Seq[S], N], _) => k.tail == state.tail }
        .values.flatMap(_.toList).toList
    } else List.empty[(S, Double)]
  }

  def states(): Iterable[Sized[Seq[S], N]] = transitions.keys

  def generateNext(s: Sized[Seq[S], N]): Option[S] = {
    val probabilities: List[S] = for {
      (state, probability) <- transitionsFor(s).getOrElse(fallbackTransitions(s))
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
        case Some(next) =>
          val nextFrom: Sized[Seq[S], N] = Sized.wrap(from.toList.tail :+ next)
          generateSequence(nextFrom, length - 1, from.unsized.head :: sequence)
        case None => sequence.reverse ::: from.toList
      }
    else sequence.reverse
}
object MarkovChain {
  def empty[S, N <: Nat]: MarkovChain[S, N] =
    MarkovChain(Map.empty[Sized[Seq[S], N], TransitionCount[S]])

  def accumulateTransitions[S, N <: Nat](transitions: Seq[(Sized[Seq[S], N], S)]): MarkovChain[S, N] =
    transitions.foldLeft(MarkovChain.empty[S, N])((chain, tup) => tup match {
      case (from, to) => chain.addTransition(from, to)
    })

  def firstOrder[S](states: List[S]): MarkovChain[S, _1] = {
    val transitions = (states ++ states.take(1)).sliding(2, 1).toList
        .map { case List(a, b) => Sized(a) -> b }
    accumulateTransitions[S, _1](transitions)
  }

  def secondOrder[S](states: List[S]): MarkovChain[S, _2] = {
    val transitions = (states ++ states.take(2)).sliding(3, 1).toList
        .map { case List(a, b, c) => Sized(a, b) -> c }
    accumulateTransitions[S, _2](transitions)
  }

  def thirdOrder[S](states: List[S]): MarkovChain[S, _3] = {
    val transitions = (states ++ states.take(3)).sliding(4, 1).toList
        .map { case List(a, b, c, d) => Sized(a, b, c) -> d }
    accumulateTransitions[S, _3](transitions)
  }

  def fourthOrder[S](states: List[S]): MarkovChain[S, _4] = {
    val transitions = (states ++ states.take(4)).sliding(5, 1).toList
        .map { case List(a, b, c, d, e) => Sized(a, b, c, d) -> e }
    accumulateTransitions[S, _4](transitions)
  }

  def generateSequenceOfOrder[S](input: List[S], order: Int, length: Int): List[S] = (order match {
    case 1 => MarkovChain.firstOrder(input)
    case 2 => MarkovChain.secondOrder(input)
    case 3 => MarkovChain.thirdOrder(input)
    case 4 => MarkovChain.fourthOrder(input)
  }).generateSequence(Sized.wrap(input.take(order)), length)
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
  def empty[S]: TransitionCount[S] = TransitionCount(Map.empty[S, Int])
}

