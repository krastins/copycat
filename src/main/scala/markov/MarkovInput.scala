package markov

import shapeless.{Nat, Sized}
import SeqOrder._
import scala.collection.immutable.List

final case class MarkovInput[S, N <: Nat] private (init: Sized[Seq[S], N], states: List[S], order: SeqOrder)

object MarkovInput {

  final case class InputIsTooShort[A](input: List[A], order: SeqOrder)

  private def apply[S, N <: Nat](input: List[S], order: SeqOrder): MarkovInput[S, N] = {
    val init = Sized.wrap[List[S], N](input.take(order.id))
    val chained = input ++ input.take(order.id)
    new MarkovInput(init, chained, order)
  }

  def ofOrder[S, N <: Nat : HasOrder](input: List[S]): Either[InputIsTooShort[S], MarkovInput[S, N]] = {
    val hasOrder = implicitly[HasOrder[N]]
    import hasOrder._

    Either.cond(input.length >= order.id, MarkovInput[S, N](input, order), InputIsTooShort(input, order))
  }

  private[markov] def ofOrderUnsafe[S, N <: Nat : HasOrder](input: List[S]): MarkovInput[S, N] = ofOrder(input) match {
    case Right(input) => input
    case Left(error) => throw new Exception(error.toString)
  }
}
