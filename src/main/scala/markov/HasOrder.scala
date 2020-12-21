package markov

import markov.SeqOrder.SeqOrder

trait HasOrder[T] {
  val order: SeqOrder
}
