package markov

import shapeless.Nat


object SeqOrder extends Enumeration(1) {
  type SeqOrder = Value

  val First, Second, Third, Fourth = Value

  object instances {
    implicit val forNat1: HasOrder[Nat._1] = new HasOrder[Nat._1] {
      override val order: SeqOrder = First
    }

    implicit val forNat2: HasOrder[Nat._2] = new HasOrder[Nat._2] {
      override val order: SeqOrder = Second
    }

    implicit val forNat3: HasOrder[Nat._3] = new HasOrder[Nat._3] {
      override val order: SeqOrder = Third
    }

    implicit val forNat4: HasOrder[Nat._4] = new HasOrder[Nat._4] {
      override val order: SeqOrder = Fourth
    }
  }
}
