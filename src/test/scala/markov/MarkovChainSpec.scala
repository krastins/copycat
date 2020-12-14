package markov

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless.Nat._2
import shapeless.Sized

class MarkovChainSpec extends AnyFlatSpec with Matchers {

  val emptyChain: MarkovChain[String, _2] = MarkovChain[String, _2]()

  "transitionProbability" should "be zero if the chain is empty" in {
    emptyChain.transitionProbability(Sized("A", "B"), "C") shouldBe 0
  }

  it should "be zero if the chain doesn't contain the transition" in {
    emptyChain.addTransition(Sized("B", "C"), "D").transitionProbability(Sized("A", "B"), "B") shouldBe 0
  }

  it should "be 1 if it's the only transition in chain" in {
    emptyChain.addTransition(Sized("B", "C"), "D").transitionProbability(Sized("B", "C"), "D") shouldBe 1
  }

  it should "be based on the state transition frequency" in {
    val chain = emptyChain.addTransition(Sized("B", "C"), "D")
      .addTransition(Sized("B", "C"), "E")
      .addTransition(Sized("B", "C"), "E")
      .addTransition(Sized("B", "C"), "F")
      .addTransition(Sized("C", "D"), "E")
      .addTransition(Sized("C", "D"), "F")

    chain.transitionProbability(Sized("B", "C"), "D") shouldBe 0.25
    chain.transitionProbability(Sized("B", "C"), "E") shouldBe 0.5
    chain.transitionProbability(Sized("C", "D"), "F") shouldBe 0.5
  }

  "generateSequence" should "be deterministic if there's only one possible transition for each state" in {
    val chain = emptyChain.addTransition(Sized("A", "B"), "C")
      .addTransition(Sized("B", "C"), "D")
      .addTransition(Sized("C", "D"), "A")
      .addTransition(Sized("D", "A"), "B")
    chain.generateSequence(Sized("A", "B"), 10) shouldBe List("A", "B", "C", "D", "A", "B", "C", "D", "A", "B")
  }
}
