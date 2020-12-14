package markov

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkovChainSpec extends AnyFlatSpec with Matchers {

  val emptyChain: MarkovChain[String] = MarkovChain[String]()

  "transitionProbability" should "be zero if the chain is empty" in {
    emptyChain.transitionProbability("A", "B") shouldBe 0
  }

  it should "be zero if the chain doesn't contain the transition" in {
    emptyChain.addTransition("C", "D").transitionProbability("A", "B") shouldBe 0
  }

  it should "be 1 if it's the only transition in chain" in {
    emptyChain.addTransition("C", "D").transitionProbability("C", "D") shouldBe 1
  }

  it should "be based on the state transition frequency" in {
    val chain = emptyChain.addTransition("C", "D")
      .addTransition("C", "E")
      .addTransition("C", "E")
      .addTransition("C", "F")
      .addTransition("D", "E")
      .addTransition("D", "F")

    chain.transitionProbability("C", "D") shouldBe 0.25
    chain.transitionProbability("C", "E") shouldBe 0.5
    chain.transitionProbability("D", "F") shouldBe 0.5
  }

  "generateSequence" should "be deterministic if there's only one possible transition for each state" in {
    val chain = emptyChain.addTransition("A", "B")
      .addTransition("B", "C")
      .addTransition("C", "A")
    chain.generateSequence("A", 10) shouldBe List("A", "B", "C", "A", "B", "C", "A", "B", "C", "A")
  }

}
