package markov
import SeqOrder.instances._
import cats.syntax.either._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless.Nat._2
import shapeless.{Nat, Sized}

class MarkovChainSpec extends AnyFlatSpec with Matchers {

  val emptyChain: MarkovChain[String, _2] = MarkovChain.empty[String, _2]

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

  "generateSequence" should "try to fallback to one order lower if transition is missing" in {
    val chain = emptyChain
      .addTransition(Sized("F", "A"), "B")
      .addTransition(Sized("A", "B"), "C")
      .addTransition(Sized("B", "C"), "A")
    chain.generateSequence(Sized("A", "B"), 8) shouldBe List("A", "B", "C", "A", "B", "C", "A", "B")
  }

  "generateSequence" should "terminate early if the chain is not looping" in {
    val chain = emptyChain
      .addTransition(Sized("A", "B"), "C")
      .addTransition(Sized("B", "C"), "D")
    chain.generateSequence(Sized("A", "B"), 8) shouldBe List("A", "B", "C", "D")
  }

  //fixme: Take a look on property based specs, should work great here.
  "firstOrder" should "form a first order markov chain from a list" in {
    val input = MarkovInput.ofOrderUnsafe[String, Nat._1](List("A", "B", "C", "D"))
    val chain: MarkovChain[String, Nat._1] = MarkovChain.ofOrder(input.states, input.order)

    chain.generateSequence(Sized("A"), 8) shouldBe List("A", "B", "C", "D", "A", "B", "C", "D")
  }
//
//  "secondOrder" should "form a second order markov chain from a list" in {
//    val chain = MarkovChain.secondOrder(List("A", "B", "C", "D"))
//    chain.generateSequence(Sized("A", "B"), 8) shouldBe List("A", "B", "C", "D", "A", "B", "C", "D")
//  }
//
//  "thirdOrder" should "form a third order markov chain from a list" in {
//    val chain = MarkovChain.thirdOrder(List("A", "B", "C", "D"))
//    chain.generateSequence(Sized("A", "B", "C"), 8) shouldBe List("A", "B", "C", "D", "A", "B", "C", "D")
//  }
//
//  "fourthOrder" should "form a fourth order markov chain from a list" in {
//    val chain = MarkovChain.fourthOrder(List("A", "B", "C", "D", "E"))
//    chain.generateSequence(Sized("A", "B", "C", "D"), 8) shouldBe List("A", "B", "C", "D", "E", "A", "B", "C")
//  }
}
