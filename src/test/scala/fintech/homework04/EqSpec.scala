package fintech.homework04
import org.scalatest.{FlatSpec, Matchers}
import fintech.homework06.EqSyntax._

class EqSpec extends FlatSpec with Matchers {
  it should "work correctly with Option" in {
    Option(7) ==== Option(7) should be (true)
    Option(3) ==== Option(0) should be (false)
    Option("success") ==== Option("success") should be (true)
    Option("fail") ==== Option("success") should be (false)
  }

  it should "work correctly with Seq" in {
    Seq(1, 2, 3) === Seq(1, 2, 3) should be (true)
    Seq(1, 2, 3, 4) ==== Seq(1, 2, 3) should be (false)
    Seq("Legen", "dary", 5) === Seq("Legen", "dary", 5) should  be (true)
    Seq("Legen", "dary") === Seq("Legendary", 47) should be (false)
  }

  it should "work correctly with Map" in {
    Map("42" -> 42, "13" -> 13) ==== Map("13" -> 13, "42" -> 42) should be (true)
    Map("42" -> 42, "13" -> 13) ==== Map("42" -> 42, "1" -> 13) should be (false)
    Map("42" -> Option(42), "13" -> Option(13)) ==== Map("42" -> Option(42), "1" -> Option(13)) should be (false)
  }
}
