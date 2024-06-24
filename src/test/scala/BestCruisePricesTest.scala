package testbestcruiseprices

import bestcruiseprices.BestGroupPrices.*
import org.scalatest.funsuite.AnyFunSuite

class BestCruisePricesTest extends AnyFunSuite {

  // test expected output
  test("function has expected output ") {
    val rates: Seq[Rate] = Seq(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )
    val prices: Seq[CabinPrice] = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    val correctBestGroupPrices: Seq[BestGroupPrice] = Seq(
      BestGroupPrice("CA","M1",200.0,"Military"),
      BestGroupPrice("CA","S1",225.0,"Senior"),
      BestGroupPrice("CB","M1",230.0,"Military"),
      BestGroupPrice("CB","S1",245.0,"Senior"))

    assert(getBestGroupPrices(rates, prices) == correctBestGroupPrices)
  }
  test("handles empty price list") {
    val rates: Seq[Rate] = Seq(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )
    val prices: Seq[CabinPrice] = Seq()
    assertThrows[RuntimeException](getBestGroupPrices(rates, prices))
  }

  test("hits `Group not found` exception with empty rate list") {
    val rates: Seq[Rate] = Seq()

    val prices: Seq[CabinPrice] = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )
    assertThrows[RuntimeException](getBestGroupPrices(rates, prices))

  }
}
