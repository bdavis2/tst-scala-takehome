import bestcruiseprices.BestGroupPrices.*
import promotioncombinations.CombinablePromotions.*

@main def main(): Unit = {
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

  println(
    "Best group prices:\n" + getBestGroupPrices(rates, prices)
      .mkString("\n")
  )

  val promotions = Seq(
    Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")),
    Promotion("P5", Seq("P2"))
  )

  println(
    "All Promotion Combinations:\n" + allCombinablePromotions(promotions)
      .mkString("\n")
  )

  println(
    "All Promotions for P3:\n" + combinablePromotions("P3", promotions)
      .mkString("\n")
  )

  println(
    "All Promotions for P2:\n" + combinablePromotions("P2", promotions)
      .mkString("\n")
  )
}
