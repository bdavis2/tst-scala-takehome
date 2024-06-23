object BestGroupPrices {

  def main(args: Array[String]): Unit = {
    val rates: Seq[Rate] = Seq(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )

    val prices: Seq[CabinPrice] = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    for (e <- getBestGroupPrices(rates, prices)) {
      println(e)
    }
  }

  case class Rate(rateCode: String, rateGroup: String)

  case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)

  case class BestGroupPrice(
                             cabinCode: String,
                             rateCode: String,
                             price: BigDecimal,
                             rateGroup: String
                           )

  def getBestGroupPrices(
                          rates: Seq[Rate],
                          prices: Seq[CabinPrice]
                        ): Seq[BestGroupPrice] = {
    prices
      .groupBy(price => price.cabinCode)
      .map { (cabinCode, prices) =>
        (
          cabinCode,
          prices.groupBy(price =>
            rates
              .find(rate => {
                rate.rateCode == price.rateCode
              })
              .getOrElse(throw new RuntimeException("Group not found"))
              .rateGroup
          )
        )
      } // get min for each rate group per cabin
      .map { (cabinCode, rateGroup) =>
        (
          cabinCode,
          rateGroup.map { (rateGroup, cabinPrices) =>
            (rateGroup, cabinPrices.minBy(_.price))
          }
        )
      } // flatten back to seq of bestGroupPrices
      .flatMap { (cabinCode, innerMap) =>
        innerMap.map { (rateGroup, cabinPrice) =>
          BestGroupPrice(
            cabinCode = cabinPrice.cabinCode,
            rateCode = cabinPrice.rateCode,
            price = cabinPrice.price,
            rateGroup = rateGroup
          )
        }
      }
      .toSeq
      .sortBy(bp => (bp.cabinCode, bp.rateCode)) // sort increasing
  }
}


