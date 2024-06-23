object CombinablePromotions {
  case class Promotion(code: String, notCombinableWith: Seq[String])

  case class PromotionCombo(promotionCodes: Seq[String])

  def main(args: Array[String]): Unit = {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    println(
      "All Promotion Combinations:\n" + allCombinablePromotions(promotions)
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

  def allCombinablePromotions(
                               allPromotions: Seq[Promotion]
                             ): Seq[PromotionCombo] = {
    val notCombinableMap = allPromotions
      .map(promotion => promotion.code -> promotion.notCombinableWith.toSet)
      .toMap

    // recursive helper function
    def generateCombos(
                        promotionCodes: Seq[String],
                        remainingPromotions: Seq[Promotion]
                      ): Seq[PromotionCombo] = {
      if (remainingPromotions.isEmpty) {
        Seq(PromotionCombo(promotionCodes))
      } else {
        val promotion = remainingPromotions.head
        // Check if the current promotion can be added to the current combination
        if (promotionCodes.forall(code => !notCombinableMap(promotion.code).contains(code))) {
          // generate combinations with promotion
          val withPromotion = generateCombos(promotionCodes :+ promotion.code, remainingPromotions.tail)
          // generate combinations without promotion
          val withoutPromotion = generateCombos(promotionCodes, remainingPromotions.tail)
          withPromotion ++ withoutPromotion
        } else {
          generateCombos(promotionCodes, remainingPromotions.tail)
        }
      }
    }

    // Generate all possible promotion combos
    val allCombos = generateCombos(Seq(), allPromotions)

    // filter out all subsequences
    allCombos.filterNot { combo =>
      allCombos.exists { other =>
        other != combo && combo.promotionCodes.toSet.subsetOf(
          other.promotionCodes.toSet
        )
      }
    }
  }

  def combinablePromotions(
                            promotionCode: String,
                            allPromotions: Seq[Promotion]
                          ): Seq[PromotionCombo] = {
    allCombinablePromotions(allPromotions).filter(
      _.promotionCodes.contains(promotionCode)
    )
  }

}