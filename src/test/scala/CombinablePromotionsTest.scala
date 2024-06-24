package testcombinablepromotions

import promotioncombinations.CombinablePromotions.*
import org.scalatest.funsuite.AnyFunSuite

class CombinablePromotionsTest extends AnyFunSuite {

  // test expected output
  test("allCombinablePromotions function has expected output") {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )
    val expected = List(PromotionCombo(List("P1", "P2")), PromotionCombo(List("P1", "P4", "P5")), PromotionCombo(List("P2", "P3")), PromotionCombo(List("P3", "P4", "P5")))
    assert(allCombinablePromotions(promotions) == expected)
  }

  test("handles empty promotion list") {
    val promotions = Seq()

    assert(allCombinablePromotions(promotions) == List(PromotionCombo(List())))
  }
  test("combinablePromotions function has expected output"){
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val expected = List(PromotionCombo(List("P1", "P2")), PromotionCombo(List("P1", "P4", "P5")))

    assert(combinablePromotions("P1", promotions) == expected)
  }

  test("combinablePromotions function fails on promotion that doesn't exist"){
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )
    
    assert(combinablePromotions("P6", promotions) == List())
  }

}
