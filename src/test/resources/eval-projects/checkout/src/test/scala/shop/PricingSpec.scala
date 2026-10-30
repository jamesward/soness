package shop

class PricingSpec extends munit.FunSuite:
  test("subtotal sums the lines"):
    assertEquals(Pricing.subtotal(Order("o", "c", List(Line("a", BigDecimal(2), 3), Line("b", BigDecimal("1.50"), 2)))), BigDecimal(9))

  test("a discount is rounded to the cent"):
    assertEquals(Pricing.discountAmount(BigDecimal("9.99"), Some(Discount("x", 10))), BigDecimal("1.00"))
