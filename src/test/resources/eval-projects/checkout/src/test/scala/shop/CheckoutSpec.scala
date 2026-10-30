package shop

class CheckoutSpec extends munit.FunSuite:
  private val order = Order("o1", "c1", List(Line("mug", BigDecimal(12), 2)))

  test("no code: full price plus shipping"):
    assertEquals(Checkout.receipt(order, None).total, BigDecimal("28.99"))

  test("SAVE10 takes 10% off"):
    assertEquals(Checkout.receipt(order, Some("SAVE10")).discount, BigDecimal("2.40"))
