package shop

class ShippingSpec extends munit.FunSuite:
  test("flat rate below the threshold"):
    assertEquals(Shipping.cost(BigDecimal(20), None), BigDecimal("4.99"))

  test("free at 50"):
    assertEquals(Shipping.cost(BigDecimal(50), None), BigDecimal(0))

  test("free with FREESHIP"):
    assertEquals(Shipping.cost(BigDecimal(20), Some("freeship")), BigDecimal(0))
