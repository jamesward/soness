package shop

class DiscountsSpec extends munit.FunSuite:
  test("codes are found whatever their case"):
    assertEquals(Discounts.lookup("SAVE10").map(_.percent), Some(10))
    assertEquals(Discounts.lookup(" welcome5 ").map(_.percent), Some(5))

  test("unknown codes are not"):
    assertEquals(Discounts.lookup("NOPE"), None)
