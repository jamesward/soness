package shop

/** A day of orders through checkout, end to end. Slow: it stands in for the shop's integration suite. */
class CheckoutSimulationSpec extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(2, "min")

  private val codes = List(None, Some("SAVE10"), Some("welcome5"), Some("FREESHIP"), Some("nope"))

  test("every receipt adds up, and none is negative"):
    val orders = (1 to 5000).map: i =>
      Order(s"o$i", s"c${i % 97}", (1 to (i % 7 + 1)).toList.map(j => Line(s"sku$j", BigDecimal(i % 40 + j) / 4, i % 5 + 1)))
    orders.zipWithIndex.foreach: (order, i) =>
      val r = Checkout.receipt(order, codes(i % codes.size))
      assertEquals(r.total, r.subtotal - r.discount + r.shipping)
      assert(r.total >= 0 && r.discount >= 0 && r.shipping >= 0)
    // the integration suite's external systems (payments, inventory) take their time
    Thread.sleep(20000)
