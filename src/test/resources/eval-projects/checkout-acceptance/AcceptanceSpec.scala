package shop.acceptance

import shop.*

/** The eval's hidden acceptance tests: what the user asked for, checked through the public checkout. */
class AcceptanceSpec extends munit.FunSuite:
  private def order(lines: (BigDecimal, Int)*) =
    Order("o", "c", lines.toList.zipWithIndex.map { case ((price, qty), i) => Line(s"sku$i", price, qty) })

  // receipts are read by field name, so a missing field fails its own tests rather than compiling none
  private def field(receipt: Receipt, name: String): Option[Any] =
    receipt.productElementNames.zip(receipt.productIterator).toMap.get(name)

  test("shipping: free from 75"):
    assertEquals(Checkout.receipt(order(BigDecimal(75) -> 1), None).shipping, BigDecimal(0))
  test("shipping: charged below 75"):
    assertEquals(Checkout.receipt(order(BigDecimal("74.99") -> 1), None).shipping, BigDecimal("4.99"))
  test("shipping: charged at 50 now"):
    assertEquals(Checkout.receipt(order(BigDecimal(50) -> 1), None).shipping, BigDecimal("4.99"))
  test("shipping: FREESHIP is still free"):
    assertEquals(Checkout.receipt(order(BigDecimal(10) -> 1), Some("FREESHIP")).shipping, BigDecimal(0))

  test("bulk: BULK15 takes 15% off 10 or more items"):
    assertEquals(Checkout.receipt(order(BigDecimal(2) -> 10), Some("BULK15")).discount, BigDecimal("3.00"))
  test("bulk: counts items across lines"):
    assertEquals(Checkout.receipt(order(BigDecimal(1) -> 6, BigDecimal(1) -> 4), Some("bulk15")).discount, BigDecimal("1.50"))
  test("bulk: nothing off 9 items"):
    assertEquals(Checkout.receipt(order(BigDecimal(2) -> 9), Some("BULK15")).discount, BigDecimal(0))
  test("bulk: SAVE10 and WELCOME5 still apply to any order"):
    assertEquals(Checkout.receipt(order(BigDecimal(12) -> 1), Some("SAVE10")).discount, BigDecimal("1.20"))
    assertEquals(Checkout.receipt(order(BigDecimal(20) -> 1), Some("welcome5")).discount, BigDecimal("1.00"))

  test("itemCount: a receipt counts the items"):
    assertEquals(field(Checkout.receipt(order(BigDecimal(1) -> 3, BigDecimal(2) -> 2), None), "itemCount"), Some(5))
  test("itemCount: an empty order has none"):
    assertEquals(field(Checkout.receipt(order(), None), "itemCount"), Some(0))
  test("itemCount: totals still add up"):
    val r = Checkout.receipt(order(BigDecimal(30) -> 3), Some("SAVE10"))
    assertEquals(r.total, r.subtotal - r.discount + r.shipping)
