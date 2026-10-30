package shop

@main def demo(code: String*): Unit =
  val order = Order("o1", "c1", List(Line("mug", BigDecimal(12), 2), Line("tea", BigDecimal("7.50"), 1)))
  println(Checkout.receipt(order, code.headOption))
