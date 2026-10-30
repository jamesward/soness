package shop

/** Turns an order and the code the customer entered into a receipt. */
object Checkout:
  def receipt(order: Order, code: Option[String]): Receipt =
    val subtotal = Pricing.subtotal(order)
    val discount = code.flatMap(Discounts.lookup)
    val off = Pricing.discountAmount(subtotal, discount)
    val shipping = Shipping.cost(subtotal - off, code)
    Receipt(order.id, subtotal, off, shipping, subtotal - off + shipping)
