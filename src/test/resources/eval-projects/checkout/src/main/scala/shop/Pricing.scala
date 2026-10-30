package shop

object Pricing:
  def subtotal(order: Order): BigDecimal = order.lines.map(l => l.unitPrice * l.quantity).sum

  /** The amount a discount takes off a subtotal, rounded to the cent. */
  def discountAmount(subtotal: BigDecimal, discount: Option[Discount]): BigDecimal =
    discount.fold(BigDecimal(0))(d => (subtotal * d.percent / 100).setScale(2, BigDecimal.RoundingMode.HALF_UP))
