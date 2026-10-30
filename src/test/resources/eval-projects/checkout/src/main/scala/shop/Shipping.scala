package shop

object Shipping:
  /** Orders whose subtotal, after discounts, is at least this ship free. */
  val FreeAbove: BigDecimal = BigDecimal(50)
  val Flat: BigDecimal = BigDecimal("4.99")

  def cost(subtotal: BigDecimal, code: Option[String]): BigDecimal =
    if subtotal >= FreeAbove || code.exists(_.trim.equalsIgnoreCase("FREESHIP")) then BigDecimal(0) else Flat
