package shop

/** The discount codes customers can enter at checkout. */
object Discounts:
  private val active: Map[String, Discount] = Map(
    "save10" -> Discount("save10", 10),
    "welcome5" -> Discount("welcome5", 5),
  )

  /** The discount for a code as the customer typed it, if it is active. */
  def lookup(code: String): Option[Discount] = active.get(code.trim.toLowerCase)
