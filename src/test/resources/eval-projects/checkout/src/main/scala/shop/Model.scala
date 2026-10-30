package shop

final case class Line(sku: String, unitPrice: BigDecimal, quantity: Int)
final case class Order(id: String, customerId: String, lines: List[Line])
final case class Discount(code: String, percent: Int)
final case class Receipt(orderId: String, subtotal: BigDecimal, discount: BigDecimal, shipping: BigDecimal, total: BigDecimal)
