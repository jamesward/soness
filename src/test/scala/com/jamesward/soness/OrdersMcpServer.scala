package com.jamesward.soness

import com.jamesward.ziohttp.mcp.*
import zio.*
import zio.http.*
import zio.schema.*

/**
 * A deterministic order-management MCP server, the eval fixture. Customer records are deliberately wide (contact
 * details, notes) so that reading them costs a generative model many tokens, while only a couple of fields matter to
 * each task.
 */
object OrdersMcpServer:

  final case class Customer(
    customerId: String,
    name: String,
    region: String,
    tier: String,
    email: String,
    phone: String,
    city: String,
    accountManager: String,
    signupDate: String,
    creditLimit: Int,
    notes: String,
  ) derives Schema

  final case class CustomerList(customers: List[Customer]) derives Schema

  final case class CustomerIdInput(customerId: String) derives Schema

  final case class Order(orderId: String, customerId: String, status: String, total: Double, placedOn: String) derives Schema

  final case class OrderList(customerId: String, orders: List[Order]) derives Schema

  final case class OrderIdInput(orderId: String) derives Schema

  final case class LineItem(sku: String, quantity: Int) derives Schema

  final case class OrderDetail(
    orderId: String,
    customerId: String,
    status: String,
    carrier: String,
    trackingNumber: String,
    items: List[LineItem],
  ) derives Schema

  private val adjectives = Vector("Acme", "Blue", "Crimson", "Delta", "Evergreen", "Frontier", "Granite", "Harbor",
    "Iron", "Juniper", "Keystone", "Lumen", "Meridian", "Northwind", "Orbit", "Pioneer", "Quarry")
  private val nouns = Vector("Robotics", "Foods", "Logistics", "Textiles", "Analytics", "Pharma", "Energy", "Media",
    "Motors", "Software", "Optics", "Marine", "Ceramics")
  private val suffixes = Vector("Inc", "Ltd", "GmbH", "Co", "Group")
  private val regions = Vector("AMER", "EMEA", "APAC")
  private val cities = Map(
    "AMER" -> Vector("Denver", "Toronto", "Austin", "Chicago"),
    "EMEA" -> Vector("Berlin", "Lyon", "Madrid", "Dublin"),
    "APAC" -> Vector("Osaka", "Sydney", "Singapore", "Pune"),
  )
  private val managers = Vector("Avery Chen", "Blake Moreno", "Casey Okafor", "Devon Lindqvist", "Emery Tanaka")
  private val statuses = Vector("placed", "shipped", "delivered", "cancelled")
  private val carriers = Vector("UPS", "DHL", "FedEx")

  private def tier(i: Int): String =
    if i % 23 == 0 then "Platinum" else if i % 5 == 0 then "Gold" else "Standard"

  val customers: List[Customer] = (1 to 300).toList.map: i =>
    val region = regions(i % 3)
    val name = s"${adjectives(i % 17)} ${nouns(i % 13)} ${suffixes(i % 5)}"
    val handle = name.toLowerCase.replace(" ", "")
    Customer(
      customerId = f"C$i%04d",
      name = name,
      region = region,
      tier = tier(i),
      email = s"orders@$handle.example",
      phone = f"+1-555-${i * 37 % 1000}%03d-${i * 91 % 10000}%04d",
      city = cities(region)(i % 4),
      accountManager = managers(i % 5),
      signupDate = f"20${18 + i % 7}%02d-${1 + i % 12}%02d-${1 + i % 28}%02d",
      creditLimit = 5000 + (i * 733 % 20) * 2500,
      notes = s"Prefers consolidated monthly invoicing. Primary contact rotates quarterly; confirm the purchase order " +
        s"reference before shipping. Account reviewed ${2020 + i % 6}.",
    )

  val orders: List[Order] =
    customers.zipWithIndex.flatMap: (customer, index) =>
      val i = index + 1
      (0 until i % 4 + 1).map: k =>
        Order(
          orderId = s"O${10000 + i * 10 + k}",
          customerId = customer.customerId,
          status = statuses((i + k) % 4),
          total = ((i * 131 + k * 17) % 900 + 50).toDouble,
          placedOn = f"2026-${1 + (i + k) % 9}%02d-${1 + (i * 3 + k) % 28}%02d",
        )

  def detail(order: Order): OrderDetail =
    val n = order.orderId.drop(1).toInt
    val shipped = order.status == "shipped" || order.status == "delivered"
    OrderDetail(
      order.orderId,
      order.customerId,
      order.status,
      if shipped then carriers(n % 3) else "none",
      if shipped then f"TRK${n * 7919 % 1000000}%06d" else "none",
      List(LineItem(f"SKU-${n % 50}%03d", 1 + n % 3), LineItem(f"SKU-${(n * 7) % 50}%03d", 1 + n % 5)),
    )

  /** What the server tells its clients about its tools (`initialize`'s `instructions`). */
  val Instructions = "Order ids look like O10460; customer ids like C0046."

  val server: McpServer[Any] =
    McpServer("orders", "1.0.0")
      .instructions(Instructions)
      .tool(
        McpTool("list_customers")
          .description("List every customer account with its region, tier and contact details.")
          .handle(ZIO.succeed(CustomerList(customers)))
      )
      .tool(
        McpTool("list_orders")
          .description("List the orders of one customer, with their status, total and date.")
          .handle: (input: CustomerIdInput) =>
            ZIO.succeed(OrderList(input.customerId, orders.filter(_.customerId == input.customerId)))
      )
      .tool(
        McpTool("get_order")
          .description("Get one order's shipping carrier, tracking number and line items.")
          .handle[Any, ToolError, OrderIdInput, OrderDetail]: input =>
            ZIO.fromOption(orders.find(_.orderId == input.orderId).map(detail))
              .orElseFail(ToolError(s"no order ${input.orderId}"))
      )

  /** Serves the fixture on a free port for the life of the scope and returns its MCP URL. */
  val serve: ZIO[Scope, Throwable, String] =
    for
      env  <- (Server.defaultWithPort(0) ++ McpServer.State.default).build
      port <- Server.install(server.routes).provideEnvironment(env)
    yield s"http://127.0.0.1:$port/mcp"
