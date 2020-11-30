package codingtest

import scala.io.Source

object CodingTest extends App {

  val inputFile = Source.fromResource("input_data.csv").getLines().toList.tail
  val groupFile = Source.fromResource("groups.csv").getLines().toList.tail
  val rows: List[InputRow] = inputFile.map(InputRow.apply)
  val groups: List[GroupRow] = groupFile.map(GroupRow.apply)

  // 1. How many unique buy/sell combinations are there?
  val uniqueBuySell: Int = rows.groupBy(r => (r.buy, r.sell)).keySet.size
  println("Unique BuySell Combination: " + uniqueBuySell)

  // 2. How many accounts executed at least 500 transactions?
  val accountsMin500: Int = rows.groupBy(_.account).filter { case (account, rows) =>
    rows.length >= 500
  }.keySet.size
  println("Accounts with at-least 500 transactions: " + accountsMin500)

  // 3. (Bonus) Which account has the 3rd highest average transaction amount?
  val avgTransactionAmount = rows.groupBy(_.account).map { case (account, rows) =>
    val transAmount: Double = if (rows.nonEmpty) {
      (rows.map(_.amount).fold(0L)(_ + _) / rows.length).toDouble
    }
    else 0
    (account, transAmount)
  }

  val thirdHighestTransactionAccount: Option[(String, Double)] =
  avgTransactionAmount.toSeq.sortBy(_._2).reverse.take(3).lift(2)
  println("Account with 3rd highest transaction amount: \n" +
    s"Account: ${thirdHighestTransactionAccount.map(_._1).getOrElse("No Account")}, " +
      s"Transaction Amount: ${thirdHighestTransactionAccount.map(_._2).getOrElse(0D)}")


  //4. which group sold the most GOOG stock?
    val byAmount: Map[String, Long] =
      rows.filter(_.sell.equals("GOOG")).groupBy(_.account).map { case (a, rows) => (a, rows.map(_.amount).sum) }

  val groupMap: Map[String, Long] =
    groups.groupBy(_.group).map { case (g, accounts) =>
    val amounts: Long = accounts.map { a => byAmount.getOrElse(a.account, 0L) }.sum
    (g, amounts)
  }

  val groupWithHighest = groupMap.toSeq.sortBy(_._2).reverse.head

  println("Group with Highest Transaction Amount: " + groupWithHighest._1  + ": " + groupWithHighest._2)

}

case class InputRow(sell: String, buy: String, amount: Long, account: String) extends Product with Serializable
object InputRow {
  def apply(line: String): InputRow = {
    val records = line.split(",")
    InputRow(records(0), records(1), records(2).toLongOption.getOrElse(0L), records(3))
  }
}

case class GroupRow(account: String, group: String) extends Product with Serializable
object GroupRow {
  def apply(line: String): GroupRow = {
    val records = line.split(",")
    GroupRow(records(0), records(1))
  }
}
