package example

import java.io.File

import definitions.syntax._
object MainBigDataSet extends App {

  val schema =
    stringData("Table") ::
    stringData("Breakdown") ::
    stringData("Secondary_Breakdown") ::
    intData("Year") ::
    bigDecimalData("Value") ::
    stringData("Unit") ::
    cNil

  case class Data(table: String, breakdown: String, secondaryBreakDown: String, year: Int, value: BigDecimal, unit: String)

  var count = 0
  val stream = loadFilePrintErrors(schema, new File("src/resources/bigcsv.csv"))
    .map(row => Data.tupled(row)).foreach(_ => { count = count + 1; println(count)})



}
