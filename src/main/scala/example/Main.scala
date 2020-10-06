package example

import java.io.File
import java.time.LocalDate

import definitions.Errors
import definitions.syntax._

object Main extends App {

  //Year, Month, Day Schema
  val ymdSchema = intData("Year", 4) :: intData("Month") :: intData("Day") :: cNil

  // Data Transformation function
  val toDate: ( (Int,Int,Int) ) => Either[Errors,LocalDate] = (ymd) => wrapThrown("Data", LocalDate.of(ymd._1, ymd._2, ymd._3))

  //Finally use the combine method
  val threeFieldDataSchema = combine(ymdSchema, toDate)

  val schema =
    intData("Order Number") ::
      threeFieldDataSchema ::
      stringData("Product Number", "[A-Z0-9]+") ::
      stringData("Product Name", "[A-Z]+") ::
      bigDecimalData("Count", "#,##0.0#") ::
      cNil


  var count = 0
  val stream: Stream[(Int, LocalDate, String, String, BigDecimal)] = loadFilePrintErrors(schema, new File("src/resources/test.csv"))
  stream.foreach(_ => count = count + 1)
  println(s"Loaded ${count} successful records")

}
