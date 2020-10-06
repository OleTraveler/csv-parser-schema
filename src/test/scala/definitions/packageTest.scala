package definitions

import java.time.LocalDate

import definitions.CsvInterpreter.{HeaderValues, RowValues}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper

class packageTest extends AnyFunSuite {

  import definitions.syntax._




  val calendar =
    combine[(Int,Int,Int), LocalDate]( (intData("Year", 4) :: intData("Month", 2) :: intData("Day", 2) :: cNil) ,
      ymd => wrapThrown("Date", LocalDate.of(ymd._1, ymd._2, ymd._3))
    )

  val schema =
    intData("Order Number") ::
      calendar ::
      stringData("Product Number", "[A-Z0-9]+") ::
      stringData("Product Name", "[A-Z]+") ::
      bigDecimalData("Count", "#,##0.0#") ::
      cNil

  val fHeader = CsvInterpreter.functionFromDataDescription(schema)

  val header = HeaderValues(Vector("Order Number", "Year", "Month", "Day", "Product Number", "Product Name", "Count"))
  val row1 = RowValues(Vector("1234", "1999", "12", "01", "123ABC", "Widget 1", "2.2"))

  val fRow = fHeader(header)

  test("all input data is valid") {


    fRow(row1) match {
      case Left(err) => fail(s"expected success received error: ${err}")
      case Right(r) => {
        r._1 mustEqual 1234
        r._2 mustEqual LocalDate.of(1999, 12, 1)
        r._3 mustEqual "123ABC"
        r._4 mustEqual "Widget 1"
        r._5 mustEqual BigDecimal("2.2")
      }
    }
  }

  test ("all input data is invalid") {

    val row2 = RowValues(Vector("ABC", "345", "", "", "", "???", "AB.C"))
    fRow(row2) match {
      case Right(suc) => fail(s"expected failure, received: ${suc}")
      case Left(err) => {
        err.length mustEqual 7
        err(0) mustEqual Error("Order Number", "Could not convert input 'ABC' to an Int")
        err(1) mustEqual Error("Year", "Expected 4 digits, but '345' is only 3 digits long")
        err(2) mustEqual Error("Month", "Could not convert input '' to an Int")
        err(3) mustEqual Error("Day", "Could not convert input '' to an Int")
        err(4) mustEqual Error("Product Number", "String '' did not match regex: [A-Z0-9]+")
        err(5) mustEqual Error("Product Name", "String '???' did not match regex: [A-Z]+")
        err(6) mustEqual Error("Count", "Unparseable number: \"AB.C\"")
      }
    }
  }

  test("invalid data") {
    val row1 = RowValues(Vector("1234", "1999", "14", "01", "123ABC", "Widget 1", "2.2"))
    fRow(row1) match {
      case Right(suc) => fail(s"expected failure, received: ${suc}")
      case Left(error) => {
        println(error)
        error(0) mustEqual Error("Date", "Invalid value for MonthOfYear (valid values 1 - 12): 14")
      }
    }
  }

}
