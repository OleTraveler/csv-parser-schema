import java.io.File
import java.util.Date

import com.github.tototoshi.csv.CSVReader
import definitions.CsvInterpreter.{HeaderValues, RowValues}

import scala.util.Try
import scala.util.matching.Regex

package object definitions {
  import Boilerplate._

  /** there parser error */
  case class Error(fieldName: String, errorMessage: String)
  type Errors = List[Error]

  /** This describes the types we know about */
  trait DataDescription[A]
  case class IntDataDescription(key: String, digits: Option[Int], min: Option[Int], max: Option[Int]) extends DataDescription[Int]
  case class StringDataDescription(key: String, regex: Option[Regex]) extends DataDescription[String]
  case class BigDecimalDataDescription(key: String, format: Option[String]) extends DataDescription[BigDecimal]
  case class Apply[T,B](concatenation: Concatenation[T], f: T => Either[Errors,B]) extends DataDescription[B]


  object syntax {
    /* All definition must end with cNil */
    val cNil = NilConcat

    /* Specifies the input is an Int with a specific number of digits. */
    def intData(headerName: String, digits: Int) = IntDataDescription(headerName, Some(digits), None, None)
    /* Specifies the input is an Int without any other validation */
    def intData(headerName: String): IntDataDescription = IntDataDescription(headerName, None, None, None)
    /** Specifies the input is an String which must match the specified regex */
    def stringData(headerName: String, regex: String) = StringDataDescription(headerName, Some(regex.r))
    /** Specifies the input is a String without any additional validation */
    def stringData(headerName: String) = StringDataDescription(headerName, None)
    /** Specifies the input is a BigDecimal with the specific format */
    def bigDecimalData(headerName: String, format: String) = BigDecimalDataDescription(headerName, Some(format))
    /** Specifies the input is a BigDecimal, using the default BigDecimal String formatter */
    def bigDecimalData(headerName: String) = BigDecimalDataDescription(headerName, None)
    /** Join multiple data types into a single data type, such as Year, Month, Day into a Date. */
    def combine[T,B](concatenation: Concatenation[T], f: T => Either[Errors,B]) = Apply[T,B](concatenation, f)
    /** Convenience method to use when combining data types as to catch an exception and convert it to an error. */
    def wrapThrown[A](fieldName: String, f: => A): Either[Errors,A] = {
      Try { f }.toEither.left.map(th => List(Error(fieldName, th.getMessage)))
    }

    /** Load a CSV which contains a Header.   */
    def loadFile[A](d: DataDescription[A], file: File): Stream[Either[(String,Errors), A]] = {
      val input = CsvInterpreter.functionFromDataDescription(d)
      val reader = CSVReader.open(file)
      val stream = reader.toStream
      stream.headOption match {
        case None => Stream.empty
        case Some(header) => {
          val rowFunction = input.apply(HeaderValues(header.toVector))
          stream.tail.map(row => rowFunction(RowValues(row.toVector)).left.map(err => (row.mkString(","), err)))
        }
      }
    }

    def loadFilePrintErrors[A](d: DataDescription[A], file: File): Stream[A] = {
      loadFile(d, file).flatMap {
        case Left(err) => {
          println("==========")
          println("ERROR ON LINE:")
          println(err._1)
          println("errors:")
          err._2.foreach(e => println(s"Row: '${e.fieldName}' Error: ${e.errorMessage}"))
          println("==========")
          Stream.empty
        }
        case Right(success) => Stream(success)
      }
    }
  }




}
