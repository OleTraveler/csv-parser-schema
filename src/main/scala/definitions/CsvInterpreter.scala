package definitions

import definitions.Boilerplate.{ConcatThree, ConcatTwo, NilConcat, Single, _}

import scala.util.Try
import scala.util.matching.Regex

object CsvInterpreter {

  case class HeaderValues(headerValues: Vector[String]) extends AnyVal
  case class RowValues(row: Vector[String]) extends AnyVal

  /**
   * Converts a data description into a curried function.  The first input to the function should be a Vector
   * of case sensitive headers, after which we are left with a function from RowValues => Either[Errors,A]
   * which is used to parse each row.
   * @param data The data descriptor
   * @tparam A The type which the data descriptor describes
   * @return The valid A type or a List of errors.
   */
  def functionFromDataDescription[A](data: DataDescription[A]): HeaderValues => RowValues => Either[Errors, A] = {
    val result = data match {
      case IntDataDescription(key, digits, min, max) => fromIntData(key, digits, min, max)
      case StringDataDescription(key, regex) => fromStringData(key, regex)
      case BigDecimalDataDescription(key, format) => fromBigDecimalData(key, format)
      case ap: Apply[t,b] =>
        (h: HeaderValues) => (r: RowValues) => {
          val result: Either[Errors,t] = functionFromDataDescription[t](ap.concatenation)(h)(r).asInstanceOf[Either[Errors,t]]
          result.flatMap(ap.f).asInstanceOf[Either[Errors, A]]
        }
      case NilConcat => (h: HeaderValues) => (r: RowValues) => Right( () )
      case Single(data) => functionFromDataDescription(data)
      case ConcatTwo(d1, d2) =>
        (ap(functionFromDataDescription(d1)) |@| functionFromDataDescription(d2)).apply(tupleIdentity)
      case ConcatThree(d1,d2,d3) =>
        (ap(functionFromDataDescription(d1)) |@| functionFromDataDescription(d2) |@| functionFromDataDescription(d3)).apply((a, b, c) => Right( (a,b,c)))
      case ConcatFour(d1,d2,d3,d4) =>
        (ap(functionFromDataDescription(d1)) |@| functionFromDataDescription(d2) |@| functionFromDataDescription(d3) |@| functionFromDataDescription(d4)).apply((a, b, c, d) => Right( (a,b,c,d)))
      case ConcatFive(d1,d2,d3,d4,d5) =>
        (ap(functionFromDataDescription(d1)) |@| functionFromDataDescription(d2) |@| functionFromDataDescription(d3) |@| functionFromDataDescription(d4) |@| functionFromDataDescription(d5)).apply((a, b, c, d, e) => Right( (a,b,c,d,e)))
      case ConcatSix(d1,d2,d3,d4,d5,d6) =>
        (ap(functionFromDataDescription(d1)) |@| functionFromDataDescription(d2) |@| functionFromDataDescription(d3) |@| functionFromDataDescription(d4) |@| functionFromDataDescription(d5) |@| functionFromDataDescription(d6)).apply((a, b, c, d, e, f) => Right( (a,b,c,d,e,f)))
      case ConcatSeven(d1,d2,d3,d4,d5,d6,d7) =>
        (ap(functionFromDataDescription(d1)) |@| functionFromDataDescription(d2) |@| functionFromDataDescription(d3) |@| functionFromDataDescription(d4) |@| functionFromDataDescription(d5) |@| functionFromDataDescription(d6) |@| functionFromDataDescription(d7)).apply((a, b, c, d, e, f, g) => Right( (a,b,c,d,e,f,g)))
      case ConcatEight(d1,d2,d3,d4,d5,d6,d7,d8) =>
        (ap(functionFromDataDescription(d1)) |@| functionFromDataDescription(d2) |@| functionFromDataDescription(d3) |@| functionFromDataDescription(d4) |@| functionFromDataDescription(d5) |@| functionFromDataDescription(d6) |@| functionFromDataDescription(d7) |@| functionFromDataDescription(d8)).apply((a, b, c, d, e, f, g, h) => Right( (a,b,c,d,e,f,g,h)))
      case ConcatNine(d1,d2,d3,d4,d5,d6,d7,d8,d9) =>
        (ap(functionFromDataDescription(d1)) |@| functionFromDataDescription(d2) |@| functionFromDataDescription(d3) |@| functionFromDataDescription(d4) |@| functionFromDataDescription(d5) |@| functionFromDataDescription(d6) |@| functionFromDataDescription(d7) |@| functionFromDataDescription(d8) |@| functionFromDataDescription(d9)).apply((a, b, c, d, e, f, g, h, i) => Right( (a,b,c,d,e,f,g,h,i)))
      case ConcatTen(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10) =>
        (ap(functionFromDataDescription(d1)) |@| functionFromDataDescription(d2) |@| functionFromDataDescription(d3) |@| functionFromDataDescription(d4) |@| functionFromDataDescription(d5) |@| functionFromDataDescription(d6) |@| functionFromDataDescription(d7) |@| functionFromDataDescription(d8) |@| functionFromDataDescription(d9) |@| functionFromDataDescription(d10)).apply((a, b, c, d, e, f, g, h, i, j) => Right( (a,b,c,d,e,f,g,h,i, j)))



    }
    //Scala needs some help with this GADT type.
    result.asInstanceOf[HeaderValues => RowValues => Either[Errors, A]]
  }

  def indexFromHeader(key: String, hv: HeaderValues): Either[Errors, Int] = {
    val i = hv.headerValues.indexOf(key)
    if (i >= 0) Right(i)
    else Left(List(Error(key, s"Column with header ${key} was not found.")))
  }

  def extractString(key: String, index: Either[Errors, Int], rowValues: RowValues): Either[Errors, String] = for {
    i <- index
    row <- syntax.wrapThrown(key, rowValues.row.apply(i))
  } yield row

  def fromIntData(key: String, digits: Option[Int], min: Option[Int], max: Option[Int]):
    HeaderValues => RowValues => Either[Errors, Int] =
    (headers: HeaderValues) => {
      val index = indexFromHeader(key, headers)
      rowValues =>
        for {
          str <- extractString(key, index, rowValues)
          i <- Try {str.toInt }.toEither.left.map(_ => List(Error(key, s"Could not convert input '${str}' to an Int")))
          _ <- validateDigits(key, digits, str)
          _ <- validateMin(key, min, i)
          _ <- validateMax(key, min, i)
        } yield i
    }

  def validateDigits(key: String, digitsOpt: Option[Int], str: String): Either[Errors, String] =
    digitsOpt match {
      case None => Right(str)
      case Some(digits) if (str.length == digits) => Right(str)
      case Some(digits) => Left(List(Error(key, s"Expected $digits digits, but '$str' is only ${str.length} digits long")))
    }

  def validateMin(key: String, minOpt: Option[Int], i: Int): Either[Errors, Int] =
    minOpt match {
      case None => Right(i)
      case Some(max) =>
        if (i > max) Left(List(Error(key, s"Int $i was bigger than max $max")))
        else Right(i)
    }

  def validateMax(key: String, maxOption: Option[Int], i: Int): Either[Errors, Int] =
    maxOption match {
      case None => Right(i)
      case Some(min) =>
        if (i < min) Left(List(Error(key, s"Int $i was smaller than min $min")))
        else Right(i)
    }

  def fromStringData(key: String, maybeRegex: Option[Regex]): HeaderValues => RowValues => Either[Errors,String] =
    headers => {
      val index = indexFromHeader(key, headers)
      rowValues =>
        for {
          str <- extractString(key, index, rowValues)
          _ <- maybeRegex match {
            case Some(regex) =>
              if (regex.findFirstIn(str).isDefined) Right(str)
              else Left(List(Error(key, s"String '$str' did not match regex: ${regex.toString()}")))
            case None => Right(str)
          }
        } yield str
    }


  def fromBigDecimalData(key: String, format: Option[String]): HeaderValues => RowValues => Either[Errors,BigDecimal] = {
    headers => {
      val index = indexFromHeader(key, headers)
      rowValues =>
        for {
          str <- extractString(key, index, rowValues)
          bd <- strToBigDecimal(key, str, format)
        } yield bd
    }
  }

  def strToBigDecimal(key: String, str: String, format: Option[String]): Either[Errors, BigDecimal] = {
    format match {
      case Some(pattern) => {
        //copied from https://stackoverflow.com/questions/18231802/how-can-i-parse-a-string-to-bigdecimal
        import java.text.{DecimalFormat, DecimalFormatSymbols}
        val symbols = new DecimalFormatSymbols
        symbols.setGroupingSeparator(',')
        symbols.setDecimalSeparator('.')
        val decimalFormat = new DecimalFormat(pattern, symbols)
        decimalFormat.setParseBigDecimal(true)

        // parse the string
        syntax.wrapThrown(key, BigDecimal(decimalFormat.parse(str).asInstanceOf[java.math.BigDecimal]))
      }
      case None =>
        syntax.wrapThrown(key, BigDecimal(str))
    }
  }

}
