# DSL Configuarable CSV Loader

Thank you for taking the time to review this project.

The main goal of this library is to be able to describe a CSV format and to load the data from a file 
while maintaining the types defined in the schema, such as String, Int or BigDecimal.

## Quick start
 * Clone Into Workspace
 * Run sbt at the root of this project
 * `runMain example.Main` to Run the example test or `runMain example.MainBigDataSet` if you
 really want to see some action.

## Schema DSL

The first step is to define a schema.  First import the syntax:

```import definitions.syntax._```

The current DSL functions available can be found in [src/main/scala/definitions/package.scala].

```scala
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
```

When defining a new schema, always start with the cNil value at the end. by itself, cNil is
just an empty definition.  We will be prepending definitions to cNil, similar to how one
would prepend an element to a Linked List.

```scala
val newSchema = cNil
```

Then specify each data type in front of cNil using the `::` operator.
```scala
val newSchema = stringData("Name") :: intData("age") :: cNil
```

To combine multiple fields into a single data type, first define the fields and then use the `combine` 
function to specify a function from the input types to the target type.

```scala
  //Year, Month, Day Schema
  val ymdSchema = intData("Year", 4) :: intData("Month") :: intData("Day") :: cNil

  // Data Transformation function
  val toDate: ( (Int,Int,Int) ) => Either[Errors,LocalDate] = (ymd) => wrapThrown("Data", LocalDate.of(ymd._1, ymd._2, ymd._3))

  //Finally use the combine method to create a data definition of a single value.
  val threeFieldDataSchema = combine(ymdSchema, toDate)

  // We can use 'threeFieldDataSchema' in are main schema definition
  val schema =
    intData("Order Number") ::
      threeFieldDataSchema ::
      stringData("Product Number", "[A-Z0-9]+") ::
      stringData("Product Name", "[A-Z]+") ::
      bigDecimalData("Count", "#,##0.0#") ::
      cNil

  // Finally we can get a stream of valid parsed input, consol logging any invalid rows
  val stream: Stream[(Int, LocalDate, String, String, BigDecimal)] = 
    loadFilePrintErrors(schema, new File("src/resources/test.csv"))  
```

Please Note that the stream maintains the Data Types of the parsed Data!!  In the example above, the
stream of data is of type `(Int, LocalDate, String, String, BigDecimal)` and can be mapped to a object
 if desired.  In my opition, this concept of maintaining type is the killer feature of this library and
 also why there are is some complicated structures.
 
 
 Please see the files `src/main/scala/exampleMain.scala` and `src/main/scala/MainBigDataSet.scala` 
 for example working code.



# Errata
During testing, this library starts to slow down loading considerable around row 2,000,000.  
I am not sure if this is because of the CSV library or something in this library.

There is currently a 10 column maximum limit.  Using 0 external dependencies, this could be expanded to 22 -- 
which is the maximum allowed tuples size in Scala.  This limit could be removed and the
code could be simplified if we added the Shapeless library and the Cats library as a dependency.  The object
Boilerplate contains a lot of code which could be deleted as well.