package definitions

import definitions.CsvInterpreter.{HeaderValues, RowValues}

/**
 * This is code which helps us deal with Scala's Tuple.  It is massive boilerplate which can be removed
 * by using the scala libaries Shapeless and Cats.
 */
object Boilerplate {

  sealed trait Concatenation[A] extends DataDescription[A]
  case object NilConcat extends Concatenation[()] {
    def ::[Z](zData: DataDescription[Z]): Single[(Z)] = Single(zData)
  }
  case class Single[A](data: DataDescription[A]) extends Concatenation[(A)] {
    def ::[Z](zData: DataDescription[Z]): ConcatTwo[Z,A] = ConcatTwo(zData, data)
  }
  
  case class ConcatTwo[A,B](d1: DataDescription[A], d2: DataDescription[B]) extends Concatenation[(A,B)] {
    def ::[Z](zData: DataDescription[Z]) = ConcatThree(zData, d1, d2)
  }
  case class ConcatThree[A,B,C](d1: DataDescription[A], d2: DataDescription[B], d3: DataDescription[C]) extends Concatenation[(A,B,C)] {
    def ::[Z](zData: DataDescription[Z]) = ConcatFour(zData, d1, d2, d3)
  }
  case class ConcatFour[A,B,C,D](d1: DataDescription[A], d2: DataDescription[B], d3: DataDescription[C], d4: DataDescription[D]) extends Concatenation[(A,B,C,D)] {
    def ::[Z](zData: DataDescription[Z]) = ConcatFive(zData, d1, d2, d3, d4)
  }
  case class ConcatFive[A,B,C,D,E](d1: DataDescription[A], d2: DataDescription[B], d3: DataDescription[C], d4: DataDescription[D], d5: DataDescription[E]) extends Concatenation[(A,B,C,D,E)] {
    def ::[Z](zData: DataDescription[Z]) = ConcatSix(zData, d1, d2, d3, d4, d5)
  }
  case class ConcatSix[A,B,C,D,E,F](d1: DataDescription[A], d2: DataDescription[B], d3: DataDescription[C], d4: DataDescription[D], d5: DataDescription[E], d6: DataDescription[F]) extends Concatenation[(A,B,C,D,E,F)] {
    def ::[Z](zData: DataDescription[Z]) = ConcatSeven(zData, d1, d2, d3, d4, d5, d6)
  }
  case class ConcatSeven[A,B,C,D,E,F,G](d1: DataDescription[A], d2: DataDescription[B], d3: DataDescription[C], d4: DataDescription[D], d5: DataDescription[E], d6: DataDescription[F], d7: DataDescription[G]) extends Concatenation[(A,B,C,D,E,F,G)] {
    def ::[Z](zData: DataDescription[Z]) = ConcatEight(zData, d1, d2, d3, d4, d5, d6, d6)
  }
  case class ConcatEight[A,B,C,D,E,F,G,H](d1: DataDescription[A], d2: DataDescription[B], d3: DataDescription[C], d4: DataDescription[D], d5: DataDescription[E], d6: DataDescription[F], d7: DataDescription[G], d8: DataDescription[H]) extends Concatenation[(A,B,C,D,E,F,G,H)] {
    def ::[Z](zData: DataDescription[Z]) = ConcatNine(zData, d1, d2, d3, d4, d5, d6, d7, d8)
  }
  case class ConcatNine[A,B,C,D,E,F,G,H,I](d1: DataDescription[A], d2: DataDescription[B], d3: DataDescription[C], d4: DataDescription[D], d5: DataDescription[E], d6: DataDescription[F], d7: DataDescription[G], d8: DataDescription[H], d9: DataDescription[I]) extends Concatenation[(A,B,C,D,E,F,G,H,I)] {
    def ::[Z](zData: DataDescription[Z]) = ConcatTen(zData, d1, d2, d3, d4, d5, d6, d7, d8, d9)
  }
  case class ConcatTen[A,B,C,D,E,F,G,H,I,J](d1: DataDescription[A], d2: DataDescription[B], d3: DataDescription[C], d4: DataDescription[D], d5: DataDescription[E], d6: DataDescription[F], g7: DataDescription[G], g8: DataDescription[H], g9: DataDescription[I], g10: DataDescription[J]) extends Concatenation[(A,B,C,D,E,F,G,H,I,J)]

  //We could go all the way up to ConcatTwentyTwo

  def ap[A](f1: HeaderValues => RowValues => Either[Errors, A]) = ApplicativeBuilder1(f1)

  case class ApplicativeBuilder1[A](f1: HeaderValues => RowValues => Either[Errors, A]) {
    def |@|[Z](newF: HeaderValues => RowValues => Either[Errors, Z]) = ApplicativeBuilder2(f1,newF)

    def apply[RR](f: A => Either[Errors, RR]): HeaderValues => RowValues => Either[Errors, RR] =
      headerValues =>
        rowValues =>
          f1(headerValues)(rowValues).flatMap(f)
  }

  case class ApplicativeBuilder2[A,B](
    f1: HeaderValues => RowValues => Either[Errors, A],
    f2: HeaderValues => RowValues => Either[Errors, B]) {
    def |@|[Z](newF: HeaderValues => RowValues => Either[Errors, Z]) = ApplicativeBuilder3(f1,f2, newF)

    def apply[RR](f: (A,B) => Either[Errors, RR]): HeaderValues => RowValues => Either[Errors, RR] =
          combineData2(f1,f2,f)
  }

  case class ApplicativeBuilder3[A,B,C](
                                       f1: HeaderValues => RowValues => Either[Errors, A],
                                       f2: HeaderValues => RowValues => Either[Errors, B],
                                       f3: HeaderValues => RowValues => Either[Errors, C]) {
    def |@|[Z](newF: HeaderValues => RowValues => Either[Errors, Z]) = ApplicativeBuilder4(f1,f2, f3, newF)

    def apply[RR](f: (A,B,C) => Either[Errors, RR]): HeaderValues => RowValues => Either[Errors, RR] = {
      val cd = combineData2[A,B,(A,B)](f1,f2,tupleIdentity[A,B])
      combineData2[(A,B),C,RR](cd,f3, (ab,c) => f(ab._1,ab._2,c))
    }

  }

  case class ApplicativeBuilder4[A,B,C,D](
    f1: HeaderValues => RowValues => Either[Errors, A],
                                         f2: HeaderValues => RowValues => Either[Errors, B],
                                         f3: HeaderValues => RowValues => Either[Errors, C],
                                         f4: HeaderValues => RowValues => Either[Errors, D]) {
    def |@|[Z](newF: HeaderValues => RowValues => Either[Errors, Z]) = ApplicativeBuilder5(f1,f2, f3,f4, newF)
    def apply[RR](f: (A,B,C,D) => Either[Errors, RR]): HeaderValues => RowValues => Either[Errors, RR] = {
      val cd12 = combineData2[A,B,(A,B)](f1,f2,tupleIdentity[A,B])
      val cd23 = combineData2(f3,f4,tupleIdentity[C,D])
      combineData2[(A,B),(C,D),RR](cd12, cd23, (ab,cd) => f(ab._1, ab._2, cd._1, cd._2))
    }
  }

  case class ApplicativeBuilder5[A,B,C,D,E](
                                           f1: HeaderValues => RowValues => Either[Errors, A],
                                           f2: HeaderValues => RowValues => Either[Errors, B],
                                           f3: HeaderValues => RowValues => Either[Errors, C],
                                           f4: HeaderValues => RowValues => Either[Errors, D],
                                           f5: HeaderValues => RowValues => Either[Errors, E]) {
    def |@|[Z](newF: HeaderValues => RowValues => Either[Errors, Z]) = ApplicativeBuilder6(f1,f2, f3, f4, f5, newF)


    def apply[RR](f: (A,B,C,D,E) => Either[Errors, RR]): HeaderValues => RowValues => Either[Errors, RR] = {
      val cd12 = combineData2[A,B,(A,B)](f1,f2,tupleIdentity[A,B])
      val cd23 = combineData2(f3,f4,tupleIdentity[C,D])
      val cd1234 = combineData2[(A,B),(C,D),(A,B,C,D)](cd12, cd23, (ab,cd) => Right((ab._1, ab._2, cd._1, cd._2)))
      combineData2[(A,B,C,D),E,RR](cd1234, f5, (t,e) => f(t._1, t._2, t._3, t._4, e))
    }
  }

  case class ApplicativeBuilder6[A,B,C,D,E,F](
                                             f1: HeaderValues => RowValues => Either[Errors, A],
                                             f2: HeaderValues => RowValues => Either[Errors, B],
                                             f3: HeaderValues => RowValues => Either[Errors, C],
                                             f4: HeaderValues => RowValues => Either[Errors, D],
                                             f5: HeaderValues => RowValues => Either[Errors, E],
                                             f6: HeaderValues => RowValues => Either[Errors, F]) {
    def |@|[Z](newF: HeaderValues => RowValues => Either[Errors, Z]) = ApplicativeBuilder7(f1,f2, f3, f4, f5, f6, newF)

    def apply[RR](f: (A,B,C,D,E,F) => Either[Errors, RR]): HeaderValues => RowValues => Either[Errors, RR] = {
      val cd12 = combineData2[A,B,(A,B)](f1,f2,tupleIdentity[A,B])
      val cd23 = combineData2(f3,f4,tupleIdentity[C,D])
      val cd45 = combineData2(f5,f6,tupleIdentity[E,F])
      val cd1234 = combineData2[(A,B),(C,D),(A,B,C,D)](cd12, cd23, (ab,cd) => Right((ab._1, ab._2, cd._1, cd._2)))
      combineData2[(A,B,C,D),(E,F),RR](cd1234, cd45, (t,t2) => f(t._1, t._2, t._3, t._4, t2._1, t2._2))
    }

  }

  case class ApplicativeBuilder7[A,B,C,D,E,F,G](
                                               f1: HeaderValues => RowValues => Either[Errors, A],
                                               f2: HeaderValues => RowValues => Either[Errors, B],
                                               f3: HeaderValues => RowValues => Either[Errors, C],
                                               f4: HeaderValues => RowValues => Either[Errors, D],
                                               f5: HeaderValues => RowValues => Either[Errors, E],
                                               f6: HeaderValues => RowValues => Either[Errors, F],
                                               f7: HeaderValues => RowValues => Either[Errors, G]) {
    def |@|[Z](newF: HeaderValues => RowValues => Either[Errors, Z]) = ApplicativeBuilder8(f1,f2, f3, f4, f5, f6, f7, newF)

    def apply[RR](f: (A,B,C,D,E,F,G) => Either[Errors, RR]): HeaderValues => RowValues => Either[Errors, RR] = {
      val cd12 = combineData2[A,B,(A,B)](f1,f2,tupleIdentity[A,B])
      val cd34 = combineData2(f3,f4,tupleIdentity[C,D])
      val cd56 = combineData2(f5,f6,tupleIdentity[E,F])
      val cd1234 = combineData2[(A,B),(C,D),(A,B,C,D)](cd12, cd34, (ab,cd) => Right((ab._1, ab._2, cd._1, cd._2)))
      val cd123456 = combineData2[(A,B,C,D),(E,F),(A,B,C,D,E,F)](cd1234, cd56, (abcd,ef) => Right((abcd._1, abcd._2, abcd._3, abcd._4, ef._1, ef._2)))
      combineData2[(A,B,C,D,E,F),G,RR](cd123456, f7, (t,t2) => f(t._1, t._2, t._3, t._4, t._5, t._6, t2))
    }

  }

  case class ApplicativeBuilder8[A,B,C,D,E,F,G,H](
                                                 f1: HeaderValues => RowValues => Either[Errors, A],
                                                 f2: HeaderValues => RowValues => Either[Errors, B],
                                                 f3: HeaderValues => RowValues => Either[Errors, C],
                                                 f4: HeaderValues => RowValues => Either[Errors, D],
                                                 f5: HeaderValues => RowValues => Either[Errors, E],
                                                 f6: HeaderValues => RowValues => Either[Errors, F],
                                                 f7: HeaderValues => RowValues => Either[Errors, G],
                                                 f8: HeaderValues => RowValues => Either[Errors, H]) {
    def |@|[Z](newF: HeaderValues => RowValues => Either[Errors, Z]) = ApplicativeBuilder9(f1,f2, f3, f4, f5, f6, f7, f8, newF)

    def apply[RR](f: (A,B,C,D,E,F,G,H) => Either[Errors, RR]): HeaderValues => RowValues => Either[Errors, RR] = {
      val cd12 = combineData2[A,B,(A,B)](f1,f2,tupleIdentity[A,B])
      val cd34 = combineData2(f3,f4,tupleIdentity[C,D])
      val cd56 = combineData2(f5,f6,tupleIdentity[E,F])
      val cd1234 = combineData2[(A,B),(C,D),(A,B,C,D)](cd12, cd34, (ab,cd) => Right((ab._1, ab._2, cd._1, cd._2)))
      val cd123456 = combineData2[(A,B,C,D),(E,F),(A,B,C,D,E,F)](cd1234, cd56, (abcd,ef) => Right((abcd._1, abcd._2, abcd._3, abcd._4, ef._1, ef._2)))
      val cd78 = combineData2[G,H,(G,H)](f7, f8, tupleIdentity[G,H])
      combineData2[(A,B,C,D,E,F),(G,H),RR](cd123456, cd78, (t,t2) => f(t._1, t._2, t._3, t._4, t._5, t._6, t2._1, t2._2))
    }
  }

  case class ApplicativeBuilder9[A,B,C,D,E,F,G,H,I](
                                                   f1: HeaderValues => RowValues => Either[Errors, A],
                                                   f2: HeaderValues => RowValues => Either[Errors, B],
                                                   f3: HeaderValues => RowValues => Either[Errors, C],
                                                   f4: HeaderValues => RowValues => Either[Errors, D],
                                                   f5: HeaderValues => RowValues => Either[Errors, E],
                                                   f6: HeaderValues => RowValues => Either[Errors, F],
                                                   f7: HeaderValues => RowValues => Either[Errors, G],
                                                   f8: HeaderValues => RowValues => Either[Errors, H],
                                                   f9: HeaderValues => RowValues => Either[Errors, I]) {
    def |@|[Z](newF: HeaderValues => RowValues => Either[Errors, Z]) = ApplicativeBuilder10(f1,f2, f3, f4, f5, f6, f7, f8, f9, newF)

    def apply[RR](f: (A,B,C,D,E,F,G,H,I) => Either[Errors, RR]): HeaderValues => RowValues => Either[Errors, RR] = {
      val cd12 = combineData2[A,B,(A,B)](f1,f2,tupleIdentity[A,B])
      val cd34 = combineData2(f3,f4,tupleIdentity[C,D])
      val cd56 = combineData2(f5,f6,tupleIdentity[E,F])
      val cd1234 = combineData2[(A,B),(C,D),(A,B,C,D)](cd12, cd34, (ab,cd) => Right((ab._1, ab._2, cd._1, cd._2)))
      val cd123456 = combineData2[(A,B,C,D),(E,F),(A,B,C,D,E,F)](cd1234, cd56, (abcd,ef) => Right((abcd._1, abcd._2, abcd._3, abcd._4, ef._1, ef._2)))
      val cd78 = combineData2[G,H,(G,H)](f7, f8, tupleIdentity[G,H])
      val cd789 = combineData2[(G,H), I, (G,H,I)](cd78, f9, (gh,i) => Right( (gh._1, gh._2, i)))
      combineData2[(A,B,C,D,E,F),(G,H,I),RR](cd123456, cd789, (t,t2) => f(t._1, t._2, t._3, t._4, t._5, t._6, t2._1, t2._2, t2._3))
    }
  }

  case class ApplicativeBuilder10[A,B,C,D,E,F,G,H,I,J](
                                                     f1: HeaderValues => RowValues => Either[Errors, A],
                                                     f2: HeaderValues => RowValues => Either[Errors, B],
                                                     f3: HeaderValues => RowValues => Either[Errors, C],
                                                     f4: HeaderValues => RowValues => Either[Errors, D],
                                                     f5: HeaderValues => RowValues => Either[Errors, E],
                                                     f6: HeaderValues => RowValues => Either[Errors, F],
                                                     f7: HeaderValues => RowValues => Either[Errors, G],
                                                     f8: HeaderValues => RowValues => Either[Errors, H],
                                                     f9: HeaderValues => RowValues => Either[Errors, I],
                                                     f10: HeaderValues => RowValues => Either[Errors, J]) {

    def apply[RR](f: (A,B,C,D,E,F,G,H,I,J) => Either[Errors, RR]): HeaderValues => RowValues => Either[Errors, RR] = {
      val cd12 = combineData2[A,B,(A,B)](f1,f2,tupleIdentity[A,B])
      val cd34 = combineData2(f3,f4,tupleIdentity[C,D])
      val cd56 = combineData2(f5,f6,tupleIdentity[E,F])
      val cd1234 = combineData2[(A,B),(C,D),(A,B,C,D)](cd12, cd34, (ab,cd) => Right((ab._1, ab._2, cd._1, cd._2)))
      val cd123456 = combineData2[(A,B,C,D),(E,F),(A,B,C,D,E,F)](cd1234, cd56, (abcd,ef) => Right((abcd._1, abcd._2, abcd._3, abcd._4, ef._1, ef._2)))
      val cd78 = combineData2[G,H,(G,H)](f7, f8, tupleIdentity[G,H])
      val cd90 = combineData2[I,J,(I,J)](f9,f10, tupleIdentity[I,J])
      val cd789 = combineData2[(G,H), (I,J), (G,H,I,J)](cd78, cd90, (gh,i) => Right( (gh._1, gh._2, i._1, i._2)))
      combineData2[(A,B,C,D,E,F),(G,H,I,J),RR](cd123456, cd789, (t,t2) => f(t._1, t._2, t._3, t._4, t._5, t._6, t2._1, t2._2, t2._3, t2._4))
    }
  }



  def tupleIdentity[A,B](a: A, b: B): Either[Errors,(A,B)] = Right( (a,b) )

  def combineData2[A,B,Z](
                         f1: HeaderValues => RowValues => Either[Errors, A],
                         f2: HeaderValues => RowValues => Either[Errors, B],
                         f: (A,B) => Either[Errors,Z]
                       ): HeaderValues => RowValues => Either[Errors, Z] = {
    headerValues =>
      val valuesF1 = f1(headerValues)
      val valuesF2 = f2(headerValues)
      rowValues => {
        val r1 = valuesF1(rowValues)
        val r2 = valuesF2(rowValues)
        combineResults(r1,r2, f)
      }
  }

  def combineResults[A,B,Z](r1: Either[Errors,A], r2: Either[Errors,B], f: (A,B) => Either[Errors,Z]): Either[Errors, Z] = {
    val result = (r1, r2) match {
      case (Left(e1), Left(e2)) => Left(e1 ::: e2)
      case (Left(e1), _) => Left(e1)
      case (_,Left(e2)) => Left(e2)
      case (Right(s1), Right(s2)) => Right( (s1,s2) )
    }
    result.flatMap(f.tupled(_))
  }


}
