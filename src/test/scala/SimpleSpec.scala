import java.time.{LocalDate, LocalDateTime}

import org.scalatest.{FlatSpec, Matchers}

class SimpleSpec extends FlatSpec with Matchers {
  val emptySample: Lab1 = new Lab1(Vector.empty)
  val nonEmptySample: Lab1 = new Lab1(
    Vector(
      LogRecord("1.2.3.4",
                None,
                LocalDateTime.of(2018, 1, 1, 18, 0),
                "/login",
                "503",
                None),
      LogRecord("88.99.136.11",
                Some("haskell"),
                LocalDateTime.of(2018, 1, 1, 19, 1),
                "/login",
                "404",
                Some(12)),
      LogRecord("1.2.3.4",
                Some("scala"),
                LocalDateTime.of(2018, 1, 2, 18, 1),
                "/units",
                "200",
                Some(134)),
      LogRecord("localhost",
                Some("scala"),
                LocalDateTime.of(2018, 1, 2, 18, 2),
                "/pictures/send/email",
                "404",
                None),
      LogRecord("1.2.3.4",
                Some("scala"),
                LocalDateTime.of(2018, 1, 2, 8, 0),
                "/pictures/download",
                "404",
                None),
      LogRecord("88.99.136.11",
                None,
                LocalDateTime.of(2018, 3, 1, 18, 0),
                "/units",
                "200",
                Some(134)),
      LogRecord("88.99.136.11",
                Some("haskell"),
                LocalDateTime.of(2018, 3, 1, 18, 0),
                "/pictures/generate",
                "200",
                Some(13)),
      LogRecord("88.99.136.11",
                Some("haskell"),
                LocalDateTime.of(2018, 3, 17, 18, 0),
                "/report/generate",
                "503",
                Some(1341)),
      LogRecord("1.2.3.4",
                None,
                LocalDateTime.of(2018, 2, 12, 18, 0),
                "/login",
                "200",
                None),
      LogRecord("localhost",
                Some("scala"),
                LocalDateTime.of(2018, 2, 11, 18, 0),
                "/report/generate",
                "503",
                Some(1241)),
      LogRecord("94.130.133.33",
                Some("haskell"),
                LocalDateTime.of(2018, 2, 13, 18, 0),
                "/pictures/send/email",
                "404",
                None),
      LogRecord("88.99.136.11",
                Some("scala"),
                LocalDateTime.of(2018, 2, 14, 18, 0),
                "/pictures/download",
                "404",
                None),
      LogRecord("localhost",
                None,
                LocalDateTime.of(2018, 2, 14, 18, 0),
                "/report/generate",
                "503",
                Some(2134)),
      LogRecord("1.2.3.4",
                Some("scala"),
                LocalDateTime.of(2018, 3, 14, 18, 0),
                "/login",
                "200",
                None),
      LogRecord("localhost",
                Some("haskell"),
                LocalDateTime.of(2018, 3, 8, 18, 0),
                "/pictures/download",
                "404",
                None),
      LogRecord("88.99.136.11",
                Some("haskell"),
                LocalDateTime.of(2018, 3, 2, 18, 0),
                "/report/generate",
                "503",
                Some(14)),
      LogRecord("11.22.33.44",
                None,
                LocalDateTime.of(2018, 3, 3, 18, 0),
                "/pictures/send/email",
                "404",
                None),
      LogRecord("localhost",
                Some("haskell"),
                LocalDateTime.of(2018, 3, 3, 18, 0),
                "/login",
                "200",
                None),
      LogRecord("11.22.33.44",
                Some("haskell"),
                LocalDateTime.of(2018, 1, 2, 18, 0),
                "/report/generate",
                "200",
                Some(13421)),
      LogRecord("1.2.3.4",
                Some("scala"),
                LocalDateTime.of(2018, 1, 2, 18, 0),
                "/pictures/generate",
                "200",
                Some(114)),
      LogRecord("11.22.33.44",
                None,
                LocalDateTime.of(2018, 1, 7, 18, 0),
                "/pictures/download",
                "404",
                None),
      LogRecord("88.99.136.11",
                Some("scala"),
                LocalDateTime.of(2018, 1, 7, 18, 0),
                "/pictures/send/email",
                "404",
                None),
      LogRecord("11.22.33.44",
                Some("scala"),
                LocalDateTime.of(2018, 2, 7, 18, 0),
                "/pictures/generate",
                "503",
                Some(132)),
      LogRecord("1.2.3.4",
                Some("scala"),
                LocalDateTime.of(2018, 2, 7, 18, 0),
                "/units",
                "200",
                Some(134)),
      LogRecord("11.22.33.44",
                Some("scala"),
                LocalDateTime.of(2018, 2, 7, 18, 0),
                "/pictures/send/email",
                "404",
                None),
      LogRecord("1.2.3.4",
                Some("haskell"),
                LocalDateTime.of(2018, 2, 7, 18, 0),
                "/units",
                "503",
                Some(134)),
      LogRecord("11.22.33.44",
                Some("haskell"),
                LocalDateTime.of(2018, 1, 4, 18, 0),
                "/pictures/generate",
                "200",
                Some(11134)),
      LogRecord("11.22.33.44",
                Some("haskell"),
                LocalDateTime.of(2018, 1, 4, 18, 0),
                "/units",
                "503",
                Some(134))
    ))

  "Log Record" should "parse formatted string correctly" in {
    assert(
      LogRecord
        .parse(
          "202.32.92.47 - - [01/Jun/1995:00:00:59 -0600] \"GET /~scottp/publish.html\" 200 271")
        .nonEmpty)
  }

  it should "not parse malformed strings" in {
    assert(LogRecord.parse("").isEmpty)
    assert(LogRecord.parse("malformed input").isEmpty)
    assert(LogRecord.parse("sdflhgsdfg").isEmpty)
  }

  "task1" should "work correctly" in {
    val emptyResult = emptySample.task1(134)
    val result = nonEmptySample.task1(134)
    val result2 = nonEmptySample.task1(-1)

    assert(emptyResult.isEmpty)
    assert(result == Set("/units"))
    assert(result2.isEmpty)
  }

  "task2" should "work correctly" in {
    val emptyResult = emptySample.task2("/login")
    val result = nonEmptySample.task2("/login")
    val result2 = nonEmptySample.task2("/units")

    assert(emptyResult.isEmpty)
    assert(result == Set(12))
    assert(result2 == Set(134))
  }

  "task3" should "work correctly" in {
    val emptyResult = emptySample.task3("/login")
    val result = nonEmptySample.task3("/units")
    val result2 = nonEmptySample.task3("/trails/generate")

    assert(emptyResult.isEmpty)
    assert(result == Set("haskell", "scala"))
    assert(result2.isEmpty)
  }

  "task4" should "work correctly" in {
    assert(emptySample.task4("haskell", LocalDate.of(2018, 1, 1)).isEmpty)
    assert(
      nonEmptySample.task4("haskell", LocalDate.of(2018, 1, 1)) == Set(
        "/login"))
    assert(nonEmptySample.task4("scala", LocalDate.of(2018, 1, 1)).isEmpty)
  }

  "task5" should "work correctly" in {
    val emptyResult = emptySample.task5("haskell")
    val result = nonEmptySample.task5("scala")
    val result2 = nonEmptySample.task5("haskell")

    assert(emptyResult.isEmpty)
    assert(
      result == Set(
        LocalDate.of(2018, 1, 2),
        LocalDate.of(2018, 1, 7),
        LocalDate.of(2018, 2, 7),
        LocalDate.of(2018, 2, 11),
        LocalDate.of(2018, 2, 14),
        LocalDate.of(2018, 3, 14)
      ))

    assert(
      result2 == Set(
        LocalDate.of(2018, 1, 1),
        LocalDate.of(2018, 1, 2),
        LocalDate.of(2018, 1, 4),
        LocalDate.of(2018, 2, 7),
        LocalDate.of(2018, 2, 13),
        LocalDate.of(2018, 3, 1),
        LocalDate.of(2018, 3, 2),
        LocalDate.of(2018, 3, 3),
        LocalDate.of(2018, 3, 8),
        LocalDate.of(2018, 3, 17)
      ))
  }

  "task6" should "work correctly" in {
    val emptyResult = emptySample.task6("haskell", LocalDate.of(2018, 3, 17))
    val result = nonEmptySample.task6("haskell", LocalDate.of(2018, 3, 17))
    val result2 = nonEmptySample.task6("scala", LocalDate.of(2018, 3, 14))
    val result3 = nonEmptySample.task6("scala", LocalDate.of(2012, 3, 14))

    assert(!emptyResult)
    assert(result)
    assert(result2)
    assert(!result3)
  }

  "task7" should "work correctly" in {
    val dt1: LocalDate = LocalDate.of(2018, 1, 1)
    val dt2: LocalDate = dt1.plusMonths(2)
    val dt3 = dt1.plusMonths(1)

    val emptyResult = emptySample.task7(dt1, dt2)
    val result = nonEmptySample.task7(dt1, dt2)
    val result2 = nonEmptySample.task7(dt1, dt3)
    val result3 = nonEmptySample.task7(dt3, dt1)

    assert(emptyResult.isEmpty)
    assert(result.size == 5)
    assert(
      result == Vector("/units",
                       "/pictures/send/email",
                       "/pictures/generate",
                       "/report/generate",
                       "/pictures/download"))
    assert(result2.size == 5)
    assert(
      result2 == Vector("/pictures/send/email",
                        "/units",
                        "/pictures/download",
                        "/pictures/generate",
                        "/login"))
    assert(result3.isEmpty)
  }

  "task8" should "work correctly" in {
    val emptyResult = emptySample.task8
    val result = nonEmptySample.task8

    assert(emptyResult.isEmpty)
    assert(result.nonEmpty)
    assert(result.contains(13421))
  }

  "task9" should "work correctly" in {
    val emptyResult = emptySample.task9(2)
    val result = nonEmptySample.task9(2)

    assert(emptyResult.isEmpty)
    assert(result.nonEmpty)
    assert(result.sorted == Vector("haskell", "scala"))
  }

  "task10" should "work correctly" in {
    val dt1: LocalDate = LocalDate.of(2018, 1, 1)
    val dt2: LocalDate = LocalDate.of(2018, 1, 4)

    val emptyResult = emptySample.task10(dt1)
    val result = nonEmptySample.task10(dt1).sorted
    val result2 = nonEmptySample.task10(dt2).sorted
    val result3 = nonEmptySample.task10(LocalDate.MIN).sorted

    assert(emptyResult.isEmpty)
    assert(result.size == 2)
    assert(result == Vector("1.2.3.4", "88.99.136.11"))
    assert(result2.size == 1)
    assert(result2 == Vector("11.22.33.44").sorted)
    assert(result3.isEmpty)
  }

  "task11" should "work correctly" in {
    val emptyResult = emptySample.task11(LocalDate.MIN, LocalDate.MAX)
    val result = nonEmptySample.task11(LocalDate.MIN, LocalDate.MAX)

    assert(emptyResult.isEmpty)
    assert(result == Set("88.99.136.11"))
  }

  "task12" should "work correctly" in {
    val emptyResult = emptySample.task12("/login")
    val result = nonEmptySample.task12("/login")
    val result2 = nonEmptySample.task12("/units")
    val result3 = nonEmptySample.task12("$?-lalala-?$").sorted

    assert(emptyResult.isEmpty)
    assert(result.size == 3)
    assert(result == Vector("1.2.3.4", "localhost", "88.99.136.11"))
    assert(result2.size == 3)
    assert(result2 == Vector("1.2.3.4", "88.99.136.11", "11.22.33.44"))
    assert(result3.isEmpty)
  }

  "task13" should "work correctly" in {
    val emptyResult =
      emptySample.task13(1, LocalDate.of(2011, 1, 1), LocalDate.of(2018, 9, 9))
    val result = nonEmptySample.task13(2,
                                       LocalDate.of(2018, 1, 3),
                                       LocalDate.of(2018, 2, 7))
    val result2 = nonEmptySample.task13(2,
                                        LocalDate.of(2018, 1, 3),
                                        LocalDate.of(2018, 3, 1))
    val result3 = nonEmptySample.task13(100, LocalDate.MIN, LocalDate.MAX)

    assert(emptyResult.isEmpty)
    assert(result == Set("1.2.3.4", "11.22.33.44"))
    assert(result2 == Set("1.2.3.4", "11.22.33.44"))
    assert(result3.isEmpty)
  }

  "task14" should "work correctly" in {
    val emptyResult = emptySample.task14(LocalDate.MIN, LocalDate.MAX)
    val result = nonEmptySample.task14(LocalDate.MIN, LocalDate.MAX)
    val result2 = nonEmptySample.task14(LocalDate.of(2018, 1, 19),
                                        LocalDate.of(2018, 2, 14))

    assert(emptyResult.isEmpty)
    assert(result.nonEmpty)
    assert(result.contains("/report/generate"))
    assert(result2.nonEmpty)
    assert(result2.contains("/report/generate"))
  }

  "task15" should "work correctly" in {
    val emptyResult = emptySample.task15(LocalDate.MIN, LocalDate.MAX)
    val result = nonEmptySample.task15(LocalDate.MIN, LocalDate.MAX)
    val result2 =
      nonEmptySample.task15(LocalDate.of(2018, 1, 19), LocalDate.of(2018, 2, 8))

    assert(emptyResult.isEmpty)
    assert(result.nonEmpty)
    assert(result.exists(n => (n - 10.0 / 28).abs < 0.0001))
    assert(result2.nonEmpty)
    assert(result2.exists(n => (n - 0.25).abs < 0.0001))
  }
}
