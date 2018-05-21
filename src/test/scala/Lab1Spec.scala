import java.nio.charset.StandardCharsets
import java.time.{LocalDate, LocalDateTime}

import org.scalatest.{FlatSpec, FunSpec, Matchers}

import scala.io.{Codec, Source}

class Lab1Spec extends FlatSpec with Matchers {

  def sourceIterator: Iterator[String] =
    Source.fromResource("usask_access_log")(Codec.ISO8859).getLines() 

  "Log Record" should "parse formatted string correctly" in {
    assert(
      LogRecord
        .parse(
          "202.32.92.47 - - [01/Jun/1995:00:00:59 -0600] \"GET /~scottp/publish.html\" 200 271")
        .nonEmpty)
    assert(sourceIterator.flatMap(LogRecord.parse).size == 2408623)
  }

  it should "not parse malformed strings" in {
    assert(LogRecord.parse("").isEmpty)
    assert(LogRecord.parse("malformed input").isEmpty)
    assert(LogRecord.parse("sdflhgsdfg").isEmpty)
    assert(LogRecord.parse("maz3.maz.net - - [11/Oa67220.dial.tip.net - - [12/Oct/1995:01:39:12 -0600] \"POST /cgi-bin/phone.pl HTTP/1.0\" 200 309").isEmpty)
    assert(LogRecord.parse("129.186.123.55 - - [12/Oct/1995ag5881.usask.ca - - [12/Oct/1995:16:07:36 -0600] \"GET /images/letter_32.gif HTTP/1.0\" 200 149").isEmpty)
  }

  val records = new Lab1(sourceIterator.flatMap(LogRecord.parse).toVector)

  "task1" should "work correctly" in {

    assert(records.task1(7000) == Set())
    assert(
      records.task1(10000) == Set(
        "GET /registrar/95_96_Calendar/Coll_of_AR/AR_promotion_Graduation.html HTTP/1.0"
      )
    )

  }

  "task2" should "work correctly" in {

    assert(records.task2("GET /~macpherc/images/lilbadge.gif") == Set(3274))
    assert(records.task2("GET /images/logo.gif HTTP/1.0") == Set(2273))

  }

  "task3" should "work correctly" in {

    assert(records.task3("GET /~macpherc/images/lilbadge.gif") == Set())
    assert(records.task3("GET /images/logo.gif HTTP/1.0") == Set())

  }

  "task4" should "work correctly" in {
    assert(records.task4("earl", LocalDate.of(1995, 6, 1)) == Set())
    assert(
      records.task4("John Thomas", LocalDate.of(1995, 12, 7)) == Set(
        "GET /dcs/courses/cai/html/introduction_lesson/index.html HTTP/1.0",
        "GET /dcs/courses/cai/html/index.html HTTP/1.0"))
  }

  "task5" should "work correctly" in {

    val result = records.task5("earl")
    val result2 = records.task5("fogel")

    assert(result == Set(LocalDate.of(1995, 8, 22)))
    assert(
      result2 == Set(LocalDate.of(1995, 8, 22),
                     LocalDate.of(1995, 9, 28),
                     LocalDate.of(1995, 10, 24),
                     LocalDate.of(1995, 11, 20),
                     LocalDate.of(1995, 12, 15)))

  }

  "task6" should "work correctly" in {

    val result = records.task6("earl", LocalDate.of(1995, 8, 1))
    val result2 = records.task6("John Thomas", LocalDate.of(1995, 12, 7))
    val result3 = records.task6("fogel", LocalDate.of(1995, 11, 14))

    assert(!result)
    assert(result2)
    assert(!result3)

  }

  "task7" should "work correctly" in {

    val dt1: LocalDate = LocalDate.of(1995, 1, 1)
    val dt2: LocalDate = dt1.plusDays(2)
    val dt3 = dt1.plusDays(1)

    val result = records.task7(dt1, dt2)
    val result2 = records.task7(dt1, dt3)
    val result3 = records.task7(dt3, dt1)

    assert(result == Vector())
    assert(result2 == Vector())
    assert(result3 == Vector())

  }

  "task8" should "work correctly" in {

    val result = records.task8
    assert(result.contains(58206298))

  }

  "task9" should "work correctly" in {

    assert(
      records.task9(2).toSet == Set("guest",
                                 "hbund569@csun.edu",
                                 "fogel",
                                 "lowey",
                                 "earl",
                                 "hannah",
                                 "zennon",
                                 "John Thomas",
                                 "mazzei@skyfox.usask.ca",
                                 "hbund569",
                                 "wallace"))
    assert(
      records.task9(3).toSet == Set("guest",
                                 "fogel",
                                 "lowey",
                                 "John Thomas",
                                 "hbund569",
                                 "wallace"))
    assert(records.task9(4).toSet == Set("lowey", "fogel", "wallace", "hbund569"))

  }

  "task10" should "work correctly" in {

    val dt1: LocalDate = LocalDate.of(1995, 1, 1)
    val dt2: LocalDate = LocalDate.of(1995, 1, 4)

    val result = records.task10(dt1).sorted
    val result2 = records.task10(dt2).sorted
    val result3 = records.task10(LocalDate.MIN).sorted

    assert(result == Vector())
    assert(result2 == Vector())
    assert(result3 == Vector())

  }

  "task11" should "work correctly" in {
    val result =
      records.task11(LocalDate.of(1995, 8, 11), LocalDate.of(1995, 8, 12))
    assert(result == Set())
  }

  "task12" should "work correctly" in {

    val result = records.task12("GET / HTTP/1.0")
    val result2 = records.task12("GET /~macpherc/images/lilbadge.gif")
    val result3 = records.task12("?-lalala-?").sorted

    assert(
      result == Vector("duke.usask.ca",
                       "sask.usask.ca",
                       "broadway.sfn.saskatoon.sk.ca",
                       "moondog.usask.ca",
                       "herald.usask.ca"))
    assert(
      result2 == Vector("repc173.roe.ac.uk",
                        "sesame.hensa.ac.uk",
                        "geol39.usask.ca",
                        "marsh.spider.co.uk",
                        "koriel.sun.com"))
    assert(result3 == Vector())

  }

  "task13" should "work correctly" in {

    val result =
      records.task13(2, LocalDate.of(1995, 1, 3), LocalDate.of(1995, 2, 7))
    val result2 =
      records.task13(2, LocalDate.of(1995, 1, 3), LocalDate.of(1995, 3, 1))
    val result3 = records.task13(500, LocalDate.MIN, LocalDate.MAX)

    assert(result == Set())
    assert(result2 == Set())
    assert(result3 == Set("128.95.226.85", "192.139.11.254"))

  }

  "task14" should "work correctly" in {

    val result = records.task14(LocalDate.MIN, LocalDate.MAX)
    val result2 =
      records.task14(LocalDate.of(1995, 1, 19), LocalDate.of(1995, 2, 14))

    assert(result.isEmpty)
    assert(result2.isEmpty)

  }

  "task15" should "work correctly" in {

    val result = records.task15(LocalDate.MIN, LocalDate.MAX)
    val result2 =
      records.task15(LocalDate.of(1995, 1, 19), LocalDate.of(1995, 2, 8))

    assert(result.exists(n => (n - 0.0087037).abs < 0.00001))
    assert(result2.isEmpty)

  }
}
