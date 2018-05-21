import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import scala.util.Try
import scala.io.{Codec, Source}

final case class LogRecord(host: String,
                           userName: Option[String],
                           timestamp: LocalDateTime,
                           request: String,
                           replyCode: String,
                           bytesInReply: Option[Int])

object LogRecord {
  val recordRegex = """^(\S+)\s(\S+)\s(.*?)\s\[(.+)\]\s\"(.+)\"\s(\d+)\s(\d+)?(.*)$""".r 
  val dateTimeFormatter = DateTimeFormatter.ofPattern("dd/MMM/yyyy:HH:mm:ss xxxx") // 01/Jun/1995:00:25:16 -0600
  
  def parse(string: String): Option[LogRecord] = string match {
    case recordRegex(host, x, un, tss, req,rc,br,y) =>
      val unoption:Option[String] = un match {
        case "-" => None 
        case other => Option(other)
      }
      Try(LocalDateTime.parse(tss, dateTimeFormatter)).toOption.map(
        LogRecord(host, unoption, _, req, rc,Try(br.toInt).toOption ))
    case s =>
      println(s)
      None
    }
}

class Lab1(val records: Vector[LogRecord]) {

  def task1(replyBytes: Int): Set[String] = records.withFilter(_.bytesInReply contains replyBytes).map(_.request).toSet

  def task2(request: String): Set[Int] = records.withFilter(_.request == request).flatMap(_.bytesInReply).filter(_>0).toSet

  def task3(request: String): Set[String] = records.withFilter(_.request == request).flatMap(_.userName).toSet

  def task4(username: String, date: LocalDate): Set[String] = 
  records.withFilter(x=>((x.userName contains username)&&(x.timestamp.toLocalDate==date))).map(_.request).toSet

  def task5(username: String): Set[LocalDate] = 
  records.withFilter(_.userName contains username).map(_.timestamp.toLocalDate).toSet 

  def task6(username: String, date: LocalDate): Boolean = 
  records.exists(x=>(x.userName contains username)&&(x.timestamp.toLocalDate==date))

  def task7(startDate: LocalDate, endDate: LocalDate): Vector[String] = {
        def f(x:LocalDate) = (x.isEqual(startDate)||x.isAfter(startDate))&&(x.isEqual(endDate)||x.isBefore(endDate))
    records.
    withFilter(x=>f(x.timestamp.toLocalDate) ).
    map(_.request).
    toSeq.
    groupBy(x=>x).
    mapValues(_.size).
    toSeq.
    sortWith(_._2 > _._2).
    take(5).
    map(_._1).
    toVector
    }

  def task8: Option[Int] = 
  Try(
    records.map(x=>(x.timestamp.toLocalDate,x.host,x.bytesInReply))
    .groupBy(x=>(x._1,x._2))
    .mapValues(_.flatMap(_._3).sum)
    .values.max 
  ).toOption

  def task9(n: Int): Vector[String] = 
    records.collect{case x if x.userName.nonEmpty => (x.userName,x.host)}.
    groupBy(_._1). 
    mapValues(_.size).
    filter(_._2>=n).keys.flatten.toVector


  def task10(date: LocalDate): Vector[String] = 
  records.filter(_.timestamp.toLocalDate==date).groupBy(_.host).mapValues(_.size).toSeq.sortWith(_._2 > _._2).take(5).map(_._1).toVector

  def isIP(host: String):Boolean = {
    val ipr = """^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$""".r
    host match {
        case ipr(a,b,c,d) => (a.toInt<=256)&&(c.toInt<=256)&&(b.toInt<=256)&&(d.toInt<=256)
        case _ => false
    }
  }

  def task11(startDate: LocalDate, endDate: LocalDate): Set[String] = {
      def f(x:LocalDate) = (x.isEqual(startDate)||x.isAfter(startDate))&&(x.isEqual(endDate)||x.isBefore(endDate))
    val allreqs = records.map(_.request).distinct
    records.
    filter(x=>isIP(x.host)&&f(x.timestamp.toLocalDate)).
    groupBy(_.host). // host -> vec of logrecs
    filter({
        case (h,ls) => allreqs.foldLeft(true)(
            (flag,req)=>flag&&(ls.map(_.request) contains req)
        )
    }).map(_._1).toSet
  }

  def task12(request: String): Vector[String] = 
  records.
  filter(_.request == request).
  map(_.host).
  groupBy(x=>x). //host -> vector of host
  mapValues(_.size).
  toSeq.
  sortWith(_._2 > _._2).
  take(5).
  map(_._1).
  toVector

  def task13(n: Int, startDate: LocalDate, endDate: LocalDate): Set[String] = {
      def f(x:LocalDate) = (x.isEqual(startDate)||x.isAfter(startDate))&&(x.isEqual(endDate)||x.isBefore(endDate))
    records.
    filter(x=>isIP(x.host)&&f(x.timestamp.toLocalDate)).
    groupBy(_.host).
    filter({
        case (h,ls)=> ls.groupBy(_.request).mapValues(_.size).exists(_._2>=n)
    }).take(5).
    map(_._1).
    toSet
  }

  def task14(startDate: LocalDate, endDate: LocalDate): Option[String] = {
    def f(x:LocalDate) = (x.isEqual(startDate)||x.isAfter(startDate))&&(x.isEqual(endDate)||x.isBefore(endDate))
    Try(
        records.
        filter(x=>f(x.timestamp.toLocalDate) && x.replyCode=="503").
        groupBy(_.request).
        mapValues(_.size).
        maxBy(_._2)._1
    ).toOption
  }

  def task15(startDate: LocalDate, endDate: LocalDate): Option[Double] = {
    def f(x:LocalDate) = (x.isEqual(startDate)||x.isAfter(startDate))&&(x.isEqual(endDate)||x.isBefore(endDate))
    val N = records.filter(x=>f(x.timestamp.toLocalDate)).size
    val m = records.filter(x=>f(x.timestamp.toLocalDate) && x.replyCode=="404").size
    val r = m/(N.toDouble)
    if ((r.isNaN)||(r.isInfinity)) None else Some(r)
  }


}
