private val forbiddenChars = Set(' ', ':', '-', '+', '(', ')', '[', ']', ',', '.')
private val shortcuts = List(
  "category theory" -> "ct",
  "category" -> "cat",
  "functional programming" -> "fp",
  "functional" -> "func",
  "function" -> "fun",
  "library" -> "lib",
  "repository" -> "repo",
  "identity" -> "id",
  "identification" -> "id"
)

def nameToId(name: String): String =
  val lowercased = name.toLowerCase
  val noBadChars = forbiddenChars.foldLeft(lowercased) { (n, c) => n.replace(c, '_') }
  val shortened = shortcuts.foldLeft(noBadChars) { case (n, (oldStr, newStr)) => n.replaceAll(oldStr, newStr) }
  shortened

import java.time.Instant
import java.time.temporal.ChronoField
import java.util.Calendar

def dateToStr(date: Instant): String =
  val year = date.get(ChronoField.YEAR)
  val month = date.get(ChronoField.MONTH_OF_YEAR)
  val day = date.get(ChronoField.DAY_OF_MONTH)
  s"$year-$month-$day"

def now(): String =
  val cal = Calendar.getInstance
  toStr(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH))

def toStr(year: Int, month: Int, day: Int): String =
  val sb = new StringBuilder
  sb.append(year).append('-')
  if month < 10 then sb.append('0')
  sb.append(month).append('-')
  if day < 10 then sb.append('0')
  sb.append(day)
  sb.result

def toCal(str: String): Option[Calendar] =
  import scala.util.Try
  Try {
    val arr = str.split("-")
    val cal = Calendar.getInstance()
    cal.set(Calendar.YEAR, arr(0).toInt)
    cal.set(Calendar.MONTH, arr(1).toInt)
    cal.set(Calendar.DAY_OF_MONTH, arr(2).toInt)
    cal
  }.toOption

def toStr(cal: Calendar): String = toStr(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH))
