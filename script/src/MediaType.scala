import scala.util.Try
import Logger.*

enum MediaType:
  case Article
  case Website
  case Video
  case Book
  case Podcast
  case Course
  case Project
  case Example
  case Library
  case App
  case Image
  case Unknown

object MediaType:
  def apply(name: String): MediaType =
    val fixedName =
      val trimmed = name.trim
      if !trimmed.head.isUpper || trimmed.tail.exists(_.isUpper) then
        s"${trimmed.head.toUpper}${trimmed.tail.toLowerCase}"
      else
        trimmed
    Try(MediaType.valueOf(fixedName)).getOrElse {
      error(s"There is no media type with name $name")
      MediaType.Unknown
    }

