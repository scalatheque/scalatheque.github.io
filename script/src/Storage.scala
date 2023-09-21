
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.nio.file.StandardOpenOption.{CREATE, TRUNCATE_EXISTING, WRITE}
import scala.util.{Failure, Success, Try}
import Logger.*
import upickle.default.{write, read, ReadWriter}

class Storage[Id : Ordering, Element <: Identifiable[Id] : ReadWriter](jsonFileStr: String):
  import math.Ordered.orderingToOrdered

  private lazy val storageFilePath: Option[Path] =
    Try {
      val filePath = Paths.get(jsonFileStr)
      val file = filePath.toFile
      if !file.exists then file.createNewFile()
      if !file.canRead then file.setReadable(true)
      if !file.canWrite then file.setWritable(true)
      info(s"file path: $filePath, exists: ${Files.exists(filePath)}, size: ${Files.size(filePath)}")
      filePath
    } match
      case Success(path) =>
        Some(path)
      case Failure(ex) =>
        error(ex)
        None

  private def withFilePath[T](f: Path => T): Either[String, T] = storageFilePath match
    case Some(filePath) =>
      Try { f(filePath) } match
        case Success(res) => Right(res)
        case Failure(ex)  => error(ex); Left(ex.getMessage)
    case None =>
      error(s"Unable to resolve the data file path for $storageFilePath")
      Left(s"Unable to resolve the data file path for $storageFilePath")

  private def dump(lines: List[Element]): Either[String, Unit] = withFilePath { path =>
    val jsonStr = write(lines.sortByIds)
    Files.writeString(path, jsonStr, CREATE, WRITE, TRUNCATE_EXISTING)
  }

  private def readIn(): Either[String, List[Element]] = withFilePath { path =>
    if Files.exists(path) && Files.size(path) > 0 then
      read[List[Element]](path.toFile)
    else
      List.empty[Element]
  }

  def reset(): Unit = withFilePath { Files.deleteIfExists }

  def elements: List[Element] = readIn().getOrElse(List.empty[Element])

  def ids: Set[Id] = elements.ids

  def get(id: Id): Option[Element] = elements.find(_.id == id)

  def add(element: Element): Boolean =
    val els = elements
    if els.exists(_.id == element.id) then false
    else
      dump(element :: els)
      true

  def add(newElements: Iterable[Element]): Unit =
    val els = elements
    val elsIds = els.ids
    val newEls = newElements.filterNot(el => elsIds.contains(el.id)).toList
    dump(newEls ::: els)

  def modify(element: Element): Boolean =
    val els = elements
    if !els.exists(_.id == element.id) then false
    else
      dump(element :: els.filterNot(_.id == element.id))
      true

  def addOrModify(element: Element): Unit =
    dump(element :: elements.filterNot(_.id == element.id))

  def addOrModify(newElements: Iterable[Element]): Unit =
    val newIds = newElements.ids
    val oldEls = elements.filterNot(el => newIds.contains(el.id))
    dump(newElements.toList ::: oldEls)

  def remove(id: Id): Boolean =
    val els = elements
    if !els.exists(_.id == id) then false
    else
      dump(els.filterNot(_.id == id))
      true

  def remove(ids: Iterable[Id]): Unit =
    val idSet = ids.toSet
    dump(elements.filterNot(el => idSet.contains(el.id)).toList)
