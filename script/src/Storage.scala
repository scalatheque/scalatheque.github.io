
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.nio.file.StandardOpenOption.{CREATE, TRUNCATE_EXISTING, WRITE}
import scala.util.{Failure, Success, Try}
import Logger.*
import upickle.default.{write, read, ReadWriter}

class Storage[Id : Ordering, Entry <: Identifiable[Id] : ReadWriter](jsonFileStr: String):
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

  private def withFilePath[T](f: Path => T): Either[String, T] = synchronized {
    storageFilePath match
      case Some(filePath) =>
        Try {
          f(filePath)
        } match
          case Success(res) => Right(res)
          case Failure(ex)  => error(ex); Left(ex.getMessage)
      case None =>
        error(s"Unable to resolve the data file path for $storageFilePath")
        Left(s"Unable to resolve the data file path for $storageFilePath")
  }

  private def dump(lines: List[Entry]): Either[String, Unit] = withFilePath { path =>
    val jsonStr = write(lines.sortByIds)
    Files.writeString(path, jsonStr, CREATE, WRITE, TRUNCATE_EXISTING)
  }

  private def readIn(): Either[String, List[Entry]] = withFilePath { path =>
    if Files.exists(path) && Files.size(path) > 0 then
      read[List[Entry]](path.toFile)
    else
      List.empty[Entry]
  }

  def reset(): Unit = withFilePath { Files.deleteIfExists }

  def list: List[Entry] = readIn().getOrElse(List.empty[Entry])

  inline def isEmpty: Boolean = list.isEmpty
  inline def nonEmpty: Boolean = list.nonEmpty
  inline def size: Int = list.size
  inline def ids: Set[Id] = list.ids
  inline def get(id: Id): Option[Entry] = list.find(_.id == id)

  def add(newEntry: Entry): Boolean =
    val oldEntries = list
    if oldEntries.exists(_.id == newEntry.id) then
      error(s"You try to add an entry with an id that is already in the storage: $newEntry")
      false
    else
      dump(newEntry :: oldEntries)
      true

  inline def add(newEntries: Entry*): Unit = addAll(newEntries.toList)

  def addAll(newEntries: Iterable[Entry]): Unit =
    if newEntries.nonEmpty then
      val distinctNewEntries = newEntries.toList.distinctBy(_.id)
      if distinctNewEntries.size < newEntries.size then
        error(s"Some of the new entries have duplicated ids and were ignored: ${newEntries.toSet -- distinctNewEntries.toSet}")
      val oldEntries = list
      val oldEntriesIds = oldEntries.ids
      val (news, refusedEntries) = distinctNewEntries.partition(el => !oldEntriesIds.contains(el.id))
      if refusedEntries.nonEmpty then
        error(s"Some of the entries were refused: $refusedEntries")
      if news.nonEmpty then dump(news ::: oldEntries)

  def modify(modifiedEntry: Entry): Boolean =
    val entries = list
    if !entries.exists(_.id == modifiedEntry.id) then
      error(s"You try to modify an entry but its id is not in the storage: $modifiedEntry")
      false
    else
      dump(modifiedEntry :: entries.filterNot(_.id == modifiedEntry.id))
      true

  inline def modify(entries: Entry*): Unit = modifyAll(entries.toList)

  def modifyAll(entries: Iterable[Entry]): Unit =
    if entries.nonEmpty then
      val oldEntries = list
      val (validEntries, notStoredEntries) = entries.partition(e => ids.contains(e.id))
      if notStoredEntries.nonEmpty then
        error(s"Some of the entries you try to modify are not in the storage: $notStoredEntries")
      if validEntries.nonEmpty then  
        val newIds = validEntries.ids
        val remainingEntries = oldEntries.filterNot(e => newIds.contains(e.id))
        dump(validEntries.toList ::: remainingEntries)

  def addOrModify(entry: Entry): Unit =
    dump(entry :: list.filterNot(_.id == entry.id))

  inline def addOrModify(entries: Entry*): Unit = addOrModifyAll(entries.toList)

  def addOrModifyAll(entries: Iterable[Entry]): Unit =
    if entries.nonEmpty then
      val ids = entries.ids
      val oldEntries = list.filterNot(el => ids.contains(el.id))
      dump(entries.toList ::: oldEntries)

  def remove(id: Id): Boolean =
    val entries = list
    if !entries.exists(_.id == id) then
      warn(s"You try to remove an entry that is not in the storage: $id")
      false
    else
      val entriesToRemain = entries.filterNot(_.id == id)
      if entriesToRemain.nonEmpty then
        dump(entriesToRemain)
      else
        reset()
      true

  inline def remove(ids: Id*): Unit = removeAll(ids.toList)

  def removeAll(ids: Iterable[Id]): Unit =
    if ids.nonEmpty then
      val idSet = ids.toSet
      val entries = list
      val notInStorage = idSet.filterNot(id => list.exists(_.id == id))
      if notInStorage.nonEmpty then
        error(s"Some of the ids are of entries that are not in the storage: $ids")
      val entriesToRemain = entries.filterNot(entry => idSet.contains(entry.id))
      if entriesToRemain.size < entries.size then 
        if entriesToRemain.nonEmpty then 
          dump(entriesToRemain)
        else
          reset()  
