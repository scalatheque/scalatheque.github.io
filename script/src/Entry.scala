import upickle.default.*
import scala.util.chaining.scalaUtilChainingOps

import Logger.*


final case class Entry(id: Int,
                       title: String,
                       description: String,
                       link: String,
                       mediaType: String,
                       authorId: String,
                       categoryId: String,
                       tags: List[String],
                       created: String,
                       added: String
                      ) extends Identifiable[Int] derives ReadWriter

final class EntryStorage(jsonFileStr: String) extends Storage[Int, Entry](jsonFileStr)

object Entry:
  private var customStorage: Option[EntryStorage] = None

  // IMPORTANT: You have to set the custom storage before the first use
  def setCustomStorage(fileName: String, resetAtStart: Boolean = true): Unit =
    customStorage = Some(EntryStorage(fileName))
    if resetAtStart then reset()

  private lazy val storage = customStorage.getOrElse(EntryStorage("entry.json"))

  def add(entry: Entry): Boolean = storage.get(entry.id) match
    case Some(id) =>
      error(s"An entry with id $id already exists")
      false
    case None =>
      storage.add(entry)

  def add(entries: Entry*): Unit = entries.foreach(add)

  def modify(entry: Entry): Boolean = storage.modify(entry)

  def modify(entries: Entry*): Unit = storage.modifyAll(entries.toList)

  def remove(id: Int): Boolean = storage.remove(id)

  def remove(ids: Int*): Unit = storage.removeAll(ids.toList)

  def list: List[Entry] = storage.list

  def reset(): Unit = storage.reset()

  def size: Int = storage.size

  def isEmpty: Boolean = storage.isEmpty

  def nonEmpty: Boolean = storage.nonEmpty

  def get(id: Int): Option[Entry] =
    storage.get(id).orElse {
      error(s"No entry with the id $id")
      None
    }
