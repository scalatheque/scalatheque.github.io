import upickle.default.*
import scala.util.chaining.scalaUtilChainingOps
import java.time.Instant
import Logger.*

final case class Entry(id:          Long,
                       title:       String,
                       link:        String,
                       mediaType:   String,
                       authorId:    String,
                       categoryId:  String,
                       tags:        List[String],
                       description: String,
                       created:     String,
                       added:       String
                      ) extends Identifiable[Long] with PrettyStr derives ReadWriter:
  override def prettyStr: String =
    val sb = new StringBuilder
    sb.append("id: ").append(id).append(", title: ").append(title).append(", link: ").append(link)
    if mediaType.nonEmpty then sb.append(", media: ").append(mediaType)
    if authorId.nonEmpty then sb.append(", author: ").append(authorId)
    if categoryId.nonEmpty then sb.append(", category: ").append(categoryId)
    if tags.nonEmpty then sb.append(", tags: ").append(tags.mkString("|"))
    if description.nonEmpty then sb.append(", description: ").append(description)
    if created.nonEmpty then sb.append(", created: ").append(created)
    if added.nonEmpty then sb.append(", added: ").append(added)
    sb.result

final class EntryStorage(jsonFileStr: String) extends Storage[Long, Entry](jsonFileStr)

object Entry extends PrettyStrCompanion[Entry]:
  private var customStorage: Option[EntryStorage] = None

  // IMPORTANT: You have to set the custom storage before the first use
  def setCustomStorage(fileName: String, resetAtStart: Boolean = true): Unit =
    customStorage = Some(EntryStorage(fileName))
    if resetAtStart then reset()

  private lazy val storage = customStorage.getOrElse(EntryStorage("entry.json"))

  export storage.{remove, removeAll, modify, addOrModify, list, size, isEmpty, nonEmpty, get, exists, ids}

  private var current: Option[Long] = None

  def use(entry: Entry): Unit =
    current = Some(entry.id)

  def reset(): Unit =
    storage.reset()
    current = None

  def show(): Unit = current.flatMap(get).foreach(show)

  private var _firstFreeId: Long = -1

  private def firstFreeId(): Long =
    if _firstFreeId == -1 && nonEmpty then _firstFreeId = list.maxBy(_.id).id + 1L
    _firstFreeId

  private def getAndIncId(): Long = firstFreeId().tap { id => _firstFreeId = id + 1L }

  def add(title: String, link: String, mediaType: MediaType = MediaType.Unknown, authorId: String = ""): Entry =
    val entry = new Entry(id = getAndIncId(), title = title, link = link, mediaType = mediaType.toString,
                          authorId = authorId, categoryId = "", tags = Nil, description = "", created = "",
                          added = Instant.now().toString)
    if storage.add(entry) then use(entry)
    entry

  private def changeField(id: Long, change: Entry => Entry): Option[Entry] = get(id).map { change(_).tap(modify) }

  def changeTitle(id: Long, newTitle: String): Option[Entry] = changeField(id, _.copy(title = newTitle))
  def changeTitle(newTitle: String): Option[Entry] = current.flatMap { changeTitle(_, newTitle) }

  def changeLink(id: Long, newLink: String): Option[Entry] = changeField(id, _.copy(link = newLink))
  def changeLink(newLink: String): Option[Entry] = current.flatMap { changeLink(_, newLink) }

  def changeType(id: Long, newType: MediaType): Option[Entry] = changeField(id, _.copy(mediaType = newType.toString))
  def changeType(newType: MediaType): Option[Entry] = current.flatMap { changeType(_, newType) }

  def changeAuthor(id: Long, newAuthor: Author): Option[Entry] = changeField(id, _.copy(authorId = newAuthor.id))
  def changeAuthor(newAuthor: Author): Option[Entry] = current.flatMap { changeAuthor(_, newAuthor) }

  def changeCategory(id: Long, newCategory: Category): Option[Entry] = changeField(id, _.copy(categoryId = newCategory.id))
  def changeCategory(newCategory: Category): Option[Entry] = current.flatMap { changeCategory(_, newCategory) }

  def changeDescription(id: Long, newDescription: String): Option[Entry] = changeField(id, _.copy(description = newDescription))
  def changeDescription(newDescription: String): Option[Entry] = current.flatMap { changeDescription(_, newDescription) }

  def changeCreated(id: Long, newCreated: Instant): Option[Entry] = changeField(id, _.copy(created = newCreated.toString))
  def changeCreated(newCreated: Instant): Option[Entry] = current.flatMap { changeCreated(_, newCreated) }
