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

  export storage.{modify, addOrModify, list, size, isEmpty, nonEmpty, get, exists, ids}

  private var current: Option[Long] = None
  
  def getCurrent: Entry = current.flatMap(get).get

  def use(entry: Entry): Unit =
    current = Some(entry.id)

  def reset(): Unit =
    storage.reset()
    current = None

  def show(): Unit = current.flatMap(get).foreach(show)

  private var _firstFreeId: Long = 1L

  private def firstFreeId(): Long =
    if _firstFreeId == 1L && nonEmpty then _firstFreeId = list.maxBy(_.id).id + 1L
    _firstFreeId

  private def getAndIncId(): Long = firstFreeId().tap { id => _firstFreeId = id + 1L }

  private def treatTags(tags: Iterable[String]): List[String] = tags.toList.map(_.toLowerCase.trim).sorted

  def add(title:        String,
          link:         String,
          mediaType:    MediaType = MediaType.Unknown,
          authorId:     String = "",
          categoryId:   String = "",
          tags:         List[String] = Nil,
          description:  String = "",
          created:      String = ""
         ): Entry =
    val entry = new Entry(id = getAndIncId(), title = title, link = link, mediaType = mediaType.toString,
                          authorId = authorId, categoryId = categoryId, tags = treatTags(tags),
                          description = description, created = created, added = now())
    if storage.add(entry) then use(entry)
    entry

  def remove(id: Long): Boolean =
    storage.remove(id).tap { res => if res then current = None}
  inline def remove(ids: Long*): Unit = removeAll(ids.toList)
  def removeAll(ids: Iterable[Long]): Unit = ids.foreach(remove)

  private def changeField(id: Long, change: Entry => Entry): Option[Entry] = get(id).map { change(_).tap(modify) }

  def changeTitle(id: Long, newTitle: String): Option[Entry] = changeField(id, _.copy(title = newTitle))
  def changeTitle(newTitle: String): Option[Entry] = current.flatMap { changeTitle(_, newTitle) }

  def changeLink(id: Long, newLink: String): Option[Entry] = changeField(id, _.copy(link = newLink))
  def changeLink(newLink: String): Option[Entry] = current.flatMap { changeLink(_, newLink) }

  def changeType(id: Long, newType: MediaType): Option[Entry] = changeField(id, _.copy(mediaType = newType.toString))
  def changeType(newType: MediaType): Option[Entry] = current.flatMap { changeType(_, newType) }
  def changeType(newType: String): Option[Entry] =
    for {
      entryId   <- current
      mediaType =  MediaType(newType)
      entry     <- changeType(entryId, mediaType)
    } yield entry

  def changeAuthor(id: Long, newAuthor: Author): Option[Entry] = changeField(id, _.copy(authorId = newAuthor.id))
  def changeAuthor(newAuthor: Author): Option[Entry] = current.flatMap { changeAuthor(_, newAuthor) }
  def changeAuthor(authorId: String): Option[Entry] =
    for {
      entryId <- current
      author  <- Author.get(authorId)
      entry   <- changeAuthor(entryId, author)
    } yield entry

  def changeCategory(id: Long, newCategory: Category): Option[Entry] = changeField(id, _.copy(categoryId = newCategory.id))
  def changeCategory(newCategory: Category): Option[Entry] = current.flatMap { changeCategory(_, newCategory) }
  def changeCategory(categoryId: String): Option[Entry] =
    for {
      entryId  <- current
      category <- Category.get(categoryId)
      entry    <- changeCategory(entryId, category)
    } yield entry

  def changeDescription(id: Long, newDescription: String): Option[Entry] = changeField(id, _.copy(description = newDescription))
  def changeDescription(newDescription: String): Option[Entry] = current.flatMap { changeDescription(_, newDescription) }

  def changeCreated(id: Long, year: Int, month: Int, day: Int): Option[Entry] =
    changeField(id, _.copy(created = toStr(year, month, day)))
  def changeCreated(year: Int, month: Int, day: Int): Option[Entry] = current.flatMap { changeCreated(_, year, month, day) }

  def changeTags(id: Long, newTags: String*): Option[Entry] = changeField(id, _.copy(tags = treatTags(newTags)))
  def changeTags(newTags: String*): Option[Entry] = current.flatMap { id =>
    changeField(id, _.copy(tags = treatTags(newTags)))
  }

  def addTag(id: Long, tag: String): Option[Entry] =
    changeField(id, entry => entry.copy(tags = treatTags(tag :: entry.tags)))
  def addTag(tag: String): Option[Entry] = current.flatMap { addTag(_, tag) }

  def removeTag(id: Long, tag: String): Option[Entry] = get(id) match {
    case None =>
      None
    case Some(entry) if !entry.tags.contains(tag) =>
      error(s"There is no tag $tag in the entry $entry")
      None
    case Some(entry) =>
      modify(entry.copy(tags = treatTags(entry.tags.toSet - tag)))
      Some(entry)
  }
  def removeTag(tag: String): Option[Entry] = current.flatMap(removeTag(_, tag))

  def resetTags(id: Long): Option[Entry] = changeField(id, _.copy(tags = Nil))
  def resetTags(): Option[Entry] = current.flatMap(resetTags)

  def byAuthor(authorId: String): List[Entry] = list.filter(_.authorId == authorId)
  def by(author: Author): List[Entry] = byAuthor(author.id)
  def byCategory(categoryId: String): List[Entry] = list.filter(_.categoryId == categoryId)
  def by(category: Category): List[Entry] = byCategory(category.id)
  def byTag(tag: String): List[Entry] = list.filter(_.tags.contains(tag))
  def by(mediaType: MediaType): List[Entry] = list.filter(_.mediaType == mediaType.toString)
  def byMediaType(mediaType: String): List[Entry] = by(MediaType(mediaType))
