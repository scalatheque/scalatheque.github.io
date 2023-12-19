import upickle.default.*
import scala.util.chaining.scalaUtilChainingOps
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

  private lazy val storage = customStorage.getOrElse(EntryStorage(ScalathequePaths.EntryJson))

  export storage.{modify, addOrModify, list, size, isEmpty, nonEmpty, get, exists, ids}

  private var current: Option[Long] = None

  def getCurrent: Entry = current.flatMap(get).get

  def use(entry: Entry): Unit =
    current = Some(entry.id)

  def use(entryId: Long): Boolean = get(entryId) match
    case  Some(entry) => use(entry); true
    case None => false

  def useLast: Unit = list.lastOption.foreach(use)

  def showAll: Unit = showAll(list)

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

  def copy(oldEntryId: Long): Option[Entry] =
    get(oldEntryId).map(_.copy(id = getAndIncId())).flatMap { newEntry =>
      if storage.add(newEntry) then
        use(newEntry)
        Some(newEntry)
      else
        None
    }

  def copy(): Option[Entry] = current.flatMap(copy)

  def remove(id: Long): Boolean =
    storage.remove(id).tap { res => if res then current = None}
  inline def remove(ids: Long*): Unit = removeAll(ids.toList)
  def removeAll(ids: Iterable[Long]): Unit = ids.foreach(remove)

  private def setField(id: Long, change: Entry => Entry): Option[Entry] = get(id).map { change(_).tap(modify) }

  def setTitle(id: Long, newTitle: String): Option[Entry] = setField(id, _.copy(title = newTitle))
  def setTitle(newTitle: String): Option[Entry] = current.flatMap { setTitle(_, newTitle) }

  def setLink(id: Long, newLink: String): Option[Entry] = setField(id, _.copy(link = newLink))
  def setLink(newLink: String): Option[Entry] = current.flatMap { setLink(_, newLink) }

  def setType(id: Long, newType: MediaType): Option[Entry] = setField(id, _.copy(mediaType = newType.toString))
  def setType(newType: MediaType): Option[Entry] = current.flatMap { setType(_, newType) }
  def setType(newType: String): Option[Entry] =
    for {
      entryId   <- current
      mediaType =  MediaType(newType)
      entry     <- setType(entryId, mediaType)
    } yield entry

  def setAuthor(id: Long, newAuthor: Author): Option[Entry] = setField(id, _.copy(authorId = newAuthor.id))
  def setAuthor(newAuthor: Author): Option[Entry] = current.flatMap { setAuthor(_, newAuthor) }
  def setAuthor(authorId: String): Option[Entry] =
    for {
      entryId <- current
      author  <- Author.get(authorId)
      entry   <- setAuthor(entryId, author)
    } yield entry

  def setCategory(id: Long, newCategory: Category): Option[Entry] = setField(id, _.copy(categoryId = newCategory.id))
  def setCategory(newCategory: Category): Option[Entry] = current.flatMap { setCategory(_, newCategory) }
  def setCategory(categoryId: String): Option[Entry] =
    for {
      entryId  <- current
      category <- Category.get(categoryId)
      entry    <- setCategory(entryId, category)
    } yield entry

  def setDescription(id: Long, newDescription: String): Option[Entry] = setField(id, _.copy(description = newDescription))
  def setDescription(newDescription: String): Option[Entry] = current.flatMap { setDescription(_, newDescription) }

  def setCreated(id: Long, year: Int, month: Int, day: Int): Option[Entry] =
    setField(id, _.copy(created = toStr(year, month, day)))
  def setCreated(year: Int, month: Int, day: Int): Option[Entry] = current.flatMap { setCreated(_, year, month, day) }

  def setTags(id: Long, newTags: String*): Option[Entry] = setField(id, _.copy(tags = treatTags(newTags)))
  def setTags(newTags: String*): Option[Entry] = current.flatMap { id =>
    setField(id, _.copy(tags = treatTags(newTags).distinct.sorted))
  }

  def addTag(id: Long, tag: String): Option[Entry] =
    setField(id, entry => entry.copy(tags = treatTags(tag :: entry.tags)))
  def addTag(tag: String): Option[Entry] = current.flatMap { addTag(_, tag) }

  def addTags(tags: String*): Option[Entry] = current.flatMap { id =>
    setField(id, entry => entry.copy(tags = (treatTags(tags) ::: entry.tags).distinct.sorted))
  }

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

  def resetTags(id: Long): Option[Entry] = setField(id, _.copy(tags = Nil))
  def resetTags(): Option[Entry] = current.flatMap(resetTags)

  def byAuthor(authorId: String): List[Entry] = list.filter(_.authorId == authorId)
  def by(author: Author): List[Entry] = byAuthor(author.id)
  def byCategory(categoryId: String): List[Entry] = list.filter(_.categoryId == categoryId)
  def by(category: Category): List[Entry] = byCategory(category.id)
  def byTag(tag: String): List[Entry] = list.filter(_.tags.contains(tag))
  def by(mediaType: MediaType): List[Entry] = list.filter(_.mediaType == mediaType.toString)
  def byType(mediaType: String): List[Entry] = by(MediaType(mediaType))

  def tags: List[String] = list.flatMap(_.tags).distinct.sorted
