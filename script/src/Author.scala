import upickle.default.*
import scala.util.chaining.scalaUtilChainingOps

final case class Author(id:      String,
                        name:    String,
                        email:   String,
                        twitter: String,
                        website: String,
                        youtube: String,
                        avatar:  String
                       ) extends Identifiable[String] with PrettyStr derives ReadWriter:
  override def prettyStr: String =
    val sb = new StringBuilder
    sb.append("id: ").append(id).append(", name: ").append(name)
    if email.nonEmpty then sb.append(", email: ").append(email)
    if twitter.nonEmpty then sb.append(", twitter: ").append(twitter)
    if website.nonEmpty then sb.append(", website: ").append(website)
    if youtube.nonEmpty then sb.append(", youtube: ").append(youtube)
    if avatar.nonEmpty then sb.append(", avatar: ").append(avatar)
    sb.result

final class AuthorStorage(jsonFileStr: String) extends Storage[String, Author](jsonFileStr)

object Author extends PrettyStrCompanion[Author]:
  def apply(name: String, email: String = "", twitter: String = "", website: String = "", youtube: String = "", avatar: String = ""): Author =
    val id = nameToId(name)
    new Author(id, name, email, twitter, website, youtube, avatar)

  private var customStorage: Option[AuthorStorage] = None

  // IMPORTANT: You have to set the custom storage before the first use
  def setCustomStorage(fileName: String, resetAtStart: Boolean = true): Unit =
    customStorage = Some(AuthorStorage(fileName))
    if resetAtStart then reset()

  private lazy val storage = customStorage.getOrElse(AuthorStorage(ScalathequePaths.AuthorJson))

  export storage.{modify, addOrModify, list, size, isEmpty, nonEmpty, get, exists, ids}

  def showAll: Unit = showAll(list)

  private var current: Option[String] = None

  def getCurrent: Author = current.flatMap(get).get

  def use(author: Author): Unit =
    current = Some(author.id)

  def use(authorId: String): Boolean = get(authorId) match
    case Some(author) => use(author); true
    case None => false

  def useLast: Unit = list.lastOption.foreach(use)

  def reset(): Unit =
    storage.reset()
    current = None

  def show(): Unit = current.flatMap(get).foreach(show)

  def add(author: Author): Boolean =
    storage.add(author).tap { res => if res then use(author) }
  def add(name: String): Author = Author(name).tap(add)
  def add(names: String*): List[Author] = names.toList.map(Author(_)).tap { _.foreach(add) }
  def add(authors: Author*): Unit = authors.foreach(add)

  def remove(id: String): Boolean =
    storage.remove(id).tap { res => if res then current = None }
  inline def remove(ids: String*): Unit = removeAll(ids.toList)
  def removeAll(ids: Iterable[String]): Unit = ids.foreach(remove)

  private def setField(id: String, change: Author => Author): Option[Author] = get(id).map { change(_).tap(modify) }

  def setName(id: String, newName: String): Option[Author] = setField(id, _.copy(name = newName))
  def setName(newName: String): Option[Author] = current.flatMap{ setName(_, newName) }

  def setEmail(id: String, newEmail: String): Option[Author] = setField(id, _.copy(email = newEmail))
  def setEmail(newEmail: String): Option[Author] = current.flatMap{ setEmail(_, newEmail) }

  def setTwitter(id: String, newTwitter: String): Option[Author] = setField(id, _.copy(twitter = newTwitter))
  def setTwitter(newTwitter: String): Option[Author] = current.flatMap{ setTwitter(_, newTwitter) }

  def setWebsite(id: String, newWebsite: String): Option[Author] = setField(id, _.copy(website = newWebsite))
  def setWebsite(newWebsite: String): Option[Author] = current.flatMap{ setWebsite(_, newWebsite) }

  def setYoutube(id: String, newYoutube: String): Option[Author] = setField(id, _.copy(youtube = newYoutube))
  def setYoutube(newYoutube: String): Option[Author] = current.flatMap{ setYoutube(_, newYoutube) }

  def setAvatar(id: String, newAvatar: String): Option[Author] = setField(id, _.copy(avatar = newAvatar))
  def setAvatar(newAvatar: String): Option[Author] = current.flatMap{ setAvatar(_, newAvatar) }
