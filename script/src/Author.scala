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

  private var current: Option[String] = None
  
  def getCurrent: Author = current.flatMap(get).get

  def use(author: Author): Unit =
    current = Some(author.id)

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

  private def changeField(id: String, change: Author => Author): Option[Author] = get(id).map { change(_).tap(modify) }

  def changeName(id: String, newName: String): Option[Author] = changeField(id, _.copy(name = newName))
  def changeName(newName: String): Option[Author] = current.flatMap{ changeName(_, newName) }

  def changeEmail(id: String, newEmail: String): Option[Author] = changeField(id, _.copy(email = newEmail))
  def changeEmail(newEmail: String): Option[Author] = current.flatMap{ changeEmail(_, newEmail) }

  def changeTwitter(id: String, newTwitter: String): Option[Author] = changeField(id, _.copy(twitter = newTwitter))
  def changeTwitter(newTwitter: String): Option[Author] = current.flatMap{ changeTwitter(_, newTwitter) }

  def changeWebsite(id: String, newWebsite: String): Option[Author] = changeField(id, _.copy(website = newWebsite))
  def changeWebsite(newWebsite: String): Option[Author] = current.flatMap{ changeWebsite(_, newWebsite) }

  def changeYoutube(id: String, newYoutube: String): Option[Author] = changeField(id, _.copy(youtube = newYoutube))
  def changeYoutube(newYoutube: String): Option[Author] = current.flatMap{ changeYoutube(_, newYoutube) }

  def changeAvatar(id: String, newAvatar: String): Option[Author] = changeField(id, _.copy(avatar = newAvatar))
  def changeAvatar(newAvatar: String): Option[Author] = current.flatMap{ changeAvatar(_, newAvatar) }
