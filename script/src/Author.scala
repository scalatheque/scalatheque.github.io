import upickle.default.*
import scala.util.chaining.scalaUtilChainingOps

import Logger.*

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

  private lazy val storage = customStorage.getOrElse(AuthorStorage("author.json"))

  def add(author: Author): Boolean = storage.get(author.id) match
    case Some(id) =>
      error(s"An author with id $id already exists")
      false
    case None =>
      storage.add(author)

  def add(name: String): Author = Author(name).tap(add)

  def add(names: String*): List[Author] =
    names.toList.map(Author(_)).tap {
      _.foreach(add)
    }
    
  def add(authors: Author*): Unit = authors.foreach(add)

  export storage.{remove, modify, addOrModify, list, reset, size, isEmpty, nonEmpty, get}

  def changeName(id: String, newName: String): Option[Author] =
    get(id).map { author =>
      author.copy(name = newName).tap(storage.modify)
    }

  def changeEmail(id: String, newEmail: String): Option[Author] =
    get(id).map { author =>
      author.copy(email = newEmail).tap(storage.modify)
    }

  def changeTwitter(id: String, newTwitter: String): Option[Author] =
    get(id).map { author =>
      author.copy(twitter = newTwitter).tap(storage.modify)
    }

  def changeWebsite(id: String, newWebsite: String): Option[Author] =
    get(id).map { author =>
      author.copy(website = newWebsite).tap(storage.modify)
    }

  def changeYoutube(id: String, newYoutube: String): Option[Author] =
    get(id).map { author =>
      author.copy(youtube = newYoutube).tap(storage.modify)
    }

  def changeAvatar(id: String, newAvatar: String): Option[Author] =
    get(id).map { author =>
      author.copy(avatar = newAvatar).tap(storage.modify)
    }
