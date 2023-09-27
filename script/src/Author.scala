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
                       ) extends Identifiable[String] derives ReadWriter

final class AuthorStorage(jsonFileStr: String) extends Storage[String, Author](jsonFileStr)

object Author:
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
  def modify(author: Author): Boolean = storage.modify(author)
  def modify(authors: Author*): Unit = storage.modifyAll(authors.toList)
  def remove(id: String): Boolean = storage.remove(id)
  def remove(ids: String*): Unit = storage.removeAll(ids.toList)
  def list: List[Author] = storage.list
  def reset(): Unit = storage.reset()
  def size: Int = storage.size
  def isEmpty: Boolean = storage.isEmpty
  def nonEmpty: Boolean = storage.nonEmpty
  def get(id: String): Option[Author] =
    storage.get(id).orElse {
      error(s"No author with the id $id")
      None
    }

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