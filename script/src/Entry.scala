import upickle.default.*
import scala.util.chaining.scalaUtilChainingOps
import java.time.Instant
import Logger.*

final case class Entry(id:          Int,
                       title:       String,
                       link:        String,
                       mediaType:   String = "",
                       authorId:    String = "",
                       categoryId:  String = "",
                       tags:        List[String] = Nil,
                       description: String = "",
                       created:     String = "",
                       added:       String = Instant.now().toString
                      ) extends Identifiable[Int] with PrettyStr derives ReadWriter:
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

final class EntryStorage(jsonFileStr: String) extends Storage[Int, Entry](jsonFileStr)

object Entry extends PrettyStrCompanion[Entry]:
  private var customStorage: Option[EntryStorage] = None

  // IMPORTANT: You have to set the custom storage before the first use
  def setCustomStorage(fileName: String, resetAtStart: Boolean = true): Unit =
    customStorage = Some(EntryStorage(fileName))
    if resetAtStart then reset()

  private lazy val storage = customStorage.getOrElse(EntryStorage("entry.json"))

  export storage.{add, remove, modify, addOrModify, list, reset, size, isEmpty, nonEmpty, get}
