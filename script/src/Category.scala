import upickle.default.*
import scala.util.chaining.scalaUtilChainingOps

final case class Category(id: String, name: String, parentId: String)
  extends Identifiable[String]
  derives ReadWriter

final class CategoryStorage(jsonFileStr: String)
  extends Storage[String, Category](jsonFileStr)

object Category:
  def apply(id: String, name: String): Category =
    new Category(id = id, name = name, parentId = "")

  def apply(name: String): Category =
    new Category(
      id = nameToId(name),
      name = name,
      parentId = ""
    )

  private var customStorage: Option[CategoryStorage] = None

  // IMPORTANT: You have to set the custom storage before the first use
  def setCustomStorage(fileName: String): Unit =
    customStorage = Some(CategoryStorage(fileName))

  private lazy val storage = customStorage.getOrElse(CategoryStorage("category.json"))

  def add(name: String): Category = Category(name).tap { storage.add }

  def add(names: String*): List[Category] =
    names.toList.map(Category(_)).tap { storage.addAll }

  def add(cat: Category): Boolean = storage.add(cat)

  def add(cats: Category*): Unit = storage.addAll(cats.toList)

  def changeName(id: String, newName: String): Option[Category] =
    storage.get(id).tap {
      _.foreach { cat => storage.modify(cat.copy(name = newName)) }
    }

  def remove(id: String): Boolean = storage.remove(id)

  def remove(ids: String*): Unit = storage.removeAll(ids.toList)

  def list: List[Category] = storage.list
  def reset(): Unit = storage.reset()
  def size: Int = storage.size
  def isEmpty: Boolean = storage.isEmpty
  def nonEmpty: Boolean = storage.nonEmpty
  def get(id: String): Option[Category] = storage.get(id)
