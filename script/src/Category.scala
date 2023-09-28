import upickle.default.*
import scala.util.chaining.scalaUtilChainingOps

import Logger.*

final case class Category(id: String, name: String, parentId: String)
  extends Identifiable[String] with PrettyStr
  derives ReadWriter:
  def isTopCategory: Boolean = parentId == ""
  override def prettyStr: String =
    val sb = new StringBuilder
    sb.append("id: ").append(id).append(", name: ").append(name)
    if parentId.nonEmpty then sb.append(", parent: ").append(parentId)
    sb.result

final class CategoryStorage(jsonFileStr: String)
  extends Storage[String, Category](jsonFileStr)

object Category extends PrettyStrCompanion[Category]:
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
  def setCustomStorage(fileName: String, resetAtStart: Boolean = true): Unit =
    customStorage = Some(CategoryStorage(fileName))
    if resetAtStart then reset()

  private lazy val storage = customStorage.getOrElse(CategoryStorage("category.json"))

  def add(cat: Category): Boolean =
    if !cat.isTopCategory && !storage.exists(cat.parentId) then
      error(s"There is no parent category with id ${cat.parentId} for the new category $cat")
      false
    else
      storage.add(cat)

  def add(name: String): Category = Category(name).tap(add)

  def add(names: String*): List[Category] =
    names.toList.map(Category(_)).tap {
      _.foreach(add)
    }

  def add(cats: Category*): Unit = cats.foreach(add)

  def changeName(id: String, newName: String): Option[Category] =
    storage.get(id).map { cat =>
      cat.copy(name = newName).tap(storage.modify)
    }

  export storage.{remove, modify, addOrModify, list, reset, size, isEmpty, nonEmpty, get}

  def topCategories: List[Category] = storage.list.filter(_.isTopCategory)
  def children(parentId: String): List[Category] = storage.list.filter(_.parentId == parentId)
