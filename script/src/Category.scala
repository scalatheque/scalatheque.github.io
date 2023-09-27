import upickle.default.*
import scala.util.chaining.scalaUtilChainingOps

import Logger.*

final case class Category(id: String, name: String, parentId: String)
  extends Identifiable[String]
  derives ReadWriter:
  def isTopCategory: Boolean = parentId == ""
  def prettyStr: String =
    val sb = new StringBuilder
    sb.append("id: ").append(id).append(", name: ").append(name)
    if parentId.nonEmpty then sb.append(", parent: ").append(parentId)
    sb.result

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
  def setCustomStorage(fileName: String, resetAtStart: Boolean = true): Unit =
    customStorage = Some(CategoryStorage(fileName))
    if resetAtStart then reset()

  private lazy val storage = customStorage.getOrElse(CategoryStorage("category.json"))

  def add(cat: Category): Boolean =
    if !cat.isTopCategory && !storage.exists(cat.parentId) then
      error(s"There is no parent category with id ${cat.parentId} for the new category $cat")
      false
    else storage.get(cat.id) match
      case Some(id) =>
        error(s"A category with id $id already exists")
        false
      case None =>
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

  def remove(id: String): Boolean = storage.remove(id)

  def remove(ids: String*): Unit = storage.removeAll(ids.toList)

  def list: List[Category] = storage.list
  def reset(): Unit = storage.reset()
  def size: Int = storage.size
  def isEmpty: Boolean = storage.isEmpty
  def nonEmpty: Boolean = storage.nonEmpty
  def get(id: String): Option[Category] =
    storage.get(id).orElse {
      error(s"No category with the id $id")
      None
    }

  def topCategories: List[Category] = storage.list.filter(_.isTopCategory)
  def children(parentId: String): List[Category] = storage.list.filter(_.parentId == parentId)

  def show(cat: Category): Unit = info(cat.prettyStr)
  def showAll(cats: List[Category]): Unit = info("\n" + cats.map(_.prettyStr).mkString("\n"))
  def show(cats: Category*): Unit = showAll(cats.toList)
