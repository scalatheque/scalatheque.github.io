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
  def apply(id: String, name: String): Category = new Category(id = id, name = name, parentId = "")

  def apply(name: String): Category = new Category(id = nameToId(name), name = name, parentId = "")

  private var customStorage: Option[CategoryStorage] = None

  // IMPORTANT: You have to set the custom storage before the first use
  def setCustomStorage(fileName: String, resetAtStart: Boolean = true): Unit =
    customStorage = Some(CategoryStorage(fileName))
    if resetAtStart then reset()

  private lazy val storage = customStorage.getOrElse(CategoryStorage(ScalathequePaths.CategoryJson))

  export storage.{modify, addOrModify, list, size, isEmpty, nonEmpty, get, exists, ids}

  private var current: Option[String] = None

  def getCurrent: Category = current.flatMap(get).get

  def use(cat: Category): Unit =
    current = Some(cat.id)

  def reset(): Unit =
    storage.reset()
    current = None

  def show(): Unit = current.flatMap(get).foreach(show)

  def add(cat: Category): Boolean =
    if !cat.isTopCategory && !exists(cat.parentId) then
      error(s"There is no parent category with id ${cat.parentId} for the new category $cat")
      false
    else
      storage.add(cat).tap { res => if res then use(cat) }

  def add(name: String): Category = Category(name).tap(add)
  def add(names: String*): List[Category] = names.toList.map(Category(_)).tap { _.foreach(add) }
  def add(cats: Category*): Unit = cats.foreach(add)

  def remove(id: String): Boolean =
    storage.remove(id).tap { res => if res then current = None }
  inline def remove(ids: String*): Unit = removeAll(ids.toList)
  def removeAll(ids: Iterable[String]): Unit = ids.foreach(remove)

  private def changeField(id: String, change: Category => Category): Option[Category] =
    get(id).map {
      change(_).tap(modify)
    }

  def changeName(id: String, newName: String): Option[Category] = changeField(id, _.copy(name = newName))
  def changeName(newName: String): Option[Category] = current.flatMap{ changeName(_, newName) }

  def changeParent(id: String, newParentId: String): Option[Category] = changeField(id, _.copy(parentId = newParentId))
  def changeParent(newParentId: String): Option[Category] = current.flatMap { changeParent(_, newParentId) }

  def topCategories: List[Category] = list.filter(_.isTopCategory)
  def children(parentId: String): List[Category] =
    if parentId.isEmpty then Nil else list.filter(_.parentId == parentId)
