import upickle.default.*

final case class Category(id: String, name: String, parentId: String) 
  extends Identifiable[String] 
  derives ReadWriter

object Category:
  def apply(id: String, name: String): Category =
    new Category(id = id, name = name, parentId = "")

  def apply(name: String): Category =
    new Category(
      id = name.toLowerCase.replace(' ','_'),
      name = name,
      parentId = ""
    )

final class CategoryStorage(jsonFileStr: String = "category.json") 
  extends Storage[String, Category](jsonFileStr)
