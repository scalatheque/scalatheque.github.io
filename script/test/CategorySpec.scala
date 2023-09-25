import munit.Location
import java.nio.file.Paths

class CategorySpec extends munit.FunSuite:
  given Location = Location.empty

  test("create a category") {
    val category = Category("cat1", "Category", "")
    assertEquals(category.id, "cat1")
    assertEquals(category.name, "Category")
    assertEquals(category.parentId, "")
  }

  test("Create a test category storage") {
    assert(!Paths.get("test_category.json").toFile.exists())
    Category.setCustomStorage("test_category.json")
    val elements = Category.list
    assertEquals(elements.size, 0)
    assert(Paths.get("test_category.json").toFile.exists())
    Category.reset()
  }

  test("Add a category") {
    Category.setCustomStorage("test_category.json")
    val category = Category("cat1", "Category", "")
    Category.add(category)
    assertEquals(Category.size, 1)
    assertEquals(Category.get("cat1"), Some(category))
    Category.reset()
  }

  test("Change name") {
    Category.setCustomStorage("test_category.json")
    val category = Category("cat1", "Category", "")
    Category.add(category)
    assertEquals(Category.get("cat1").fold("")(_.name), "Category")
    Category.changeName("cat1", "Foo")
    assertEquals(Category.get("cat1").fold("")(_.name), "Foo")
    Category.reset()
  }

