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
    Category.reset()
  }

  test("Add a category") {
    assert(!Paths.get("test_category.json").toFile.exists())
    Category.setCustomStorage("test_category.json")
    val category = Category("cat1", "Category", "")
    Category.add(category)
    assert(Paths.get("test_category.json").toFile.exists())
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

  test("create a tree of categories") {
    Category.setCustomStorage("test_category.json")
    val root = Category("root", "Root", "")
    val leftBranch = Category("lb", "Left Branch", "root")
    val rightBranch = Category("rb", "Right Branch", "root")
    val leftLeftLeaf = Category("lll", "Left Left Leaf", parentId="lb")
    val leftRightLeaf = Category("lrl", "Left Right Leaf", parentId = "lb")
    val rightLeaf = Category("rl", "Right Leaf", parentId = "rb")

    Category.add(root, leftBranch, rightBranch, leftLeftLeaf, leftRightLeaf, rightLeaf)
    val topCategories = Category.topCategories
    assertEquals(topCategories, List(root))
    val branches = Category.children("root")
    assertEquals(branches, List(leftBranch, rightBranch))
    val leftLeaves = Category.children("lb")
    assertEquals(leftLeaves, List(leftLeftLeaf, leftRightLeaf))
    val rightLeaves = Category.children("rb")
    assertEquals(rightLeaves, List(rightLeaf))
    val stump = Category.children("rl")
    assert(stump.isEmpty)
    Category.reset()
  }
