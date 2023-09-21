//> using lib "org.scalameta::munit::0.7.29"

import munit.Location

class CategorySpec extends munit.FunSuite:
  implicit val location: Location = Location.empty

  test("create a category") {
    val category = Category("cat1", "Category", None)
    assertEquals(category.id, "cat1")
  }

