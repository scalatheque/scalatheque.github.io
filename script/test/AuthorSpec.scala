import munit.Location
import java.nio.file.Paths

class AuthorSpec extends munit.FunSuite:
  given Location = Location.empty

  test("create an author") {
    val author = Author("Author")
    assertEquals(author.id, "author")
    assertEquals(author.name, "Author")
  }

  test("Add an author") {
    assert(!Paths.get("test_author.json").toFile.exists())
    Author.setCustomStorage("test_author.json")
    val author = Author("Author")
    Author.add(author)
    assert(Paths.get("test_author.json").toFile.exists())
    assertEquals(Author.size, 1)
    assertEquals(Author.get(author.id), Some(author))
    Author.reset()
  }

  test("Change name") {
    Author.setCustomStorage("test_author.json")
    val author = Author("Author")
    Author.add(author)
    assertEquals(Author.get(author.id).fold("")(_.name), "Author")
    Author.setName(author.id, "Foo")
    assertEquals(Author.get(author.id).fold("")(_.name), "Foo")
    Author.reset()
  }
