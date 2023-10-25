import munit.Location
import upickle.default.*
import scala.util.chaining.scalaUtilChainingOps
import java.nio.file.Paths

class StorageSpec extends munit.FunSuite:
  given Location = Location.empty

  case class Foo(id: String, data: String = "") extends Identifiable[String] derives ReadWriter
  val fileName = "fooStorage.json"

  def exists(fileName: String): Boolean = Paths.get(fileName).toFile.exists

  test("create and reset storage") {
    assert(!exists(fileName))
    val storage = Storage[String, Foo](fileName)
    assert(storage.isEmpty)
    assert(exists(fileName))
    storage.reset()
    assert(!exists(fileName))
  }

  test("add an entry") {
    val storage = Storage[String, Foo](fileName)
    assert(storage.isEmpty)
    val foo = Foo("foo")
    storage.add(foo)
    assert(storage.nonEmpty)
    val list = storage.list
    assertEquals(list.head, foo)
    storage.reset()
  }

  test("can't add a second entry with the same id") {
    val storage = Storage[String, Foo](fileName)
    assert(storage.isEmpty)
    val foo1 = Foo("foo", "Foo")
    val res1 = storage.add(foo1)
    assert(res1)
    assert(storage.nonEmpty)
    val foo2 = Foo("foo", "Bar")
    val res2 = storage.add(foo2)
    assert(!res2)
    assertEquals(storage.size, 1)
    val list = storage.list
    assertEquals(list.head, foo1)
    storage.reset()
  }

  test("can't add a second entry with the same id - with addAll") {
    val storage = Storage[String, Foo](fileName)
    assert(storage.isEmpty)
    val foo1 = Foo("foo", "Foo")
    val foo2 = Foo("foo", "Bar")
    storage.addAll(List(foo1, foo2))
    assertEquals(storage.size, 1)
    val list = storage.list
    assertEquals(list.head, foo1)
    storage.reset()
  }

  test("add more than one entry") {
    val storage = Storage[String, Foo](fileName)
    assert(storage.isEmpty)
    val foo1 = Foo("foo1", "Foo 1")
    val foo2 = Foo("foo2", "Foo 2")
    storage.add(foo1, foo2)
    assertEquals(storage.size, 2)
    val list = storage.list
    assertEquals(list.size, 2)
    storage.reset()
  }

  test("entries in the storage are sorted by ids") {
    val storage = Storage[String, Foo](fileName)
    assert(storage.isEmpty)
    val bbb = Foo("bbb", "Bbb")
    val ccc = Foo("ccc", "Ccc")
    storage.add(bbb, ccc)
    assertEquals(storage.size, 2)
    val list = storage.list
    assertEquals(list.size, 2)
    assertEquals(list(0), bbb)
    assertEquals(list(1), ccc)
    val aaa = Foo("aaa", "Aaa")
    storage.add(aaa)

    assertEquals(storage.size, 3)
    val newList = storage.list
    assertEquals(newList(0), aaa)
    assertEquals(newList(1), bbb)
    assertEquals(newList(2), ccc)

    storage.reset()
  }

  test("remove an entry") {
    val storage = Storage[String, Foo](fileName)
    assert(storage.isEmpty)
    storage.add(Foo("foo"))
    assert(storage.nonEmpty)
    storage.remove("foo")
    assert(storage.isEmpty)
    storage.reset()
  }

  test("remove an entry when the storage has two") {
    val storage = Storage[String, Foo](fileName)
    storage.add(Foo("foo"), Foo("bar"))
    assertEquals(storage.size, 2)
    storage.remove("foo")
    assertEquals(storage.size, 1)
    assertEquals(storage.list.head.id, "bar")
    storage.reset()
  }

  test("remove more than one entry") {
    val storage = Storage[String, Foo](fileName)
    storage.add(Foo("foo"), Foo("bar"))
    assertEquals(storage.size, 2)
    storage.remove("foo", "bar")
    assert(storage.isEmpty)
    storage.reset()
  }

  test("retrieve an entry") {
    val storage = Storage[String, Foo](fileName)
    val foo = Foo("foo", "foo")
    storage.add(foo)
    val entry = storage.get(foo.id)
    assertEquals(entry, Some(foo))
    storage.reset()
  }

  test("modify an entry") {
    val storage = Storage[String, Foo](fileName)
    storage.add(Foo("foo", "foo"))
    storage.modify(Foo("foo", "bar"))
    val entry = storage.get("foo")
    assertEquals(entry, Some(Foo("foo", "bar")))
    storage.reset()
  }

  test("add and then modify an entry with addOrModify") {
    val storage = Storage[String, Foo](fileName)
    assert(storage.isEmpty)
    val foo1 = Foo("foo", "Foo")
    storage.addOrModify(foo1)
    assertEquals(storage.size, 1)
    assertEquals(storage.list.head, foo1)
    val foo2 = Foo("foo", "Bar")
    storage.addOrModify(foo2)
    assertEquals(storage.size, 1)
    assertEquals(storage.list.head, foo2)
    storage.reset()
  }
