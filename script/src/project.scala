//> using lib "com.lihaoyi::upickle::3.1.3"

@main
def main(): Unit =
  Logger.init()
  Logger.info("Hello, world!")
  val storage = CategoryStorage("storage.json")
  val cat1 = Category("cat1", "Category 1")
  val cat2 = Category("cat2", "Category 2", "cat1")
  storage.add(List(cat1, cat2))
  println(storage.elements)
  storage.reset()

