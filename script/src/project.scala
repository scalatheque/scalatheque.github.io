//> using lib "com.lihaoyi::upickle::3.1.3"
//> using lib "org.scalameta::munit::0.7.29"

@main
def main(): Unit =
  Logger.info("Hello, world!")
  val storage = CategoryStorage("storage.json")
  val cat1 = Category("cat1", "Category 1")
  val cat2 = Category("cat2", "Category 2", "cat1")
  storage.add(cat1, cat2)
  Logger.info(storage.list.mkString(", "))
  storage.reset()

/*
Category(id, name, parentId)
MediaType enum: Article, Website, Video, Book, Podcast, Course, Project, Example, Library, App, Image
Author(id, name, email, twitter, website, youtube, avatar)
Tag(id)
Entry(id, title, description, link, mediaType, author, category, tags, created, added)
 */
