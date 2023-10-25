import java.nio.file.{Path, Paths}

object ScalathequePaths:
  val CategoryJson: String = "category.json"
  val AuthorJson: String = "author.json"
  val EntryJson: String = "entry.json"

  lazy val CategoriesMd: Path = Paths.get("../_tabs/categories.md")
  lazy val TagsMd: Path = Paths.get("../_tabs/tags.md")
  lazy val AuthorsMd: Path = Paths.get("../_tabs/authors.md")
  lazy val ImgDir: Path = Paths.get("../assets/img")

  lazy val CategoriesTemplate = Paths.get("md_templates/categories.md")
  lazy val TagsTemplate = Paths.get("md_templates/tags.md")
  lazy val AuthorsTemplate = Paths.get("md_templates/authors.md")
