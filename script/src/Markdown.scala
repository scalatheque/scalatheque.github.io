object Markdown:
  final case class PrefixConfiguration(authorPrefix: Boolean = true, categoryPrefix: Boolean = true, tagsPrefix: Boolean = true)
  val AllPrefixesEnabled: PrefixConfiguration = PrefixConfiguration()

  def from(sb: StringBuilder, entry: Entry, prefixConf: PrefixConfiguration): Unit =
    sb.append(s"<details>\n<summary>(${entry.title})[${entry.link}]")
    if prefixConf.authorPrefix then
      Author.get(entry.authorId).foreach { author => sb.append(" by ").append(author.name) }
    if prefixConf.categoryPrefix then
      Category.get(entry.categoryId).foreach { cat => sb.append(" | category: ").append(cat.name) }
    if prefixConf.tagsPrefix && entry.tags.nonEmpty then
      sb.append(" | tags: ").append(entry.tags.mkString(", "))
    sb.append(s"</summary>\n${entry.description}\n</details>\n")

  def from(entry: Entry): String =
    val sb = new StringBuilder
    from(sb, entry, AllPrefixesEnabled)
    sb.result

  private def flatEntriesList(sb: StringBuilder, entries: List[Entry], id: String, summary: String, prefixConf: PrefixConfiguration): Unit =
    sb.append("<details id=\"").append(id).append("\">\n")
      .append("<summary>").append(summary).append("</summary>\n<ul>\n")
    entries.foreach { entry =>
      sb.append("  <li>")
      from(sb, entry, prefixConf)
      sb.append("  </li>\n")
    }
    sb.append("</ul>\n</details>\n")

  def from(author: Author): String =
    val entries = Entry.by(author)
    if entries.isEmpty then ""
    else
      val sb = new StringBuilder
      flatEntriesList(sb, entries, author.id, author.name, PrefixConfiguration(authorPrefix = false))
      sb.result

  def from(tag: String): String =
    val entries = Entry.byTag(tag)
    if entries.isEmpty then ""
    else
      val sb = new StringBuilder
      flatEntriesList(sb, entries, tag, tag, PrefixConfiguration(tagsPrefix = false))
      sb.result

  def from(cat: Category): String =
    def rec(sb: StringBuilder, category: Category): Unit = Category.children(category.id) match
      case Nil =>
        flatEntriesList(sb, Entry.by(category), category.id, category.name, PrefixConfiguration(categoryPrefix = false))
      case children =>
        val sb = new StringBuilder
        sb.append("<details id=\"").append(category.id).append("\">\n")
          .append("<summary>").append(category.name).append("</summary>\n")
        children.foreach(rec(sb, _))
        sb.append("</details>")
    val sb = new StringBuilder
    rec(sb, cat)
    sb.result
