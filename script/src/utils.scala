private val forbiddenChars = Set(' ', ':', '-', '+', '(', ')', '[', ']', ',', '.')
private val shortcuts = List(
  "category theory" -> "ct",
  "category" -> "cat",
  "functional programming" -> "fp",
  "functional" -> "func",
  "function" -> "fun",
  "library" -> "lib",
  "repository" -> "repo",
  "identity" -> "id",
  "identification" -> "id"
)

def nameToId(name: String): String =
  val lowercased = name.toLowerCase
  val noBadChars = forbiddenChars.foldLeft(lowercased) { (n, c) => n.replace(c, '_') }
  val shortened = shortcuts.foldLeft(noBadChars) { case (n, (oldStr, newStr)) => n.replaceAll(oldStr, newStr) }
  shortened
