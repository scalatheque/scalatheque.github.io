import Logger.info

trait PrettyStr:
  def prettyStr: String

trait PrettyStrCompanion[T <: PrettyStr]:
  def show(t: T): Unit = info(t.prettyStr)
  def showAll(ts: List[T]): Unit = info("\n" + ts.map(_.prettyStr).mkString("\n"))
  def show(ts: T*): Unit = showAll(ts.toList)

