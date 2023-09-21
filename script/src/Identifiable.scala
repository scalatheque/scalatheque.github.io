trait Identifiable[Id: Ordering]:
  def id: Id

object Identifiable:
  extension [V](idfs: Iterable[V])
    def ids[Id](using V <:< Identifiable[Id]): Set[Id] = idfs.map(_.id).toSet
    def toIdMap[Id](using V <:< Identifiable[Id]): Map[Id, V] = idfs.map(idf => idf.id -> idf).toMap
    def sortByIds[Id](using V <:< Identifiable[Id], Id => Comparable[Id]): Seq[V] = idfs.toSeq.sortBy(_.id)
