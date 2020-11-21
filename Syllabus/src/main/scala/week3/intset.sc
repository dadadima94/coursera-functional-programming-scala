abstract class IntSet {

  def contains(x: Int): Boolean

  def include(x: Int): IntSet

  def union(other: IntSet): IntSet
}


class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  override def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  override def include(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left include x, right)
    if (x > elem) new NonEmpty(elem, left, right include x)
    else this
  }

  override def union(other: IntSet): IntSet = {
    ((left union right) union other) include elem
  }

  override def toString = "{" + left + elem + right + "}"
}


object Empty extends IntSet {

  override def contains(x: Int): Boolean = false

  override def include(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def union(other: IntSet): IntSet = other

  override def toString = "."
}


val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 include 4
val t3 = new NonEmpty(7, Empty, Empty)
val t4 = t1 union t3






