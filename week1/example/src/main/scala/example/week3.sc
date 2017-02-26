abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other
  override def toString: String = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def toString: String = "{" + left + elem + right + "}"
  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem
}

val t = new NonEmpty(3, Empty, Empty)
val t1 = t.incl(4)

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

def nth[T](n: Int, list: List[T]): T =
  if (list.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) list.head
  else nth(n - 1, list.tail)

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

nth(0, list)
nth(1, list)
nth(2, list)
nth(3, list)

// Week 4
object List {
  def apply[T]() = new Nil
  def apply[T](a: T): List[T] = new Cons(a, new Nil)
  def apply[T](a: T, b: T): List[T] = new Cons(a, new Cons(b, new Nil))
}

//val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty))
//val b: Array[IntSet] = a
//b(0) = Empty
//val s: NonEmpty = a(0)
