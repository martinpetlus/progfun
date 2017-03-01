package patmat

object Main extends App {
  import Huffman._
  val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
  val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  println(encode(t1)("ab".toList))
  println(decode(t1, List(0, 1)))
  println(decodedSecret)
  println(convert(t1))
  println(convert(t2))
  println(quickEncode(t1)("ab".toList))
  println(quickEncode(t2)("abd".toList))
}
