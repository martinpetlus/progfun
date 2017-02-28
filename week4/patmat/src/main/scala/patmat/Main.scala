package patmat

object Main extends App {
  import Huffman._
  val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
  println(encode(t1)("ab".toList))
  println(decode(t1, List(0, 1)))
  println(decodedSecret)
}
