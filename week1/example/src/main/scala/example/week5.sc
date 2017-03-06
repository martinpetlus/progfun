def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

init(List(1, 2, 3))

def removeAt[T](n: Int, xs: List[T]): List[T] =
  (xs take n) ::: (xs drop n + 1)

removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)
removeAt(8, List('a', 'b', 'c', 'd')) // List(a, b, c, d)

def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => Nil
  case y :: ys => (y match {
      case List() => Nil
      case z :: zs => z :: flatten(zs)
      case _ => List(y)
    }) ::: flatten(ys)
}

flatten(List(List(1, 1), 2, List(), List(3, List(5, 8))))

def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
  case (Nil, _) => ys
  case (_, Nil) => xs
  case (x :: xs1, y :: ys1) =>
    if (x < y) x :: merge(xs1, ys)
    else y :: merge(xs, ys1)
}

def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => y * y :: squareList(ys)
}

def squareList(xs: List[Int]): List[Int] =
  xs map (x => x * x)

// def pack[T](xs: List[T]): List[List[T]] = xs match {
//   case Nil => Nil
//   case x :: xs1 =>
//     (x :: xs1.takeWhile(y => x.equals(y))) :: pack(xs1.dropWhile(y => x.equals(y)))
// }

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => x == y)
    first :: pack(rest)
}

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())((x, acc) => f(x) :: acc)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((x, acc) => acc + 1)

println(mapFun(List(1, 2, 3), (x: Int) => x * 2))
println(lengthFun(List(1, 2, 3)))
