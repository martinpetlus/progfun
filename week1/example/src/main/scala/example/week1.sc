def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f)(a + 1, b)

def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * product(f)(a + 1, b)

def factorial(n: Int): Int =
  product(x => if (x == 0) 1 else x)(0, n)

def reduce(r: (Int, Int) => Int, d: Int)(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) d else r(f(a), reduce(r, d)(f)(a + 1, b))

sum(x => x)(2, 4)
product(x => x)(2, 4)

reduce((x, y) => x + y, 0)(x => x)(2, 4)
reduce((x, y) => x * y, 1)(x => x)(2, 4)

factorial(0)
factorial(5)
