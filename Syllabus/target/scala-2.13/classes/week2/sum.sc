def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > 0) 0 else f(a) + sum(f)(a + 1, b)

sum(x => x * x * x)(5, 6)


def sumBetter(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sumBetter(f)(a + 1, b)

sumBetter(x => x * x * x)(5, 7)


def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * product(f)(a + 1, b)

def fact(n: Int) = product(x => x)(1, n)

fact(5)



def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  if (a > b) zero else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

def productWithMap(f: Int => Int)(a: Int, b: Int) = mapReduce(f, (x, y) => x * y, 1)(a, b)

def factBetter(n: Int) = productWithMap(x => x)(1, n)

factBetter(5)

