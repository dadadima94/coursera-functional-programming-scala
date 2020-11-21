import week3.{Cons, List, Nil}

@scala.annotation.tailrec
def nth[T](n: Int, list: List[T]): T = {
  if (n == 0) list.head
  else nth(n - 1, list.tail)
}

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))


