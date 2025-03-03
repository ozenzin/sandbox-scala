package oz.sandbox


/*
 * Calculate all combinations for game of 24:
 * Given four numbers 1..9 in any order (repetitions allowed) and using four arithmetic ops (+, -, *, and /) augmented with parentheses,
 * find all combinations resulting in number 24.
 * For instance, given 5, 3, 3, 6 following combinations match:
 * <pre>
 * 5 * 3 + 3 + 6 = 24
 * 5 * 6 - 3 - 3 = 24
 * (5 + 3)*(6 - 3)= 24
 * ...
 * </pre>
 */
object GameOf24 {

  val numbers = List(2, 3, 5, 7)//we would need to look in all n.permutations (i.e. 4!)


  val ops = List(`+`, `-`, `*`, `/`)


  def main(args: Array[String]): Unit = {

//    assert(24 == ops(0)(ops(0)(ops(2)(numbers(0), numbers(1)), numbers(2)), numbers(3)) )
//    assert(24 == ops(2)(ops(0)(numbers(0), numbers(1)), ops(1)(numbers(3), numbers(2))) )

    for (ns <- numbers.permutations) {
      for (op <- ops) {
        Solution.parenthesized1(ns)(op)
      }

      for (ops2 <- ops.combinations(2)) {
        Solution.parenthesized2(ns)(ops2(0), ops2(1))
        Solution.parenthesized2(ns)(ops2(1), ops2(0))
      }

      for (ops3 <- ops.combinations(3).flatMap(_.permutations)) {
        Solution.parenthesized3(ns)(ops3(0), ops3(1), ops3(2))
      }
    }

  }

  object Solution {

    def check(res: Int, show: String):  Unit = {
      if (res == 24) println(show)
    }

    /*
    1. (((_ _) _) _)
    2. ((_ (_ _)) _)
    3. (_ _) (_ _)
    4. (_ ((_ _) _)
    5. (_ (_ (_ _)))
    */
    def parenthesized1(n: List[Int])(fn: (Int, Int) => Int): Unit = fn match {
      case `/` | `-` =>
      check(fn(fn(fn(n(0), n(1)), n(2)), n(3)),
        s"(((${n(0)} $fn ${n(1)}) $fn ${n(2)}) $fn ${n(3)})")//#1
      check(fn(fn(n(0), fn(n(1), n(2))), n(3)),
        s"((${n(0)} $fn (${n(1)} $fn ${n(2)})) $fn ${n(3)})")//#2
      check(fn(fn(n(0), n(1)), fn(n(2), n(3))),
        s"(${n(0)} $fn ${n(1)}) $fn (${n(2)} $fn ${n(3)})")//#3
      check(fn(n(0), fn(fn(n(1), n(2)), n(3))),
        s"(${n(0)} $fn ((${n(1)} $fn ${n(2)}) $fn ${n(3)})")//#4
      check(fn(n(0), fn(n(1), fn(n(2), n(3)))),
        s"(${n(0)} $fn (${n(1)} $fn (${n(2)} $fn ${n(3)})))")//#5
      case _ =>
    }

    def parenthesized2(n: List[Int])(fn1: (Int, Int) => Int, fn2: (Int, Int) => Int): Unit = {
      check(fn1(fn1(fn2(n(0), n(1)), n(2)), n(3)),
        s"(((${n(0)} $fn2 ${n(1)}) $fn1 ${n(2)}) $fn1 ${n(3)})")//#1
      check(fn1(fn2(fn1(n(0), n(1)), n(2)), n(3)),
        s"(((${n(0)} $fn1 ${n(1)}) $fn2 ${n(2)}) $fn1 ${n(3)})")//#1
      check(fn1(fn2(fn2(n(0), n(1)), n(2)), n(3)),
        s"(((${n(0)} $fn2 ${n(1)}) $fn2 ${n(2)}) $fn1 ${n(3)})")//#1
      check(fn2(fn1(fn1(n(0), n(1)), n(2)), n(3)),
        s"(((${n(0)} $fn1 ${n(1)}) $fn1 ${n(2)}) $fn2 ${n(3)})")//#1
      check(fn2(fn1(fn2(n(0), n(1)), n(2)), n(3)),
        s"(((${n(0)} $fn2 ${n(1)}) $fn1 ${n(2)}) $fn2 ${n(3)})")//#1
      check(fn2(fn2(fn1(n(0), n(1)), n(2)), n(3)),
        s"(((${n(0)} $fn1 ${n(1)}) $fn2 ${n(2)}) $fn2 ${n(3)})")//#1

      check(fn1(fn1(n(0), fn2(n(1), n(2))), n(3)),
        s"((${n(0)} $fn1 (${n(1)} $fn2 ${n(2)})) $fn1 ${n(3)})")//#2
      check(fn1(fn2(n(0), fn1(n(1), n(2))), n(3)),
        s"((${n(0)} $fn2 (${n(1)} $fn1 ${n(2)})) $fn1 ${n(3)})")//#2
      check(fn1(fn2(n(0), fn2(n(1), n(2))), n(3)),
        s"((${n(0)} $fn2 (${n(1)} $fn2 ${n(2)})) $fn1 ${n(3)})")//#2
      check(fn2(fn1(n(0), fn1(n(1), n(2))), n(3)),
        s"((${n(0)} $fn1 (${n(1)} $fn1 ${n(2)})) $fn2 ${n(3)})")//#2
      check(fn2(fn1(n(0), fn2(n(1), n(2))), n(3)),
        s"((${n(0)} $fn1 (${n(1)} $fn2 ${n(2)})) $fn2 ${n(3)})")//#2
      check(fn2(fn2(n(0), fn1(n(1), n(2))), n(3)),
        s"((${n(0)} $fn2 (${n(1)} $fn1 ${n(2)})) $fn2 ${n(3)})")//#2

      check(fn1(fn1(n(0), n(1)), fn2(n(2), n(3))),
        s"(${n(0)} $fn1 ${n(1)}) $fn1 (${n(2)} $fn2 ${n(3)})")//#3
      check(fn1(fn2(n(0), n(1)), fn1(n(2), n(3))),
        s"(${n(0)} $fn2 ${n(1)}) $fn1 (${n(2)} $fn1 ${n(3)})")//#3
      check(fn1(fn2(n(0), n(1)), fn2(n(2), n(3))),
        s"(${n(0)} $fn2 ${n(1)}) $fn1 (${n(2)} $fn2 ${n(3)})")//#3
      check(fn2(fn1(n(0), n(1)), fn1(n(2), n(3))),
        s"(${n(0)} $fn1 ${n(1)}) $fn2 (${n(2)} $fn1 ${n(3)})")//#3
      check(fn2(fn1(n(0), n(1)), fn2(n(2), n(3))),
        s"(${n(0)} $fn1 ${n(1)}) $fn2 (${n(2)} $fn2 ${n(3)})")//#3
      check(fn2(fn2(n(0), n(1)), fn1(n(2), n(3))),
        s"(${n(0)} $fn2 ${n(1)}) $fn2 (${n(2)} $fn1 ${n(3)})")//#3

      check(fn1(n(0), fn1(fn2(n(1), n(2)), n(3))),
        s"(${n(0)} $fn1 ((${n(1)} $fn2 ${n(2)}) $fn1 ${n(3)})")//#4
      check(fn1(n(0), fn2(fn1(n(1), n(2)), n(3))),
        s"(${n(0)} $fn1 ((${n(1)} $fn1 ${n(2)}) $fn2 ${n(3)})")//#4
      check(fn1(n(0), fn2(fn2(n(1), n(2)), n(3))),
        s"(${n(0)} $fn1 ((${n(1)} $fn2 ${n(2)}) $fn2 ${n(3)})")//#4
      check(fn2(n(0), fn1(fn1(n(1), n(2)), n(3))),
        s"(${n(0)} $fn2 ((${n(1)} $fn1 ${n(2)}) $fn1 ${n(3)})")//#4
      check(fn2(n(0), fn1(fn2(n(1), n(2)), n(3))),
        s"(${n(0)} $fn2 ((${n(1)} $fn2 ${n(2)}) $fn1 ${n(3)})")//#4
      check(fn2(n(0), fn2(fn1(n(1), n(2)), n(3))),
        s"(${n(0)} $fn2 ((${n(1)} $fn1 ${n(2)}) $fn2 ${n(3)})")//#4

      check(fn1(n(0), fn1(n(1), fn2(n(2), n(3)))),
        s"(${n(0)} $fn1 (${n(1)} $fn1 (${n(2)} $fn2 ${n(3)})))")//#5
      check(fn1(n(0), fn2(n(1), fn1(n(2), n(3)))),
        s"(${n(0)} $fn1 (${n(1)} $fn2 (${n(2)} $fn1 ${n(3)})))")//#5
      check(fn1(n(0), fn2(n(1), fn2(n(2), n(3)))),
        s"(${n(0)} $fn1 (${n(1)} $fn2 (${n(2)} $fn2 ${n(3)})))")//#5
      check(fn2(n(0), fn1(n(1), fn1(n(2), n(3)))),
        s"(${n(0)} $fn2 (${n(1)} $fn1 (${n(2)} $fn1 ${n(3)})))")//#5
      check(fn2(n(0), fn1(n(1), fn2(n(2), n(3)))),
        s"(${n(0)} $fn2 (${n(1)} $fn1 (${n(2)} $fn2 ${n(3)})))")//#5
      check(fn2(n(0), fn2(n(1), fn1(n(2), n(3)))),
        s"(${n(0)} $fn2 (${n(1)} $fn2 (${n(2)} $fn1 ${n(3)})))")//#5
    }

    def parenthesized3(n: List[Int])(fn1: (Int, Int) => Int, fn2: (Int, Int) => Int, fn3: (Int, Int) => Int): Unit = {
      check(fn1(fn2(fn3(n(0), n(1)), n(2)), n(3)),
        s"(((${n(0)} $fn3 ${n(1)}) $fn2 ${n(2)}) $fn1 ${n(3)})")//#1
      check(fn1(fn3(fn2(n(0), n(1)), n(2)), n(3)),
        s"(((${n(0)} $fn2 ${n(1)}) $fn3 ${n(2)}) $fn3 ${n(3)})")//#1
      check(fn3(fn2(fn1(n(0), n(1)), n(2)), n(3)),
        s"(((${n(0)} $fn1 ${n(1)}) $fn2 ${n(2)}) $fn3 ${n(3)})")//#1
      check(fn3(fn1(fn2(n(0), n(1)), n(2)), n(3)),
        s"(((${n(0)} $fn2 ${n(1)}) $fn1 ${n(2)}) $fn3 ${n(3)})")//#1
      check(fn2(fn1(fn3(n(0), n(1)), n(2)), n(3)),
        s"(((${n(0)} $fn3 ${n(1)}) $fn1 ${n(2)}) $fn2 ${n(3)})")//#1
      check(fn2(fn3(fn1(n(0), n(1)), n(2)), n(3)),
        s"(((${n(0)} $fn1 ${n(1)}) $fn3 ${n(2)}) $fn2 ${n(3)})")//#1

      check(fn1(fn2(n(0), fn3(n(1), n(2))), n(3)),
        s"((${n(0)} $fn2 (${n(1)} $fn3 ${n(2)})) $fn1 ${n(3)})")//#2
      check(fn1(fn3(n(0), fn2(n(1), n(2))), n(3)),
        s"((${n(0)} $fn3 (${n(1)} $fn2 ${n(2)})) $fn1 ${n(3)})")//#2
      check(fn3(fn2(n(0), fn1(n(1), n(2))), n(3)),
        s"((${n(0)} $fn2 (${n(1)} $fn1 ${n(2)})) $fn3 ${n(3)})")//#2
      check(fn3(fn1(n(0), fn2(n(1), n(2))), n(3)),
        s"((${n(0)} $fn1 (${n(1)} $fn2 ${n(2)})) $fn3 ${n(3)})")//#2
      check(fn2(fn1(n(0), fn3(n(1), n(2))), n(3)),
        s"((${n(0)} $fn1 (${n(1)} $fn3 ${n(2)})) $fn2 ${n(3)})")//#2
      check(fn2(fn3(n(0), fn1(n(1), n(2))), n(3)),
        s"((${n(0)} $fn3 (${n(1)} $fn1 ${n(2)})) $fn2 ${n(3)})")//#2

      check(fn1(fn2(n(0), n(1)), fn3(n(2), n(3))),
        s"(${n(0)} $fn2 ${n(1)}) $fn1 (${n(2)} $fn3 ${n(3)})")//#3
      check(fn1(fn3(n(0), n(1)), fn2(n(2), n(3))),
        s"(${n(0)} $fn3 ${n(1)}) $fn1 (${n(2)} $fn2 ${n(3)})")//#3
      check(fn3(fn2(n(0), n(1)), fn1(n(2), n(3))),
        s"(${n(0)} $fn2 ${n(1)}) $fn3 (${n(2)} $fn1 ${n(3)})")//#3
      check(fn3(fn1(n(0), n(1)), fn2(n(2), n(3))),
        s"(${n(0)} $fn1 ${n(1)}) $fn3 (${n(2)} $fn2 ${n(3)})")//#3
      check(fn2(fn1(n(0), n(1)), fn3(n(2), n(3))),
        s"(${n(0)} $fn1 ${n(1)}) $fn2 (${n(2)} $fn3 ${n(3)})")//#3
      check(fn2(fn3(n(0), n(1)), fn1(n(2), n(3))),
        s"(${n(0)} $fn3 ${n(1)}) $fn2 (${n(2)} $fn1 ${n(3)})")//#3

      check(fn1(n(0), fn2(fn3(n(1), n(2)), n(3))),
        s"(${n(0)} $fn1 ((${n(1)} $fn3 ${n(2)}) $fn2 ${n(3)})")//#4
      check(fn1(n(0), fn3(fn2(n(1), n(2)), n(3))),
        s"(${n(0)} $fn1 ((${n(1)} $fn2 ${n(2)}) $fn3 ${n(3)})")//#4
      check(fn3(n(0), fn2(fn1(n(1), n(2)), n(3))),
        s"(${n(0)} $fn3 ((${n(1)} $fn1 ${n(2)}) $fn2 ${n(3)})")//#4
      check(fn3(n(0), fn1(fn2(n(1), n(2)), n(3))),
        s"(${n(0)} $fn3 ((${n(1)} $fn2 ${n(2)}) $fn1 ${n(3)})")//#4
      check(fn2(n(0), fn1(fn3(n(1), n(2)), n(3))),
        s"(${n(0)} $fn2 ((${n(1)} $fn3 ${n(2)}) $fn1 ${n(3)})")//#4
      check(fn2(n(0), fn3(fn1(n(1), n(2)), n(3))),
        s"(${n(0)} $fn2 ((${n(1)} $fn1 ${n(2)}) $fn3 ${n(3)})")//#4

      check(fn1(n(0), fn2(n(1), fn3(n(2), n(3)))),
        s"(${n(0)} $fn1 (${n(1)} $fn2 (${n(2)} $fn3 ${n(3)})))")//#5
      check(fn1(n(0), fn3(n(1), fn2(n(2), n(3)))),
        s"(${n(0)} $fn1 (${n(1)} $fn3 (${n(2)} $fn2 ${n(3)})))")//#5
      check(fn3(n(0), fn2(n(1), fn1(n(2), n(3)))),
        s"(${n(0)} $fn3 (${n(1)} $fn2 (${n(2)} $fn1 ${n(3)})))")//#5
      check(fn3(n(0), fn1(n(1), fn2(n(2), n(3)))),
        s"(${n(0)} $fn3 (${n(1)} $fn1 (${n(2)} $fn2 ${n(3)})))")//#5
      check(fn2(n(0), fn1(n(1), fn3(n(2), n(3)))),
        s"(${n(0)} $fn2 (${n(1)} $fn1 (${n(2)} $fn3 ${n(3)})))")//#5
      check(fn2(n(0), fn3(n(1), fn1(n(2), n(3)))),
        s"(${n(0)} $fn2 (${n(1)} $fn3 (${n(2)} $fn1 ${n(3)})))")//#5
    }

  }

  object + extends Function2[Int, Int, Int] {
    def apply(a: Int, b: Int): Int = a + b
    override def toString(): String = "+"
  }

  object - extends Function2[Int, Int, Int] {
    def apply(a: Int, b: Int): Int = a - b
    override def toString(): String = "-"
  }
  object * extends Function2[Int, Int, Int] {
    def apply(a: Int, b: Int): Int = a * b
    override def toString(): String = "*"
  }
  object / extends Function2[Int, Int, Int] {
    def apply(a: Int, b: Int): Int = try { a / b } catch { case _: ArithmeticException => Int.MaxValue }
    override def toString(): String = "/"
  }

}
