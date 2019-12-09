package cs320

package object ex01 extends Exercise01 {
  // Problem 1
  def freeIds(expr: WAE): Set[String] =  {

     def frees(e: WAE, ids: Set[String]): Set[String] = e match {
      case Num(n) => Set()
      case Add(left, right) => frees(left, ids) ++ frees(right, ids)
      case Sub(left, right) => frees(left, ids) ++ frees(right, ids)
      case With(name, expr, body) => frees(expr, ids) ++ frees(body, ids + name)
      case Id(id) =>
        if (ids contains id) Set()
        else Set(id)
    }
    frees(expr, Set())

  }

  // Problem 2
  def bindingIds(expr: WAE): Set[String] = expr match {
    case Num(n) => Set()
    case Add(left, right) => bindingIds(left) ++ bindingIds(right)
    case Sub(left, right) => bindingIds(left) ++ bindingIds(right)
    case With(name, expr, body) => bindingIds(expr) ++ bindingIds(body) + name
    case Id(id) => Set()
  }
  // Problem 3
  def boundIds(expr: WAE): Set[String] = {
    def bounds(e: WAE, ids: Set[String]): Set[String] = e match {
      case Num(n) => Set()
      case Add(left, right) => bounds(left, ids) ++ bounds(right, ids)
      case Sub(left, right) => bounds(left, ids) ++ bounds(right, ids)
      case With(name, expr, body) => bounds(expr, ids) ++ bounds(body, ids + name)
      case Id(id) =>
        if (ids contains id) Set(id)
        else Set()
    }
    bounds(expr, Set())
  }
  // Tests
  def tests: Unit = {
    test(freeIds(WAE("{with {x 1} {+ x y}}")), Set("y"))
    test(freeIds(WAE("{with {z 2} 1}")), Set())
    test(bindingIds(WAE("{with {x 1} {+ x y}}")), Set("x"))
    test(bindingIds(WAE("{with {z 2} 1}")), Set("z"))
    test(boundIds(WAE("{with {x 1} {+ x y}}")), Set("x"))
    test(boundIds(WAE("{with {z 2} 1}")), Set())

    /* Write your own tests */
  }
}