package cs320

import cs320._

package object hw02 extends Homework02 {
  // applies a binary numeric function on all combinations of numbers from
  // the two input lists, and return the list of all of the results
  def binOp(
    op: (Int, Int) => Int,
    ls: List[Int],
    rs: List[Int]
  ): List[Int] = ls match {
    case Nil => Nil
    case l :: rest =>
      def f(r: Int): Int = op(l,r)
      rs.map(f) ++ binOp(op, rest, rs)
  }

  def run(str: String): List[Int] = {
    def runMW(MW: MUWAE,env: Map[String,List[Int]]):List[Int]= MW match {
    case Num(nums)=>nums
    case Add(l,r)=>binOp(_+_,runMW(l,env),runMW(r,env))
    case Sub(l,r)=>binOp(_-_,runMW(l,env),runMW(r,env))
    case With(x,i,b)=>runMW(b, env+(x->runMW(i,env)))
    case Id(s)=>env.getOrElse(s,throw new Exception)
    case Max(l,m,r)=>binOp(cpmax, binOp(cpmax,runMW(l,env),runMW(m,env)) , runMW(r,env))
    case Min(l,m,r)=>binOp(cpmin, binOp(cpmin,runMW(l,env),runMW(m,env)) , runMW(r,env))

    }
    runMW(MUWAE.apply(str), Map())
  }

  def cpmin(l:Int,r:Int):Int={
    if (l<r) l else r
  }

  def cpmax(l:Int,r:Int):Int={
    if (l>r) l else r
  }

  /*
  def Compare(ls:List[Int],rs:List[Int],op:(Int,Int)=>Boolean):List[Int]={

    for (n<-ls;m<-rs)
      yield if (op(n,m)) n else m // for max, op: _>_ for min, op: _<_

  }
  */

  def tests: Unit = {
  // 10 given tests
  test(run("{+ 3 7}"), List(10))
  test(run("{- 10 {3 5}}"), List(7, 5))
  test(run("{with {x {+ 5 5}} {+ x x}}"), List(20))
  test(run("{min 3 4 5}"), List(3))
  test(run("{max {+ 1 2} 4 5}"), List(5))
  test(run("{min {1 4} {2 9} 3}"), List(1, 1, 2, 3))
  test(run("{+ 3 7}"), List(10))
  test(run("{- 10 {3 5}}"), List(7, 5))
  test(run("{with {x {+ 5 5}} {+ x x}}"), List(20))
  test(run("{max {1 6} {2 5} {3 4}}"), List(3, 4, 5, 5, 6, 6, 6, 6))

  test(run("{-1 {-1}}"), List(2))
  test(run("{}"), List())
  test(run("{+ {} 0}"), List())
  test(run("{+ {} {0}}"), List())
  test(run("{- {0} {}}"), List())
  test(run("{+ {0 0} {1 1}}"), List(1, 1, 1, 1))
  test(run("{- {1 1} {0 0 0 0}}"), List(1, 1, 1, 1, 1, 1, 1, 1))

  test(run("{+ {+ {0 0} {0 0}} {+ {0 0} {1 1}}}"), List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
  test(run("{- {+ {+ {0 0} {0 0}} {+ {0 0} {1 1}}} {+ {+ {0 0} {0 0}} {+ {0 0} {1 1}}}}"), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  test(run("{+ {1 3} {0 1}}"), List(1, 2, 3, 4))
  test(run("{- {4 2} {0 1}}"), List(4, 3, 2, 1))

  test(run("{with {y 0} y}"), List(0))
  testExc(run("{with {y 0} x}"), "")
  test(run("{with {x {}} {with {y 0} {+ x y}}}"), List())
  test(run("{with {x {}} {with {y {0}} {+ x y}}}"), List())
  test(run("{with {x {}} {with {y {0}} {- y x}}}"), List())
  test(run("{with {x {0 0}} {with {y {1 1}} {+ x y}}}"), List(1, 1, 1, 1))
  test(run("{with {x {0 0 0 0}} {with {y {1 1}} {- y x}}}"), List(1, 1, 1, 1, 1, 1, 1, 1))

  test(run("{with {x {0 0}} {with {y {1 1}} {+ {+ x x} {+ x y}}}}"), List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
  test(run("{with {x {0 0}} {with {y {1 1}} {with {z {+ x x}} {+ z {+ x y}}}}}"), List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
  test(run("{with {x {0 0}} {with {y {1 1}} {with {z {+ x x}} {with {x {+ z {+ x y}}} {- x x}}}}}"), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  test(run("{+ {with {x {1 3}} {1 3}} {with {x {1 3}} {0 1}}}"), List(1, 2, 3, 4))
  test(run("{+ {with {x {1 3}} x} {with {x {0 1}} x}}"), List(1, 2, 3, 4))
  test(run("{- {with {x {4 2}} x} {with {x {0 1}} x}}"), List(4, 3, 2, 1))

  test(run("{min {} 2 3}"), List())
  test(run("{max 1 {} 3}"), List())
  test(run("{min 1 2 {with {z {}} z}}"), List())
  test(run("{max 1 2 {with {z 3} z}}"), List(3))
  test(run("{with {z 1} {min z 2 {with {z 3} z}}}"), List(1))
  test(run("{with {z 1} {max z 2 {with {z 3} z}}}"), List(3))
  test(run("{max 2 {1 3} {3 1}}"), List(3, 2, 3, 3))
  test(run("{min 2 {1 3} {1 3}}"), List(1, 1, 1, 2))
  test(run("{with {x {1 3}} {min 2 x x}}"), List(1, 1, 1, 2))
  test(run("{with {x {1 3}} {min x x x}}"), List(1, 1, 1, 1, 1, 1, 1, 3))
  test(run("{with {x {1 1 3}} {min x x x}}"), List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3))
  test(run("{with {x {3 3 1}} {max x x x}}"), List(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1))
  test(run("{with {x {1 3}} {with {x {min x x x}} {- {+ 1 x} 2}}}"), List(0, 0, 0, 0, 0, 0, 0, 2))
  testExc(run("{with {y {1 3}} {with {x {min x x x}} {- {+ 1 x} 2}}}"), "")
  test(run("{with {x {1 1 3}} {min x {- {+ 0 x} 0} {1 1 3}}}"), List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3))
  test(run("{min 1 2 {with {x {0 0}} {with {y {1 1}} {with {z {+ x x}} {with {x {+ z {+ x y}}} {- x x}}}}}}"), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  }

}
