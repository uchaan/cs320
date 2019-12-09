package cs320

package object hw03 extends Homework03 {
  // define MRFWAEValue
  trait MRFWAEValue
  case class NumV(n:Int) extends MRFWAEValue
  case class CloV(ps: List[String], b: MRFWAE, e: Env) extends MRFWAEValue
  case class RecV(rec:Map[String,MRFWAEValue]) extends MRFWAEValue
  type Env=Map[String,MRFWAEValue] 

  def run(str: String): String = {

    def interp(MR:MRFWAE, env:Map[String,MRFWAEValue]):MRFWAEValue= MR match {
      case Num(num)=>NumV(num)
      case Add(l,r)=>
        val NumV(n)=interp(l,env)
        val NumV(m)=interp(r,env)
        NumV(n+m)    // MRFWAEValue type
      case Sub(l,r)=>
        val NumV(n)=interp(l,env)
        val NumV(m)=interp(r,env)
        NumV(n-m)    // MRFWAEValue type
      case With(x,i,b)=> interp(b,env+(x->interp(i,env)))
      // MRFWAEValue type
      case Id(x) => env.getOrElse(x,throw new Exception)
      case Fun(ps,b)=> CloV(ps,b,env)
        // ps: List[String] , b : MRFWAE / {fun {x*} e}
      case App(f,a) => interp(f,env) match {
        case CloV(ps,b,fenv)=>
          if (ps.length!=a.length) error(s"wrong arity") else interp(b, fenv++arguments(ps,a.map(n=>interp(n,Map())),ps.length,Map()))
        case v=>
          error(s"not a closure: $v")
      }
      case Rec(rec) => RecV(rec.map{case (k,v)=>(k,interp(v,env))})
      // Map(String->MRFWAE) -> Map(String->MRFWAEValue)
      case Acc(expr,name) => interp(expr,env) match {
        case RecV(rec)=> if (rec.contains(name)) rec(name) else error(s"no such field")
        case v=> 
          error(s"no such field")
      }
      // MRFWAE, String
    }
  

    val2str(interp(MRFWAE.apply(str),Map())) // return MRFWAEValue 
    // convert MRFWAEValue to String
  }

  def val2str(mr:MRFWAEValue):String= mr match {
    case NumV(n)=>n.toString
    case CloV(ps,b,env)=>"function"
    case RecV(rec)=>"record"
  }

  // put function arguments to map ( List(x,y)-> List ( NumV(1),NumV(2) => ))
  def arguments(l1: List[String],l2: List[MRFWAEValue], n:Int, env: Map[String,MRFWAEValue]): Map[String,MRFWAEValue] = {
    if (n==0) env else arguments(l1,l2,n-1,env+(l1(n-1)->l2(n-1)))
  }

  def tests: Unit = {

    test(run("{{{fun {x} {fun {x} x}} 1} 2}"), "2") // ok 

    test(run("{{fun {x y} {+ x y}} 1 2}"), "3") //ok
    test(run("{{fun {} {+ 3 4}}}"), "7") //ok
    testExc(run("{{fun {x y} {+ x y}} 1}"), "wrong arity")

    // test shadowing
    
    test(run("{access {record {x 1} {y 2}} x}"), "1")
    testExc(run("{access {record {x 1} {y 2}} z}"), "no such field")
    testExc(run("{record {x {access {record {y 1}} z}}}"), "no such field")

    test(run("42"), "42") //ok
    test(run("{fun {x} x}"), "function") // ok 
    test(run("{record {x 1}}"), "record")

    /* Write your own tests */

    test(run("{{fun {x y} {+ x y}} {+ 1 2} {- 9 2}}"), "10")
    testExc(run("{access {record {z {record {x 2}}}} x}"),"no such field")
    test(run("{with {x {fun {x} {+ x 5}}} {x 7}}"),"12")
    test(run("{access {record {x 1} {y 2} {z {with {x 3} {+ x x}}}} z}"), "6")
    testExc(run("{access { with {x {fun {x} {+ x 2}}} {x 2}} x}"),"no such field")
    test(run("{with {x {record {y 1} {z 2}}} {access x y}}"), "1")
    testExc(run("{with {x {record {y 1} {z 2}}} {access x w}}"), "no such field")

    test(run("{{fun {} 3}}"), "3")
    test(run("{withcc k {+ 1 {k 2}}}"), "2")
    test(run("{withcc done {{withcc esc {done {+ 1 {withcc k {esc k}}}}} 3}}"), "4")
    test(run("{withcc esc {{fun {x} {+ 1 x}} {esc 3}}}"), "3")
    test(run("{+ 1 {{fun {x y} {+ x y}} 3 2}}"), "6")
    test(run("{+ 1 {withcc esc {{fun {x} {+ 2 x}} {esc 3}}}}"), "4")
    test(run("{+ 1 {withcc esc {{fun {x y} {+ 2 x}} {esc 3} 2}}}"), "4")
    test(run("{+ 1 {withcc esc {{fun {x y} {+ 2 x}} 2 {esc 3}}}}"), "4")
    test(run("{withcc esc {{fun {x y} x} {esc 4} {esc 3}}}"), "4")

    test(run("{try 1 catch 2}"), "1")
    test(run("{try {throw} catch 2}"), "2")
    test(run("{try {+ 1 {throw}} catch 2}"), "2")
    test(run("{{fun {f} {f 3}} {fun {x} {+ x 1}}}"), "4")
    test(run("{{fun {f} {try {f} catch 1}} {fun {} {throw}}}"), "1")
    testExc(run("{throw}"), "no enclosing try-catch")
    test(run("{if0 {- 1 1} 2 3}"), "2")
  }
  
}
