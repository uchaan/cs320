package cs320

package object hw07 extends Homework07 {
  trait KXCFAEV
  case class NumV(n: Int) extends KXCFAEV
  case class CloV(params: List[String], body: KXCFAE, env: Env) extends KXCFAEV
  case class ContV(proc: Cont) extends KXCFAEV
  case object ThrowV extends KXCFAEV
  

  type Cont = KXCFAEV => KXCFAEV
  type Env = Map[String, KXCFAEV]

  def run(str: String): String = {
    val v = interp(KXCFAE.apply(str), Map(), x=>x)
    val2str(v)
  }

  def val2str(v: KXCFAEV): String= v match {
    case NumV(n) => n.toString
    case CloV(p, b, env) => "function"
    case ThrowV => error(s"no enclosing try-catch")
    case ContV(kv) => "continuation"
  }

  def interp(kcfae: KXCFAE, env: Env, k: Cont): KXCFAEV = kcfae match {
    case Num(n) => k(NumV(n))
    case Add(l, r) => 
      interp(l, env, lv => 
        interp(r, env, rv => 
          k(num_ops(lv,rv,_+_))))
    case Sub(l, r) => 
      interp(l, env, lv => 
        interp(r, env, rv => 
          k(num_ops(lv,rv,_-_))))
    case Id(x) => k(env.getOrElse(x, error(s"$x is a free identifier")))
    case If0(c, i, e) => 
      interp(c, env, cv => 
        if (if_cmp(cv)) interp(i, env, k) else interp(e, env, k))
    case Fun(params, b) => k(CloV(params, b, env))
    case App(f, args) => 
      args match {
        case Nil =>  // size(ps) == size(args) or size(ps) > size(args)
          interp(f, env, fv =>
            fv match {
              case CloV(ps, b, fenv) =>
                if (ps==Nil) interp(b, env, k) else error(s"wrong arity")
            })
        case h::t => 
          interp(f, env, fv =>
            interp(h, env, hv => 
              fv match {
                case CloV(ps, b, fenv) =>
                    ps match {
                    case Nil => error(s"wrong arity")// if size(ps) < size(args)
                    case ph::pt => 
                      interp(App(Fun(pt, b), t), fenv +(ph -> hv), k)
                    }  
                case ContV(kv) => kv(hv)
                case v => error(s"not a closure")
              }))
      }
    case Withcc(x, b) => 
      interp(b, env + (x -> ContV(k)), k)
    case Try(tE, cE) => 
      interp(tE, env, tv => 
        if (tv==ThrowV) interp(cE, env, k) else tv)
    case Throw => k(ThrowV)
       //if (k(ThrowV)==ThrowV) error(s"no enclosing try-catch") else k(ThrowV)
    
    /*
    case class Try(tryE: KXCFAE, catchE: KXCFAE) extends KXCFAE               //     | {try e catch e}
    case object Throw extends KXCFAE                          
    */
  }

  def if_cmp(cv: KXCFAEV):Boolean = cv match {
    case NumV(n) => 
      if (n==0) true else false
    case ThrowV => 
      error(s"no enclosing try-catch")
  }
  

  def num_ops(lv:KXCFAEV, rv:KXCFAEV, f:(Int,Int)=>(Int)):KXCFAEV = {
    lv match {
      case NumV(n1) => rv match {
        case NumV(n2) => NumV(f(n1,n2))
        case ThrowV => ThrowV
      } 
      case ThrowV => ThrowV
    }
  } 

  def tests: Unit = {
    
    test(run("{{fun {x y} {- y x}} 10 12}"), "2")
    test(run("{fun {} 12}"), "function")
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
    test(run("{withcc esc {{fun {x y} x} {esc 3} 1}}"), "3")
    test(run("{withcc esc {{fun {x y} x} {esc 4} {esc 3}}}"), "4")
    test(run("{{fun {x y} x} {withcc esc {esc 1}} 2}"), "1")
    
    print("************** try-catch *******************\n")
    
    test(run("{try 1 catch 2}"), "1")
    
    test(run("{try {throw} catch 2}"), "2")
    test(run("{try {+ 1 {throw}} catch 2}"), "2")
    test(run("{{fun {f} {try {f} catch 1}} {fun {} {throw}}}"), "1")
    test(run("{{fun {f} {try {f} catch 1}} {fun {} 2}}"), "2")
    testExc(run("{throw}"), "no enclosing try-catch")
    testExc(run("{if0 {throw} 1 2}"), "no enclosing try-catch")
    test(run("{if0 1 {throw} 2}"),"2")
    print("************** IF0 *********************\n")

    /* Write your own tests */
    test(run("{if0 {+ 1 2} {- 3 2} {- 8 1}}"), "7")
    test(run("{if0 {- 2 2} {- 3 2} {- 8 1}}"), "1")
    
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

    testExc(run("{{fun {x y z} x} {throw} 1 2 3}"), "wrong arity")
    testExc(run("{1 {throw}}"), "not a closure")

    test(run("{try {fun {} {throw}} catch 1}"), "function")
    testExc(run("{{try {fun {} {throw}} catch 1}}"), "no enclosing try-catch")
    print(run("{{try {fun {} {throw}} catch 1}}"))
    print(interp(KXCFAE.apply("{{try {fun {} {throw}} catch 1}}"), Map(), x=>x))
  }
}
