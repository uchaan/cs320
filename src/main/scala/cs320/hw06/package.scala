package cs320

package object hw06 extends Homework06 {
  
  trait SRBFAEV
  case class NumV (n: Int) extends SRBFAEV
  case class CloV (p: String, b: SRBFAE, e: Env) extends SRBFAEV
  case class BoxV (a: Addr) extends SRBFAEV
  case class BoxV_R (a: Addr) extends SRBFAEV
  case class RecV (rec: Map[String, SRBFAEV]) extends SRBFAEV
  
  type Env = Map[String, SRBFAEV]
  type Addr = Int 
  type Sto = Map[Addr, SRBFAEV]

  def malloc(sto: Sto): Addr = 
    sto.foldLeft(0) {
      case (max, (addr,_)) => math.max(max, addr) 
    } + 1

  def run(str: String): String = {
    val (v, s) = interp(SRBFAE.apply(str), Map(), Map())
    val2str(v)
  }

  def val2str(v: SRBFAEV): String= v match {
    case NumV(n) => n.toString
    case CloV(p, b, env) => "function"
    case BoxV(a) => "box"
    case BoxV_R(a) => "record"
    case RecV(rec) => "record"
  }

  def interp(e: SRBFAE, env: Env, sto: Sto): (SRBFAEV, Sto) = e match {
    case Num(n) => (NumV(n), sto)
    case Id(x) => (env.getOrElse(x, throw new Exception), sto)
    case Fun(x, b) => (CloV(x, b, env), sto)
    case Add(l, r) => 
      val (NumV(n), ls) = interp(l, env, sto)
      val (NumV(m), rs) = interp(r, env, ls) 
      (NumV(n + m), rs)
    case Sub(l, r) => 
      val (NumV(n), ls) = interp(l, env, sto)
      val (NumV(m), rs) = interp(r, env, ls) 
      (NumV(n - m), rs)
    case App(f, a) => 
      val (CloV(x, b, fEnv), ls) = interp(f, env, sto)
      val (v, rs) = interp(a, env, ls)
      interp(b, fEnv + (x -> v), rs)
    case NewBox(e) =>
      val (v, s) = interp(e, env, sto)
      val a = malloc(s)
      (BoxV(a), s + (a -> v))
    case SetBox(b, e) =>
      val (BoxV(a), bs) = interp(b, env, sto)
      val (v, es) = interp(e, env, bs) 
      (v, es + (a -> v))
    case OpenBox(e) => 
      val (BoxV(a), s) = interp(e, env, sto) 
      (s.getOrElse(a, throw new Exception), s)
    case Seqn(l,r) => // l: SRBFAE r: List[SRBFAE]
      val (lv, ls) = interp(l, env, sto)
      r match {
        case Nil => (lv,ls)
        case h::t => interp(Seqn(h,t), env, ls)
      }
    case Rec(fields) => // List[(String, SRBFAE)] -> Map[String,SRBFAEV]
      // r: Map[String, SRBFAEV], s: Updated Store while interpreting expressions in fields
      val (r, s) = rec_map(fields, env, sto, Map()) 
      val a = malloc(s) // address a for a -> r in store s. 
      // RecV(r) extends SRBFAEV
      (BoxV_R(a), s + (a -> RecV(r))) 
    case Get(rec, x) => 
      val (BoxV_R(a), s) = interp(rec, env, sto)
      val Record = s.getOrElse(a, throw new Exception) // Record: RecV( rec )
      Record match {
        case RecV(rec) => 
          if (rec.contains(x)) (rec.getOrElse(x, throw new Exception), s) else error(s"no such field")
        case _ => error(s"not a record")
      }
    case Set(rec, x, e) => 
      val (BoxV_R(a), s) = interp(rec, env, sto) 
      val (ev, es) = interp(e, env, s) // ev is a new value for x 
      val Record = es.getOrElse(a, throw new Exception) //Record: RecV (rec) rec+(x->ev)
      Record match {
        case RecV(rec) => 
          if (rec.contains(x)) (ev, es + (a -> RecV(rec + (x -> ev)))) else error(s"no such field")
          
        case _ => error(s"not a record")
      }
  }

  def rec_map(rec: List[(String, SRBFAE)], env:Env, sto:Sto, reco:Map[String,SRBFAEV]): (Map[String, SRBFAEV], Sto) = {
    rec match {
      case Nil => (reco, sto)
      case h::t => 
        val (x, e) = h 
        val (v, s) = interp(e, env, sto) // new SRBFAEV and Store 
        rec_map(t, env, s, reco + (x -> v))
    }
  }
  
  def tests: Unit = {
    test(run("""{{fun {b} {seqn {setbox b {+ 2 {openbox b}}}
                          {setbox b {+ 3 {openbox b}}}
                          {setbox b {+ 4 {openbox b}}}
                          {openbox b}}}
                {newbox 1}}"""), "10")
    
    testExc(run("{get {rec {x 1}} y}"), "no such field")
  
    test(run("{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}"), "5")
    
    
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{newbox 1}"), "box")
    
    test(run("{rec}"), "record")
    
    

    /* Write your own tests */
    test(run("{{fun {x} {get {rec {a {openbox x}} {b {seqn {setbox x 3} 5}}} a}}{newbox 1}}"),"1")
    test(run("{{fun {x} {get {rec {b {seqn {setbox x 3} 5}} {a {openbox x}}} a}}{newbox 1}}"),"3")
    test(run("{get {rec {x 1} {x 2} {x 3} {x 4}} x}"),"4")
    test(run("{{fun {r} {set r x 5}} {rec {x 1}}}"),"5" )
    test(run("{get {rec {b 10} {b {+ 1 2}}} b}"),"3")
    testExc(run("{{fun {r} {seqn {set r y 5} {get r y}}} {rec {x 1}}}"), "no such field")
  }
}
  
