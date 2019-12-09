package cs320

package object hw09 extends Homework09 {
  def typeCheck(e: Expr, tyEnv: TyEnv): Type = e match {

    case Num(n) => NumT
    case Bool(b) => BoolT
    case Add(l, r) => mustSame(mustSame(NumT, typeCheck(l, tyEnv)), typeCheck(r, tyEnv))
    case Sub(l, r) => mustSame(mustSame(NumT, typeCheck(l, tyEnv)), typeCheck(r, tyEnv))
    case Eq(l, r) =>
      mustSame(mustSame(NumT, typeCheck(l, tyEnv)), typeCheck(r, tyEnv))
      BoolT
    case Lt(l, r) =>
      mustSame(mustSame(NumT, typeCheck(l, tyEnv)), typeCheck(r, tyEnv))
      BoolT
    case Id(x) => tyEnv.varMap.getOrElse(x, error(s"$x is a free id"))

    case Fun(params, b) => // params: List[(String,Type)]
      val type_list = params.map({case (p, t) => t}) //List[Type]
      type_list.map(t => validType(t, tyEnv))
      val tyEnv_1 = add_Vars(params, tyEnv)
      ArrowT(type_list, typeCheck(b, tyEnv_1))

    case App(f, args) => // f: Expr, args: List[Expr]
      val ArrowT(type_list, retTy) = typeCheck(f, tyEnv) // List[Type] => retTy
      val args_type = args.map(expr => typeCheck(expr, tyEnv)) //args -> List[Type]
      if (args_type == type_list) retTy else error(s"args has wrong type")

    case Block(stmts, expr) => //stmts: List[stmt]

      def Block_typeCheck(stmts: List[Stmt], tyenv: TyEnv):TyEnv = stmts match {
        case Nil => tyenv
        case h::t => 
          Block_typeCheck(t, typeCheck(tyenv, h))
      }

      typeCheck(expr, Block_typeCheck(stmts, tyEnv))

    case Assign(x, e) => 
      if (tyEnv.mutables.contains(x)) mustSame(tyEnv.varMap(x), typeCheck(e, tyEnv)) 
      else error(s"$x is not a var.")

    case Match(e, cases) => // cases: Map[String, (List[String], Expr) = (x11, x12,.. xnm), e] 
      val IdT(t) = typeCheck(e, tyEnv)
      val types = tyEnv.tbinds.getOrElse(t, error(s"no $t in tyEnv")) // types: Map[String, List[Type]= (t11,t12,...tnm)]
      val pair_list = cases.values.toList // pair_list : List[(List[String], Expr)] ((x11, x12,.. xnm), e1), 
      val types_list = types.values.toList // types_list : List[List[Types]] (t11,t12.,,t1m),,,,(tn1,...tnm)
      val fields_list = pair_list.map({case (a,b) => a}) // List[List[String]]
      val expr_list = pair_list.map({case (a,b) => b}) // List[Expr]
      val params_list = (fields_list zip types_list).map({case (a,b) => (a zip b)}) // List[List[(String, Types)] 
      val expr_params_list = expr_list zip params_list 
      
      val t_list = expr_params_list.map({case (expr, param) => typeCheck(expr, add_Vars(param, tyEnv))})
      t_list(0)

    case IfThenElse(cond, thenE, elseE) => 
      mustSame(typeCheck(cond, tyEnv),BoolT)
      mustSame(typeCheck(thenE, tyEnv), typeCheck(elseE, tyEnv))
  }

  def typeCheck(tyEnv: TyEnv, stmt: Stmt): TyEnv = stmt match {

    case Val(_, x, ty, e) => 
      validType(ty, tyEnv)
      mustSame(typeCheck(e, tyEnv), ty)
      tyEnv.addVar(x, ty, false)

    case Var(x, ty, e) => 
      validType(ty, tyEnv)
      mustSame(typeCheck(e, tyEnv), ty) 
      tyEnv.addVar(x, ty, true)

    case Def(x, params, retTy, body) => 
      val tyList = params.map({case (str, ty) => ty}) //List[Type]
      validType(retTy, tyEnv)
      tyList.map(ty => validType(ty, tyEnv))
      val tyEnv_1 = tyEnv.addVar(x, ArrowT(tyList, retTy), false)
      val tyEnv_2 = add_Vars(params, tyEnv_1)
      mustSame(typeCheck(body, tyEnv_2), retTy)
      tyEnv_2

    case Trait(t, cases) => //cases: Map [String, List[Type]]
      val tyEnv_1 = tyEnv.addTBind(t, cases)
      val class_name_list = cases.keys.toList // List[String]
      val class_type_list = cases.values.toList // List[List[Type]]
      val class_arrowT_list = class_type_list.map(ty_list => ArrowT(ty_list, IdT(t))) //List[ArrowT]
      val params = (class_name_list zip class_arrowT_list) // List[(String, Type)]
      val tyEnv_2 = add_Vars(params, tyEnv_1)
      class_type_list.flatten.map(ty => validType(ty, tyEnv_2))
      tyEnv_2
  }
  
  def interp(e: Expr, env: Env, sto: Sto): (Value, Sto) = e match {
    case Num(n) => (NumV(n), sto)
    case Bool(b) => (BoolV(b), sto)
    case Add(l, r) => 
      val (NumV(n), ls) = interp(l, env, sto) 
      val (NumV(m), rs) = interp(r, env, ls)
      (NumV(n + m), rs)

    case Sub(l, r) => 
      val (NumV(n), ls) = interp(l, env, sto) 
      val (NumV(m), rs) = interp(r, env, ls)
      (NumV(n - m), rs)

    case Eq(l, r) =>
      val (NumV(n), ls) = interp(l, env, sto) 
      val (NumV(m), rs) = interp(r, env, ls)
      (BoolV(n==m), rs)

    case Lt(l, r) =>
      val (NumV(n), ls) = interp(l, env, sto) 
      val (NumV(m), rs) = interp(r, env, ls)
      (BoolV(n < m), rs)

    case Id(x) => lookup(x, env) match {
      case AddrV(a) => storeLookup(a, sto) match {
        case ExprV(expr, eenv) => 
          val (v, sto_2) = interp(expr, eenv, sto)
          (v, sto_2 + (a -> v))
        case v => (v, sto)
      }
      case v => (v, sto)
    }

    case Fun(params, b) => // params: List[(String, Type)]
      val name_list = params.map({case (p, _) => p}) //List[String]
      (CloV(name_list, b, env), sto)

    case App(f, args) => 

      def args_evaluate(args: List[Expr], a_sto: Sto, v_list: List[Value]):(Sto, List[Value]) = {
        args match {
          case Nil => (a_sto, v_list)
          case h::t => 
            val (hv, hs) = interp(h, env, a_sto)
            args_evaluate(t, hs, v_list:::List(hv))
        }
      }

      val (fv, fs) = interp(f, env, sto) 
      fv match {
        case CloV(params, b, fenv) => // params: List[String]
          val (cs, v_list) = args_evaluate(args, fs, List())
          val len = params.length
      
          interp(b, fenv ++ ((params zip v_list).toMap), fs)
          
        case ConstructorV(name) => 
          val (cs, v_list) = args_evaluate(args, fs, List())
          (VariantV(name, v_list), cs)
      }

    case Block(stmts, expr) => 
      
      def Block_interp(stmts: List[Stmt], pair: (Env, Sto)): (Env, Sto) = 
        stmts match {
          case Nil => pair
          case h::t => 
            Block_interp(t, interp(pair, h))
        }
      
      val (be, bs) = Block_interp(stmts, (env,sto))
      interp(expr, be, bs)

    case Assign(x, e) => lookup(x, env) match {
      case AddrV(a) => 
        val (v, sto_2) = interp(e, env, sto) 
        (v, sto_2 + (a->v))
      case _ => error(s"not a var.")
    }

    case Match(e, cases) => // cases: Map[String, (List[String], Expr)]
      val (ev, es) = interp(e, env, sto)
      ev match { 
        case VariantV(name, values) => 
          val (name_list, expr) = cases.getOrElse(name, error(s"no Variant names $name"))
          interp(expr,env++(name_list zip values).toMap, es)

        case _ =>
          error(s"not a variantV")
      }

    case IfThenElse(cond, thenE, elseE) => 
      val (cv, cs) = interp(cond, env, sto) 
      cv match {
        case BoolV(true) => interp(thenE, env, cs)
        case BoolV(false) => interp(elseE, env, cs)
      } 
  }

  def interp(pair: (Env, Sto), stmt: Stmt): (Env, Sto) = stmt match {

    case Val(isLazy, x, _, e) => isLazy match {
      // val
      case false => 
        val (env, sto) = pair 
        val (v, sto_2) = interp(e, env, sto) 
        (env + (x->v), sto_2)
      // lazy val
      case true => 
        val (env, sto) = pair
        val a = malloc(sto)
        (env + (x -> AddrV(a)), sto + (a -> ExprV(e,env))) 
    }

    case Var(x, _, e) => 
      val (env, sto) = pair 
      val (v, sto_2) = interp(e, env, sto) 
      val a= malloc(sto_2)
      (env + (x -> AddrV(a)), sto_2 + (a -> v))

    case Def(x, params, _, body) => // params : List [(String, Type)]
      val (env, sto) = pair
      val param_list = params.map({case (p, t) => p}) // List[String]
      val cloV = CloV(param_list, body, env)
      cloV.env = env + (x -> cloV)
      (env + (x -> cloV), sto)

    case Trait(t, cases) => // cases: Map[String, List[Type]]
      val (env, sto) = pair
      val class_name_list = cases.keys.toList // List[String] : x1, x2, ... 
      val class_ConV_list = class_name_list.map(x => ConstructorV(x)) //List[ConV]: ConV(x1),...
      (env ++ (class_name_list zip class_ConV_list).toMap, sto)
  }

  // *******************************************************************************
  // ******************* HELPER FUNCTIONS ******************************************
  // *******************************************************************************
  
  def validType(ty: Type, tyEnv: TyEnv): Type = ty match {
    case NumT => ty
    case BoolT => ty 
    case ArrowT(p, r) => 
      val v_p = p.map(t => validType(t, tyEnv))
      ArrowT(v_p, validType(r, tyEnv))
    case IdT(x) => 
      if (tyEnv.tbinds.contains(x)) ty else error(s"$x is a free type")
    case _ => error(s"$ty is a wrong type")
  }
  def mustSame(left: Type, right: Type): Type = {
    if (same(left, right)) left 
    else error(s"$left is not equal to $right")
  }
  def same(left: Type, right: Type): Boolean = 
    (left, right) match { 
      case (NumT, NumT) => true
      case (BoolT, BoolT) => true
      case (IdT(x), IdT(y)) => if (x==y) true else false
      case (ArrowT(p1, r1), ArrowT(p2, r2)) => 
        same(r1, r2) && (p1==p2)
      case _ => false 
    }
  def lookup(x: String, env: Env): Value = {
    env.getOrElse(x, error(s"$x is a free id"))
  }
  def storeLookup(a: Addr, sto: Sto): Value = {
    sto.getOrElse(a, error(s"$a is a free id"))
  }
  def malloc(sto: Sto): Addr = 
    sto.foldLeft(0) {
      case (max, (addr,_)) => math.max(max, addr) 
    } + 1
  def add_Vars(params: List[(String, Type)], tenv: TyEnv): TyEnv = params match {
      case Nil => tenv
      case h::t => 
        val (p, ty) = h 
        add_Vars(t, tenv.addVar(p, ty, false))
    }

  // *******************************************************************************
  // **************************** TESTS ********************************************
  // *******************************************************************************

  def tests: Unit = {

    test(run("""
      var x: Int = 1
      val y: Int = (x = 3)
      x + y
    """), "6")
    
    test(run("""
      var x: Int = 1
      lazy val y: Int = (x = 3)
      x + y + x
    """), "7")
    
    test(run("""
      var x: Int = 0
      lazy val y: Int = (x = x + 1)
      val z: Int = y + y + y + y
      z"""), "4")

    testExc(run("""val x: Int = 42; x = 24"""), "")

    testExc(run("""
      trait AE
      case class Num(Int)
      case class Add(AE, AE)
      case class Sub(AE, AE)
      """), "")

    testExc(run("""
      trait Tree
      case class Leaf(Int)
      case class Node(Tree, Tree)

      def max(l: Int, r: Int): Int = {
        if (l < r) r else l
      }

    """), "")

    test(run("""
      trait AE
      case class Num(Int)
      case class Add(AE, AE)
      case class Sub(AE, AE)

      def interp(e: AE): Int = e match {
        case Num(n) => n
        case Add(l, r) => interp(l) + interp(r)
        case Sub(l, r) => interp(l) - interp(r)
      }

      interp(Add(Num(2), Sub(Num(3), Num(1))))
    """), "4")

    test(run("""
      trait Tree
      case class Leaf(Int)
      case class Node(Tree, Tree)

      def max(l: Int, r: Int): Int =
        if (l < r) r else l

      def depth(e: Tree): Int = e match {
        case Leaf(n) => 1
        case Node(l, r) => max(depth(l), depth(r)) + 1
      }

      depth(Node(Node(Leaf(1), Node(Leaf(2), Leaf(3))), Leaf(4)))
    """), "4")

    // *******************************************************************************
    // ****************************** MY TESTS ***************************************
    // *******************************************************************************
    print("************************** My Tests ************************************** \n")

    test(run("""
      var x: Int = 1
      x
    """), "1")

    test(run("((f: (Int, Int) => Int, x: Int, y: Int) => f(x, y)) ((a: Int, b: Int) => a + b, 1, 2)"), "3")

    test(run("""
      def add(x: Int, y: Int, z: Int): Int = x + y +z
      add(1, 2, 3)
      """), "6")

    test(run("""
      def add(x: Int): Int =
        x+1
        val x: Int = 3
        add(x - 1)
    """), "3")

  }
}
