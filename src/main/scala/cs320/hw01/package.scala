package cs320

package object hw01 extends Homework01 {
  // 1. Primitives (20 pts)
  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = a*b*c

  def concat(x: String, y: String): String = x+y

  // 2. Function Values (30 pts)
  def addN(n: Int): Int => Int = {
    def g(x: Int): Int = n+x
    g
  }
  
  def twice(f: Int => Int): Int => Int = {
    // f= addN(3)
    def g(x:Int): Int=f(f(x))
    g
  }
  def compose(f: Int => Int, g: Int => Int): Int => Int = {
    def k(x: Int): Int=f(g(x))
    k
  }

  // 3. Data Structures (50 pts)
  // 3.1. Lists (20 pts)
  def double(l: List[Int]): List[Int] = {
    l.map(_ * 2 )
  }

  def sum(l: List[Int]): Int = {
    l.foldLeft(0)(_+_)
  }

  // 3.2. Maps (10 pts)
  def getKey(m: Map[String, Int], s: String): Int = {
    val h : Option[Int] = m.get(s)
    if (h.isDefined) h.get else error(s)
  }

  // 3.3. User-defined Structures (20 pts)
  def countLeaves(t: Tree): Int = t match {
    case Leaf(value) => 1
    case Branch(left,value,right) => countLeaves(left)+countLeaves(right)
    
    //if (t.getClass==Leaf) 1 else countLeaves(t.left)+countLeaves(t.right)

  }

  def flatten(t: Tree): List[Int] = t match {
    case Leaf(value) => List(value)
    case Branch(left,value,right) => flatten(left):::List(value):::flatten(right)
  }





  def tests: Unit = {
    
    test(concat("abc", "def"), "abcdef") 
    test(addN(5)(3), 8)
    test(addN(5)(42), 47)
    test(twice(addN(3))(2), 8) 
    test(twice(addN(3))(7), 13) 
    test(compose(addN(3), addN(4))(5), 12)
    test(compose(addN(3), addN(4))(11), 18) 

    val l: List[Int] = List(1, 2, 3)
    test(double(l), List(2, 4, 6))
    test(double(double(l)), List(4, 8, 12))

    test(sum(List(1,2,3)), 6)
    test(sum(List(4,2,3,7,5)), 21)

    val m: Map[String, Int] = Map("Ryu" -> 42, "PL" -> 37)
    test(getKey(m, "Ryu"), 42)
    test(getKey(m, "PL"), 37)
    testExc(getKey(m, "CS320"), "CS320")

    val tree: Tree = Branch(Leaf(1), 2, Branch(Leaf(3), 4, Leaf(5)))
    test(countLeaves(tree), 3)
    test(flatten(tree), List(1, 2, 3, 4, 5))

    /* Write your own tests */

    println("-----mytest-------")
    println("1. Primitives")

    test(volumeOfCuboid(4,5,3),60)
    test(concat("abcdef","ff"),"abcdefff")

    println("2. Function Values")

    test(addN(-14)(2),-12)
    test(twice(addN(-2))(1),-3)
    test(compose(addN(-5),addN(5))(21),21)

    println("3. Data Structures")

    println("* Lists *")
    val l2: List[Int] = List(7,2,-1,0)
    test(sum(double(double(l2))),32)
    val n: List[Int]=Nil
    test(sum(n),0)

    println("* Map *")

    val m2: Map[String,Int]=Map("Park"->22,"Code"->160255)
    test(getKey(m2,"Park"),22)
    test(getKey(m2,"Code"),160255)
    testExc(getKey(m2,"Co"),"Co")

    println("* Tree *")

    val tree2: Tree = Branch(Branch(Branch(Leaf(2),3,Leaf(8)),1,Leaf(4)),5,Branch(Leaf(3),2,Leaf(6)))
    test(countLeaves(tree2), 5)
    test(flatten(tree2), List(2,3,8,1,4,5,3,2,6))

  }
}
