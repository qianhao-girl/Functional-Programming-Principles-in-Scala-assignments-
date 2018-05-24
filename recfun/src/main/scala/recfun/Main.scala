package recfun
object Main extends App{
  def pascal(c:Int,r:Int):Int={
    def error(): Int ={
      println("the coordinate you giving is not in the Pascal Triangle")
      -1
    }
    def row():Array[Int] = {
      def nextLine(preLine: Array[Int]): Array[Int] = {
        val line: Array[Int] = new Array[Int](preLine.length + 1)
        line(0) = 1;line(line.length-1) = 1
        for (i <- 1 to (line.length - 2)) {
          line(i) = preLine(i - 1) + preLine(i)
        }
        //println("line.length in nextLine:  " + line.length)
        line
      }
      def rowIter():Array[Int] = {
        var pre = Array(1,1)
        var cur = pre
        for (i <- 2 to r) {
          cur = nextLine(pre)
          pre = cur
        }
        //println("cur.length in rowIter:  " + cur.length)
        cur
      }
      rowIter()
    }
    def pascal_(): Int ={
      if(r<2 || c==0 || c==r) 1 else row()(c)
    }
    if (r<c) error() else pascal_()
  }
//Week1.2 balance
  def balance(chars: List[Char]): Boolean ={
    val par_count = Array(0,0)
    def count(char:Char):Boolean={
      def countL(n:Int):Boolean = {
        par_count(0) = par_count(0) + n
        println(par_count(0))
        if (par_count(0) < 0) false else true
      }
      def countPar():Boolean = {
        if(char=='(') countL(1) else countL(-1)
      }
      def ispar():Boolean = {
        // Eliminate redundant if expressions where both branches return constant booleans
          // (char=='('||char==')') true else false
        char=='('||char==')'
      }
      if (!ispar())  true else countPar()
    }
    def countIter(chars: List[Char]):Boolean={
      val left:Char = chars.head;val right = chars.tail
      if(!count(left)||right.isEmpty) par_count(0)==0 else countIter(right)
    }
    countIter(chars)
  }
  //Week1.3 changes for coins
  def countChange(money:Int,coins:List[Int]): Int = {
    var ways_count = 0
    def addWays(i:Int):Unit={
      ways_count = ways_count + i
    }
    def changebycoin(mon:Int,coin:Int):Unit={
      if(mon!=0 && mon%coin==0) addWays(1)
    }
    def change(m:Int,cs:List[Int]):Unit={
      val head = cs.head;val tail = cs.tail
      val max_num = m / head;val residue = m % head;var accu = 0
      def incrementAccu():Unit={
        if(m-accu*head >= residue) change(m-accu*head,tail)
        if(accu<=max_num) accu = accu + 1
        if(accu <= max_num) incrementAccu()

      }
      if(head <= m) changebycoin(m,head)
      if(m>0 && tail.nonEmpty) incrementAccu()
    }
    if(money<=0||coins.isEmpty) 0 else change(money,coins)
    ways_count
  }
}


