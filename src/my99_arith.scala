object my99_arith {
  
  def isPrime(n: Int): Boolean = {
    /**@NOTES: Fermat's Little Theorem implementation is
     * limited by ln(Int.MAX_VALUE): 
     * return (n > 1) && (((1<<n) - 2) % n == 0)
     * The below one is trivial O(n**0.5)*/ 
    if (n < 2) return false
    val limit = Math.pow(n, 0.5).toInt
    for (i <- 2 to limit) {
      if ((n % i) == 0) return false
    }
    return true
  }
  
  def gcd(m: Int, n: Int): Int = {
    /**@NOTES: Euler's GCD Algorithm */
    if (n < 1) m
    else if (m > n) gcd(n, m-n)
    else gcd(m, n-m)
  }
  
  def isCoprimeTo(m: Int, n: Int): Boolean = gcd(m, n) == 1
  
  /** @NOTES: i until j is an IterableSequence, not a List. */
  def factors(n: Int): List[Int] = (2 until n).filter(x => n % x ==0).toList
  
  def totient(n: Int): Int = {
    def tot_rec(a: Int, m: Int, acc: List[Int]): List[Int] = {
      if (a > m) acc
      else if (gcd(a, m) == 1) tot_rec(a+1, m, a::acc)
      else tot_rec(a+1, m, acc)
    }
    val ans = tot_rec(1, n, List())
    ans.size
  }
  
  /** @NOTES: running iters upto n**0.5 would not catch some prime factors on the other
   *  side of sqrt, i.e. if the number is a product of 2 primes on either side of the sqrt */
  def primeFactorsSet(n: Int): List[Int] = factors(n).filter(x => n%x ==0 && isPrime(x)).toList
  
  def primeFactors(n: Int): List[Int] = {
    val limit = Math.pow(n, 0.5).toInt
    def pf_rec(x: Int, m: Int, acc: List[Int]): List[Int] = {
      if (x > limit ) acc
      else if (m%x == 0 && isPrime(x)) {
        val y = m/x
        if (isPrime(y) && y > limit) pf_rec(2, y, x::y::acc)
        else pf_rec(2, y, x::acc)
      }
      else pf_rec(x+1, m, acc)
    }
    pf_rec(2, n, List()).sortBy(x => x)
  }
  
  def primeFactorsMultiplicity(n: Int): List[(Int, Int)] = {
    val ans = primeFactors(n).groupBy((x: Int) => x) 
    /** @NOTES: O(n) + O(n) + O(nlgn) = O(nlgn) */
    ans.map(x => (x._1, (x._2).size)).toList.sortBy(x => x._1) 
  }
  
  def totientBetter(n: Int): Int = {
    val ps = primeFactorsMultiplicity(n)
    /** @NOTES: phi(m) = (p1-1)*p1**(m1-1) * (p2-1)*p2**(m2-1) * (p3-1)*p3**(m3-1) * ... 
     * valid only for composites; doesn't make sense for primes as totient = n-1
     *  */
    def tb_rec(xs: List[(Int, Int)], phi: Int): Int = {
      xs match {
        case (p, m)::ys => tb_rec(ys, phi * (p-1) * Math.pow(p, m-1).toInt)
        case Nil => phi
      }
    }
    tb_rec(ps, 1)
  }
  
  def listPrimesInRange(a: Int, b: Int): List[Int] = {
    def lpr_rec(x: Int, y: Int, acc: List[Int]): List[Int] = {
      if (x > y) acc
      else if (isPrime(x)) lpr_rec(x+1, y, x::acc)
      else lpr_rec(x+1, y, acc)
    }
    lpr_rec(a, b, List()).reverse
  }
  
  def goldbach(n: Int): (Int, Int) = {
    if (n > 2 && (n % 2 == 0)) {
      for (i <- 2 to n/2) {
        if (isPrime(i) && isPrime(n-i)) return (i, n-i)
      }
    } 
    return (-1, -1)
  }
  
  def goldbachPairs(xs: Range): List[(Int, (Int, Int))] = {
    val ys = xs.filter(x => x % 2 == 0)
    ys.map(y => (y, goldbach(y))).toList
  }
  
  def goldbachPairsLimited(xs: Range, lowermost: Int): List[(Int, (Int, Int))] = 
    goldbachPairs(xs).filter(x => x._2._1 > lowermost)
  
  /** @NOTES: to check for function running times */
  def timer[A](f: => A) = {
    val t = System.nanoTime()
    val ans = f
    println("time: " + (System.nanoTime()-t)/1e6 + " ms")
    ans
  }
  
  

}