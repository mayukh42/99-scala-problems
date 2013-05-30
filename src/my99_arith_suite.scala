import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import my99_arith._

@RunWith(classOf[JUnitRunner])
class my99_arith_suite extends FunSuite {
  
  test("p31: isPrime") {
    assert(isPrime(7))
    assert(!isPrime(1))
    assert(!isPrime(28))
    assert(isPrime(29))
  }
  
  test("p32: gcd") {
    assert(gcd(21, 14)==7)
    assert(gcd(1, 7)==1)
    assert(gcd(36, 63)==9)
    assert(gcd(200, 1)==1)
  }
  
  test("p34: totient") {
    assert(totient(10)==4)
    assert(totient(20)==8)
    assert(totient(30)==8)
    assert(totient(41)==40)
  }
  
  test("p35a: primeFactorsSet") {
    assert(primeFactorsSet(10)==List(2,5))
    assert(primeFactorsSet(5)==List())
    assert(primeFactorsSet(64)==List(2))
    assert(primeFactorsSet(30)==List(2,3,5))
  }
  
  test("p35b: primeFactors") {
    assert(primeFactors(315)==List(3,3,5,7))
    assert(primeFactors(5)==List())
    assert(primeFactors(64)==List(2,2,2,2,2,2))
    assert(primeFactors(30)==List(2,3,5))
  }
  
  test("p36: primeFactorsMultiplicity") {
    assert(primeFactorsMultiplicity(315)==List((3,2), (5,1), (7,1)))
    assert(primeFactorsMultiplicity(5)==List())
    assert(primeFactorsMultiplicity(64)==List((2,6)))
    assert(primeFactorsMultiplicity(56)==List((2,3), (7,1)))
    assert(primeFactorsMultiplicity(10)==List((2,1), (5,1)))
  }
  
  test("p37: totientBetter") {    
    assert(totientBetter(10)==4)
    assert(totient(20)==totientBetter(20))
    assert(totient(30)==totientBetter(30))
    assert(totient(96)==totientBetter(96))
  }
  
  test("p38: totient compares") {
    /** @NOTES: 30x improvement for totientBetter. Truly better! 
     *  Best: 11.0 ms, 0.32 ms */
    timer(totient(10090))
    timer(totientBetter(10090))
  }
  
  test("p39: listPrimesInRange (both inclusive)") {    
    assert(listPrimesInRange(7, 31)==List(7, 11, 13, 17, 19, 23, 29, 31))
    assert(listPrimesInRange(1, 1)==List())
    assert(listPrimesInRange(1, 2)==List(2))
    assert(listPrimesInRange(6, 14)==List(7, 11, 13))
  }
  
  test("p40: goldbach") {
    assert(goldbach(10)==(3,7))
    assert(goldbach(20)==(3,17))
    assert(goldbach(28)==(5,23))
    assert(goldbach(96)==(7,89))
  }
  
  test("p41a: goldbachPairs") {    
    assert(goldbachPairs(9 to 20)==List((10,(3,7)), (12,(5,7)), (14,(3,11)), (16,(3,13)), (18,(5,13)), (20,(3,17))))
    assert(goldbachPairs(2 to -3)==List())
  }
  
  test("p41b: goldbachPairsLimited") {    
    assert(goldbachPairsLimited(1 to 2000, 50)==List((992,(73,919)), (1382,(61,1321)), (1856,(67,1789)), (1928,(61,1867))))
    assert(goldbachPairsLimited(2001 to 5000, 100)==List((2642,(103,2539))))
    assert(goldbachPairsLimited(2001 to 5000, 60)==List((2078,(61,2017)), (2438,(61,2377)), (2618,(61,2557)), (2642,(103,2539)), (3458,(67,3391)), (3794,(61,3733)), (3818,(79,3739)), (3848,(79,3769)), (4322,(61,4261)), (4418,(61,4357)), (4508,(61,4447)), (4618,(71,4547)), (4712,(61,4651)), (4718,(61,4657)), (4784,(61,4723)), (4886,(73,4813))))
    /** @NOTES: times for 3000, 48000, 498000 numbers (ceil(timer)):
    timer(goldbachPairsLimited(2001 to 5000, 60))		// 30 ms
    timer(goldbachPairsLimited(2001 to 50000, 60))		// 243 ms
    timer(goldbachPairsLimited(2001 to 500000, 60))		// 3276 ms */
  }

}