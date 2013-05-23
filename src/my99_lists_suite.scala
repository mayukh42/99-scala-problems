import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import my99_lists._

@RunWith(classOf[JUnitRunner])
class my99_lists_suite extends FunSuite {

  val xs = List(1,1,2,3,5,8,13,21,34,55,89,144,233,377,600)
  val ys = List(1,1,2,3,2,1,1)
  val zs = List('a','a','a','t','c','c','a','a','g','h','h','h')
  val ns = List(List(1, 1), 2, List(3, List(5, 8)))
  val ds = List(1,4,9)
    
  test("p1: last") {
    assert(last(xs)==600)
  }
  
  test("p2: last_but_one") {
    assert(last_but_one(xs)==377)
  }
  
  test("p3: kth, k starts from 0") {
    assert(kth(xs, 2)==2)
  }
  
  test("p4: len") {
    assert(len(xs)==15)
  }
  
  test("p5: reverse") {
    assert(reverse(xs)==List(600, 377, 233, 144, 89, 55, 34, 21, 13, 8, 5, 3, 2, 1, 1))
  }
  
  test("p6: isPalindrome") {
    assert(isPalindrome(xs)==false)
    assert(isPalindrome(ys)==true)
    assert(isPalindrome(List(1,3,3,1))==true)
    assert(isPalindrome(List())==true)
  }
  
  test("p7: flatten") {
    assert(flatten(ns)==List(1, 1, 2, 3, 5, 8))
  }
  
  test("p8: compress") {
    assert(compress(zs)==List('a', 't', 'c', 'a', 'g', 'h'))
  }
  
  test("p9: pack") {
    assert(pack(zs)==List(List('a','a','a'), List('t'), List('c','c'), List('a','a'), List('g'), List('h','h','h')))
  }
  
  test("p10: encode") {
    assert(encode(zs)==List((3,'a'), (1,'t'), (2,'c'), (2,'a'), (1,'g'), (3,'h')))
  }
  
  test("p11: encode_mod") {
    assert(encode_mod(zs)==List((3,'a'), 't', (2,'c'), (2,'a'), 'g', (3, 'h')))
  }
  
  test("p12: decode") {
    assert(decode(encode(zs))==zs)
  }
  
  test("p13: encodeDirect") {
    assert(encodeDirect(zs)==List((3,'a'), (1,'t'), (2,'c'), (2,'a'), (1,'g'), (3,'h')))
  }
  
  test("p14: duplicate") {
    assert(duplicate(ds)==List(1,1,4,4,9,9))
    assert(len(duplicate(xs))==2*len(xs))
  }
  
  test("p15: duplicateN") {
    assert(duplicateN(ds, 3)==List(1,1,1,4,4,4,9,9,9))
    assert(len(duplicateN(xs, 5))==5*len(xs))
  }
  
  test("p16: dropN") {
    assert(dropN(ys, 3)==List(1,1,3,2,1))
    assert(dropN(ys, 5)==List(1,1,2,3,1,1))
    val ln = len(xs)
    assert(len(dropN(xs, 5))==ln-ln/5)
  }
  
  test("p17: split") {
    assert(split(ys, 3)==(List(1,1,2), List(3,2,1,1)))
    val pair = split(xs, 10)
    assert(len(pair._1)==10 && len(pair._2)==len(xs)-10)
  }
  
  test("p18: slice") {
    assert(slice(xs, 5, 10)==List(8,13,21,34,55))
    assert(slice(xs, 0, 10)==List(1,1,2,3,5,8,13,21,34,55))
    assert(slice(xs, 12, 20)==List(233,377,600))
  }
  
  test("p19: rotateN") {
    assert(rotateN(ys, 3)==List(3,2,1,1,1,1,2))
    assert(len(rotateN(xs, 5))==len(rotateN(xs, 10)))
  }
  
  test("p20: remove_kth") {
    assert(remove_kth(ys, 3)==(List(1,1,3,2,1,1), 2))
    assert(remove_kth(zs, 5)==(List('a','a','a','t','c','a','a','g','h','h','h'), 'c'))
  }
  
  test("p21: insert_nth") {
    assert(insert_nth(ys, 3, 5)==List(1,1,2,5,3,2,1,1))
    assert(insert_nth(ds, 0, 0)==0::ds)
    assert(insert_nth(ds, len(ds), 16)==List(1,4,9,16))
  }
  
  test("p22: range_incl") {
    val i = 10; val j = 16
    assert(range_incl(4, 9)==List(4,5,6,7,8,9))
    assert(range_incl(-5, 4)==List(-5,-4,-3,-2,-1,0,1,2,3,4))
    assert(len(range_incl(i, j))==j+1-i)
  }
  
}