object my99_lists {
  
  def last(xs: List[Int]): Int = {
    xs match {
      case y::Nil => y
      case y::ys => last(ys)
    }
  }
  
  def last_but_one(xs: List[Int]): Int = {
    xs match {
      case x::y::Nil => x
      case x::ys => last_but_one(ys)
    }
  }
  
  def kth(xs: List[Int], k: Int): Int = {
    /** k starts from 0 */
    xs match {
      case x::ys => if (k==0) x else kth(ys, k-1)
      case Nil => -1
    }
  }
  
  def len(xs: List[Any]): Int = {
    xs match {
      case x::ys => 1 + len(ys)
      case Nil => 0
    }
  }
  
  def reverse(xs: List[Any]): List[Any] = {
    xs match {
      case x::ys => reverse(ys):+x
      case Nil => Nil
    }
  }
  
  def isPalindrome(xs: List[Any]): Boolean = {
    /**@NOTE: slicing lists make for a better constant in the O(n) soln. */
    val ln = len(xs)
    val idx_2nd = if (ln%2==0) ln/2 else 1+ln/2
    xs.slice(0, ln/2) == reverse(xs.slice(idx_2nd, ln))    
  }
  
  def flatten(xs: List[Any]): List[Any] = {
    xs match {
      case y::ys => y match {
        case z::zs => z::flatten(zs) ++ flatten(ys)
        case _ => y::flatten(ys) 
      }
      case Nil => Nil
    }
  }
  
  def compress(xs: List[Any]): List[Any] = {
    def comp_rec(ys: List[Any], acc: List[Any]): List[Any] = {
      ys match {
        case z::zs => if (acc.isEmpty || z != acc.head) comp_rec(zs, z::acc)
        		else comp_rec(zs, acc)
        case Nil => acc
      }
    }
    comp_rec(xs, Nil).reverse
  }
  
  def pack(xs: List[Any]): List[Any] = {
    def pack_rec(ys: List[Any], acc1: List[Any], acc2: List[Any]): List[Any] = {
      ys match {
        case z::zs => if (acc1.isEmpty || z==acc1.head) pack_rec(zs, z::acc1, acc2)
        		else pack_rec(z::zs, Nil, acc1::acc2)
        case Nil => acc1::acc2
      }
    }
    pack_rec(xs, Nil, Nil).reverse
  }
  
  def encode(xs: List[Any]): List[Any] = {
    pack(xs).map(x => x match {
      case y::ys => (len(y::ys), y)
      case _ => Nil
    })
  }
  
  def encode_mod(xs: List[Any]): List[Any] = {
    pack(xs).map(x => x match {
      case y::Nil => y
      case y::ys => (len(y::ys), y)
      case _ => (0, "undefined")
    })
  }
  
  def decode(xs: List[Any]): List[Any] = {
    def gen(n: Int, x: Any): List[Any] = {
      if (n==0) Nil
      else x::gen(n-1, x)
    }
    def decode_rec(ys: List[Any], acc: List[Any]): List[Any] = {
      ys match {
      	case (p1: Int, p2)::ps => decode_rec(ps, gen(p1,p2)++acc)
      	case _ => acc
      }
    }
    reverse(decode_rec(xs, Nil))
  }
  
  def encodeDirect(xs: List[Any]): List[Any] = {
    def enc_rec(ys: List[Any], count: Int, acc: List[Any]): List[Any] = {
      ys match {
        case y::z::zs => if (y==z) enc_rec(z::zs, count+1, acc)
        		else enc_rec(z::zs, 0, (count+1, y)::acc)
        case z::zs => (count+1, z)::acc
        case _ => acc
      }
    }
    enc_rec(xs, 0, Nil).reverse
  }
  
  def duplicate(xs: List[Any]): List[Any] = {
    xs match {
      case y::ys => y::y::duplicate(ys)
      case Nil => Nil
    }
  }
  
  def duplicateN(xs: List[Any], n: Int): List[Any] = {
    def gen(n: Int, x: Any): List[Any] = {
      if (n==0) Nil
      else x::gen(n-1, x)
    }
    def dN_rec(ys: List[Any], n: Int): List[Any] = {
      ys match {
        case z::zs => gen(n, z)++dN_rec(zs, n)
        case Nil => Nil
      }
    }
    dN_rec(xs, n)
  }
  
  def dropN(xs: List[Any], n: Int): List[Any] = {
    def dropN_rec(ys: List[Any], m: Int, k: Int, acc: List[Any]): List[Any] = {
      ys match {
      	case z::zs => if (m==1) dropN_rec(zs, k, k, acc)
      			else dropN_rec(zs, m-1, k, z::acc)
      	case Nil => acc
      }
    }
    reverse(dropN_rec(xs, n, n, Nil))
  }
  
  def split(xs: List[Any], n: Int): (List[Any], List[Any]) = {
    def split_rec(ys: List[Any], m: Int, acc: List[Any]): (List[Any], List[Any]) = {
      ys match {
        case z::zs => if (m==0) (reverse(acc), z::zs)
        		else split_rec(zs, m-1, z::acc)
        case Nil => (Nil, Nil)
      }
    }
    split_rec(xs, n, Nil)
  }
  
  def slice(xs: List[Any], i: Int, j: Int): List[Any] = {
    def slice_rec(ys: List[Any], m: Int, n: Int, o: Int, acc: List[Any]): List[Any] = {
      ys match {
        case z::zs => if (o < m) slice_rec(zs, m, n, o+1, acc)
        		else if (o>=m && o < n) slice_rec(zs, m, n, o+1, z::acc)
        		else reverse(acc)
        case _ => reverse(acc)
      }
    }
    slice_rec(xs, i, j, 0, Nil)
  }
  
  def rotateN(xs: List[Any], n: Int): List[Any] = {
    def rotN_rec(ys: List[Any], n: Int, acc: List[Any]): List[Any] = {
      ys match {
        case z::zs => if (n > 0) rotN_rec(zs, n-1, z::acc)
        		else z::zs ++ reverse(acc)
        case Nil => reverse(acc)
      }
    }
    rotN_rec(xs, n, Nil)
  }
  
  def remove_kth(xs: List[Any], k: Int): (List[Any], Any) = {
    def rem_rec(ys: List[Any], n: Int, acc: List[Any]): (List[Any], Any) = {
      ys match {
        case z::zs => if (n > 1) rem_rec(zs, n-1, z::acc)
        		else (reverse(acc) ++ zs, z)
        case Nil => (reverse(acc), Nil)
      }
    }
    rem_rec(xs, k, List())
  }
  
  def insert_nth(xs: List[Any], n: Int, x: Any): List[Any] = {
    def ins_rec(ys: List[Any], m: Int, y: Any, acc: List[Any]): List[Any] = {
      ys match {
        case z::zs => if (m==0) reverse(acc) ++ (y::ys)
        		else ins_rec(zs, m-1, y, z::acc)
        case Nil => reverse(y::acc)
      }
    }
    ins_rec(xs, n, x, List())
  }
  
  def range_incl(i: Int, j: Int): List[Int] = {
    if (i > j) Nil
    else i::range_incl(i+1, j)
  }
  
  def main(args: Array[String]): Unit = {
    /**@TODO: main() not needed if run with JUnit only */
  }

}

