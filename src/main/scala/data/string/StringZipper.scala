package data.string

sealed trait StringZipper {
  // The length of the zipper.
  // O(1) if there have not been any length-altering updates, O(n) otherwise.
  def length: Option[Int] =
    this match {
      case EmptyZ => Some(0)
      case OffBoundsZ => None
      case StringZ(_, x, _, y) => Some(y - x)
      case ListZ(l, _, r, n) => n orElse Some(l.length + 1 + r.length)
    }

  // The current focus of the zipper, if there is one.
  def focus: Option[Char] =
    this match {
      case EmptyZ => None
      case OffBoundsZ => None
      case StringZ(s, _, i, _) => Some(s(i))
      case ListZ(_, x, _, _) => Some(x)
    }

  // Return whether or not this zipper has focus.
  def hasFocus: Boolean =
    this match {
      case EmptyZ => false
      case OffBoundsZ => false
      case _ => true
    }

  // Run the given function on the focus of the zipper (or no-op if there is no focus).
  def ~(k: Char => Char): StringZipper =
    this match {
      case EmptyZ => EmptyZ
      case OffBoundsZ => OffBoundsZ
      case StringZ(s, x, i, y) => {
        val c = s(i)
        val d = k(c)
        if(c == d)
          this
        else {
          val (lefts, _, rights) = StringZListZ(s, x, i, y)
          ListZ(lefts, d, rights, Some(y - x))
        }

      }
      case ListZ(l, x, r, n) => ListZ(l, k(x), r, n)
    }

  // Set the focus to the given value (or no-op if there is no focus).
  def :=(c: Char): StringZipper =
    this ~ (_ => c)

  // Return this zipper if it has a focus, otherwise, the given zipper.
  def |(other: => StringZipper) =
    if(hasFocus) this else other

  // Move the focus to the given absolute index in the zipper.
  def !(q: Int): StringZipper =
    if(q < 0)
      OffBoundsZ
    else
      this match {
        case EmptyZ => OffBoundsZ
        case OffBoundsZ => OffBoundsZ
        case StringZ(s, x, i, y) => 
          if(q >= x && q < y)
            StringZ(s, x, q, y)
          else
            OffBoundsZ   
        case ListZ(l, x, r, n) => 
          // if carrying length, check bounds quickly
          if(n forall (q < _)) {
            @annotation.tailrec
            def tryLeft(n: Int, z: StringZipper, g: Int): (Int, StringZipper) =
              if(n == 0)
                (g, z)
              else
                z.left match {
                  case EmptyZ => (0, EmptyZ)
                  case OffBoundsZ => (g, OffBoundsZ)
                  case zz => tryLeft(n - 1, zz, g + 1)
                }
            val (h, u) =  tryLeft(q, this, 0)
            u match {
              case EmptyZ => EmptyZ
              case OffBoundsZ => this + (q - h)
              case _ => this - u.nLeft
            }
          } else
            OffBoundsZ
      }

  // Returns whether this zipper is of an empty string.
  def isEmpty: Boolean =
    this == EmptyZ

  // Returns whether the focus of this zipper is within bounds.
  def isInBounds: Boolean = 
    this != OffBoundsZ

  // Move the focus of this zipper to the right the given number of times.
  @annotation.tailrec
  final def +(q: Int): StringZipper =
    this match {
      case EmptyZ => OffBoundsZ
      case OffBoundsZ => OffBoundsZ
      case StringZ(s, x, i, y) => {
        val b = i + q
        if(b >= x && b < y)
          StringZ(s, x, b, y)
        else
          OffBoundsZ
      }      
      case ListZ(l, x, r, n) =>
        if(q == Int.MinValue)
          l match {
            case Nil => OffBoundsZ
            case h::t => ListZ(t, h, x::t, n) - Int.MaxValue
          }          
        else if(q < 0)
          this - (-q)
        else if(q == 0)
          ListZ(l, x, r, n)
        else
          r match {
            case Nil => OffBoundsZ
            case h::t => (ListZ(x::l, h, t, n): StringZipper) + (q - 1)
          }
    }

  // Move the focus of this zipper to the left the given number of times.
  @annotation.tailrec
  final def -(q: Int): StringZipper =
    this match {
      case EmptyZ => OffBoundsZ
      case OffBoundsZ => OffBoundsZ
      case StringZ(s, x, i, y) => {
        val b = i - q
        if(b >= x && b < y)
          StringZ(s, x, b, y)
        else
          OffBoundsZ
      }
      case ListZ(l, x, r, n) =>
        if(q == Int.MinValue)
          r match {
            case Nil => OffBoundsZ
            case h::t => ListZ(x::l, h, t, n) + Int.MaxValue
          }          
        else if(q < 0)
          this + (-q)
        else if(q == 0)
          ListZ(l, x, r, n)
        else
          l match {
            case Nil => OffBoundsZ
            case h::t => (ListZ(t, h, x::r, n): StringZipper) - (q - 1)
          }
    }

  // Move the focus of this zipper one position to the left.
  def left: StringZipper =
    this - 1

  // Move the focus of this zipper one position to the right.
  def right: StringZipper =
    this + 1

  // Returns whether this zipper has a value to the left of focus.
  def hasLeft: Boolean =
    this match {
      case EmptyZ => false
      case OffBoundsZ => false
      case StringZ(_, x, i, _) => i >= x
      case ListZ(q, _, _, _) => !q.isEmpty
    }

  // Returns whether this zipper has a value to the right of focus.
  def hasRight: Boolean =
    this match {
      case EmptyZ => false
      case OffBoundsZ => false
      case StringZ(_, _, i, y) => i < y
      case ListZ(_, _, q, _) => !q.isEmpty
    }

  // Returns the number of values to the left of focus (or 0 if there is no focus).
  def nLeft: Int =
    this match {
      case EmptyZ => 0
      case OffBoundsZ => 0
      case StringZ(_, x, i, _) => i - x
      case ListZ(q, _, _, _) => q.length
    }

  // Returns the number of values to the right of focus (or 0 if there is no focus).
  def nRight: Int =
    this match {
      case EmptyZ => 0
      case OffBoundsZ => 0
      case StringZ(_, _, i, y) => y - i - 1
      case ListZ(_, _, q, _) => q.length
    }

  // Drop all values to the left of focus (no-op if there is no focus).
  def dropLefts: StringZipper =
    this match {
      case EmptyZ => EmptyZ
      case OffBoundsZ => OffBoundsZ
      case StringZ(s, x, i, y) => StringZ(s, i, i, y)
      case ListZ(_, x, r, _) => ListZ(Nil, x, r, None)
    }

  // Drop all values to the right of focus (no-op if there is no focus).
  def dropRights: StringZipper =
    this match {
      case EmptyZ => EmptyZ
      case OffBoundsZ => OffBoundsZ
      case StringZ(s, x, i, y) => StringZ(s, x, i, i + 1)
      case ListZ(l, x, _, _) => ListZ(l, x, Nil, None)
    }

  // Move the focus of this zipper to the first position to the left that matches the given predicate (no-op if there is no focus).
  @annotation.tailrec
  final def <<:(p: Char => Boolean): StringZipper =
    this match {
      case EmptyZ => EmptyZ
      case OffBoundsZ => OffBoundsZ
      case _ => {
          val l = left
          l.focus match {
            case None => this
            case Some(q) => if(p(q)) l else p <<: l
          }
        }
    }

  // Move the focus of this zipper to the first position to the right that matches the given predicate (no-op if there is no focus).
  @annotation.tailrec
  final def :>>(p: Char => Boolean): StringZipper = 
    this match {
      case EmptyZ => EmptyZ
      case OffBoundsZ => OffBoundsZ
      case _ => {
          val r = right
          r.focus match {
            case None => this
            case Some(q) => if(p(q)) r else r :>> p
          }
        }
    }

  // Drop the focus and move left, out of bounds if there is no left (no-op if there is no focus).
  def dropGoLeft: StringZipper =
    this match {
      case EmptyZ => EmptyZ
      case OffBoundsZ => OffBoundsZ
      case StringZ(s, x, i, y) => 
        if(y - x == 1)
          EmptyZ
        else {
          val j = i - 1
          if(j >= x && j < y) 
            StringZListZLefts(s, x, i) match {
              case Nil => OffBoundsZ
              case h::t => ListZ(t, h, StringZListZRights(s, i, y), Some(y - x - 1))
            }          
          else
            OffBoundsZ
      }
      case ListZ(l, _, r, n) => 
        l match {
          case Nil => if(r.isEmpty) EmptyZ else OffBoundsZ
          case h::t => ListZ(t, h, r, n map (_-1))
        }
    }    

  // Drop the focus and move right, out of bounds if there is no right (no-op if there is no focus).
  def dropGoRight: StringZipper =
    this match {
      case EmptyZ => EmptyZ
      case OffBoundsZ => OffBoundsZ
      case StringZ(s, x, i, y) => 
        if(y - x == 1)
          EmptyZ
        else {
          val j = i - 1
          if(j >= x && j < y)
            StringZListZRights(s, i, y) match {
              case Nil => OffBoundsZ
              case h::t => ListZ(StringZListZLefts(s, x, i), h, t, Some(y - x - 1))
            }          
          else
            OffBoundsZ
      }
      case ListZ(l, _, r, n) => 
        r match {
          case Nil => if(l.isEmpty) EmptyZ else OffBoundsZ
          case h::t => ListZ(l, h, t, n map (_-1))
        }
    }

  // Insert the given element to the left of focus (no-op if there is no focus).
  def <+:(q: Char): StringZipper =
    this match {
      case EmptyZ => EmptyZ
      case OffBoundsZ => OffBoundsZ
      case StringZ(s, x, i, y) => {
        val (lefts, w, rights) = StringZListZ(s, x, i, y)        
        ListZ(q::lefts, w, rights, Some(y - x + 1))
      }
      case ListZ(l, x, r, n) => ListZ(q::l, x, r, n map (_+1))
    }

  // Insert the given elements to the left of focus (no-op if there is no focus).
  def <++:(s: String): StringZipper =
    s.foldRight(this)(_ <+: _)

  // Insert the given element to the right of focus (no-op if there is no focus).
  def :+>(q: Char): StringZipper =
    this match {
      case EmptyZ => EmptyZ
      case OffBoundsZ => OffBoundsZ
      case StringZ(s, x, i, y) => {
        val (lefts, w, rights) = StringZListZ(s, x, i, y)
        ListZ(lefts, w, q::rights, Some(y - x + 1))
      }
      case ListZ(l, x, r, n) => ListZ(l, x, q::r, n map (_+1))
    }

  // Insert the given elements to the right of focus (no-op if there is no focus).
  def :++>(s: String): StringZipper =
    s.foldRight(this)((a, b) => b :+> a)

  // All zippers to the left of focus.
  def lefts: List[StringZipper] = {
    var l = left
    var b = new collection.mutable.ListBuffer[StringZipper]

    while(l.hasFocus) {
      b += l
      l = l.left
    }
    
    b.toList
  }

  // All zippers to the right of focus.
  def rights: List[StringZipper] = {
    var r = right
    var b = new collection.mutable.ListBuffer[StringZipper]

    while(r.hasFocus) {
      b += r
      r = r.right
    }
    
    b.toList
  }

  // Run the given function on the entire zipper (or no-op if there is no focus).
  def ~~(k: Char => Char): StringZipper =
    this match {
      case EmptyZ => EmptyZ
      case OffBoundsZ => OffBoundsZ
      case StringZ(s, x, i, y) => {
        val (lefts, w, rights) = StringZListZ(s, x, i, y)
          ListZ(lefts map k, k(w), rights map k, Some(y - x))        
      }
      case ListZ(l, x, r, n) => ListZ(l map k, k(x), r map k, n)
    }

  // Is the focus equal to this character?
  def ?=(c: Char): Boolean =
    focus exists (_ == c)

  // Does this character exist to the left of focus?
  def ?<:(c: Char): Boolean =
    lefts exists (_ == c)

  // Does this character exist to the right of focus?
  def :>?(c: Char): Boolean =
    rights exists (_ == c)

  // Swap the focus with the element to the left of focus (no-op if there is no focus).
  def swapLeft: StringZipper =
    this match {
      case EmptyZ => EmptyZ
      case OffBoundsZ => OffBoundsZ
      case StringZ(s, x, i, y) => {
        val j = i - 1
        if(j >= x && j < y) 
          StringZListZLefts(s, x, i) match {
            case Nil => OffBoundsZ
            case h::t => ListZ(s(i)::t, h, StringZListZRights(s, i, y), Some(y - x))
          }          
        else
          OffBoundsZ
      }
      case ListZ(l, x, r, n) => 
        l match {
          case Nil => if(r.isEmpty) EmptyZ else OffBoundsZ
          case h::t => ListZ(x::t, h, r, n)
        }
    }    

  // Swap the focus with the element to the right of focus (no-op if there is no focus).
  def swapRight: StringZipper =
    this match {
      case EmptyZ => EmptyZ
      case OffBoundsZ => OffBoundsZ
      case StringZ(s, x, i, y) => {
        val j = i - 1
        if(j >= x && j < y) 
          StringZListZRights(s, i, y) match {
            case Nil => OffBoundsZ
            case h::t => ListZ(StringZListZLefts(s, x, i), h, s(i)::t, Some(y - x))
          }          
        else
          OffBoundsZ
      }
      case ListZ(l, x, r, n) => 
        r match {
          case Nil => if(r.isEmpty) EmptyZ else OffBoundsZ
          case h::t => ListZ(l, h, x::t, n)
        }
    }

  // Compare two zippers for equality.
  def ===(z: StringZipper): Boolean =
    this match {
      case EmptyZ => z.isEmpty
      case OffBoundsZ => !z.isInBounds
      case StringZ(s, x, i, y) =>
        z match {
          case EmptyZ => false
          case OffBoundsZ => false
          case StringZ(ss, xx, ii, yy) => StringZListZ(s, x, i, y) == StringZListZ(ss, xx, ii, yy)
          case ListZ(ll, xx, rr, _) => StringZListZ(s, x, i, y) == (ll, xx, rr)
        }
      case ListZ(l, x, r, _) =>
        z match {
          case EmptyZ => false
          case OffBoundsZ => false
          case StringZ(ss, xx, ii, yy) => (l, x, r) == StringZListZ(ss, xx, ii, yy)
          case ListZ(ll, xx, rr, _) => (l, x, r) == (ll, xx, rr)
        }
    }

  // Compare two zippers for inequality.
  def !==(z: StringZipper): Boolean =
    !(===(z))

  // Returns a possible string representation of this zipper (no value if the zipper is out of bounds).
  def unary_- : Option[String] =
    this match {
      case EmptyZ => Some("")
      case OffBoundsZ => None
      case StringZ(s, x, i, y) => Some(s.substring(x, y))
      case ListZ(l, x, r, n) => Some({
          val b = n match {
            case Some(o) => new StringBuilder(o)
            case None => new StringBuilder
          }
          l foreach (b insert (0, _))
          b += x          
          r foreach (b += _)
          b.toString
        })
    }    
          /*
  // A string representation of this zipper. Empty string if the zipper is out of bounds.
  override def toString: String =
    this match {
      case EmptyZ => "∅"
      case OffBoundsZ => "∞"
      case StringZ(s, x, i, y) => {
        val b = new StringBuilder(s.length)
        b.append(s.substring(x, i))
        b += '⋙'
        b += s(i)
        b += '⋘'
        b.append(s.substring(i, y))
        b.toString
      }
      case ListZ(l, x, r, n) => {
        val b = n match {
          case Some(o) => new StringBuilder(o)
          case None => new StringBuilder
        }
        l foreach (b insert (0, _))
        b += '⋙'
        b += x
        b += '⋘'
        r foreach (b += _)
        b.toString
      }
    }
        */
  // BEGIN unsafe, unexported
  private def StringZListZLefts(s: String, x: Int, i: Int): List[Char] = {
    var lefts: List[Char] = Nil
    x to i-1 foreach (c => lefts = s(c) :: lefts)
    lefts
  }

  private def StringZListZRights(s: String, i: Int, y: Int): List[Char] = {
    var rights: List[Char] = Nil
    y-1 to i+1 by -1 foreach (c => rights = s(c) :: rights)
    rights
  }

  private def StringZListZ(s: String, x: Int, i: Int, y: Int): (List[Char], Char, List[Char]) =
    (StringZListZLefts(s, x, i), s(i), StringZListZRights(s, i, y))
  // END unsafe, unexported

}
private case object EmptyZ extends StringZipper
private case object OffBoundsZ extends StringZipper
private case class StringZ(s: String, start: Int, index: Int, end: Int) extends StringZipper
private case class ListZ(ls: List[Char], x: Char, rs: List[Char], l: Option[Int]) extends StringZipper

object StringZipper {
  def apply(s: String): StringZipper =
    if(s.length == 0)
      EmptyZ
    else
      StringZ(s, 0, 0, s.length)

  case class ZipperString(s: String) {
    def unary_+ : StringZipper =
      StringZipper(s)
  }

  implicit def StringZipperString(s: String): ZipperString =
    ZipperString(s)
}
