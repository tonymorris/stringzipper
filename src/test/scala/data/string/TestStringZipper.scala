package data.string

import org.scalacheck.Properties
import org.scalacheck.Prop._
import ArbitraryStringZipper._

object TestStringZipper extends Properties("StringZipper") {
  property("length is consistent with unzipper") = forAll(
    (z: StringZipper) =>
      z.length forall (n => (-z) exists (_.length == n))
  )

  property("focus hasFocus") = forAll(
    (z: StringZipper) =>
      z.focus.isDefined == z.hasFocus
  )

  property("identity to focus is no-op") = forAll(
    (z: StringZipper) =>
      z ~ identity === z
  )

  property("apply to focus leaves length") = forAll(
    (z: StringZipper, f: Char => Char) =>
      (z ~ f).length == z.length
  )

  property("apply to focus alters focus") = forAll(
    (z: StringZipper, f: Char => Char) =>
      (z.focus map f) == (z ~ f).focus
  )

  property("set focus leaves length") = forAll(
    (z: StringZipper, c: Char) =>
      (z := c).length == z.length
  )

  property("set focus alters focus") = forAll(
    (z: StringZipper, c: Char) =>
      (z.focus map (_ => c)) == (z := c).focus
  )

  property("set apply focus retains") = forAll(
    (z: StringZipper, f: Char => Char, c: Char) =>
      (z := c) ~ f === (z := f(c))
  )

  property("choice returns one of") = forAll(
    (z1: StringZipper, z2: StringZipper) => {
      val r = z1 | z2
      r === z1 || r === z2
    }
  )

  property("move to index in length has focus") = forAll(
    (z: StringZipper, n: Int) => {
      val r = z ! n
      z.length forall (l => (n >= 0 && n < l) == r.hasFocus)
    }
  )

  property("empty has length 0") = forAll(
    (z: StringZipper) =>
      !z.isEmpty || z.length == Some(0)
  )

  property("empty has no focus") = forAll(
    (z: StringZipper) =>
      !(z.isEmpty && z.hasFocus)
  )

  property("in bounds has length") = forAll(
    (z: StringZipper) =>
      z.isInBounds || !z.length.isDefined
  )

  property("in bounds has focus") = forAll(
    (z: StringZipper) =>
      !z.isInBounds || z.focus.isDefined
  )

  property("empty is in bounds") = forAll(
    (z: StringZipper) =>
      !z.isEmpty || z.isInBounds
  )

  property("right n then left is back again") = forAll(
    (z: StringZipper, n : Int) => {
      val r = z + n
      !r.hasFocus || (r - n === z)
    }
  )

  property("left n then right is back again") = forAll(
    (z: StringZipper, n : Int) => {
      val r = z - n
      !r.hasFocus || (r + n === z)
    }
  )

  property("left negates right") = forAll(
    (z: StringZipper, n: Int) =>
      (z - n) == (z + (-n))
  )

  property("right negates left") = forAll(
    (z: StringZipper, n: Int) =>
      (z + n) == (z - (-n))
  )

  property("right then left is back again") = forAll(
    (z: StringZipper) => {
      val r = z.right
      !r.hasFocus || (r.left === z)
    }
  )

  property("left then right is back again") = forAll(
    (z: StringZipper) => {
      val r = z.left
      !r.hasFocus || (r.right === z)
    }
  )

  property("left focus == hasLeft") = forAll(
    (z: StringZipper) =>
      z.hasLeft == z.left.hasFocus
  )

  property("right focus == hasRight") = forAll(
    (z: StringZipper) =>
      z.hasLeft == z.left.hasFocus
  )

  property("nLeft >= 0") = forAll(
    (z: StringZipper) =>
      z.nLeft >= 0
  )

  property("nRight >= 0") = forAll(
    (z: StringZipper) =>
      z.nRight >= 0
  )

  property("(nLeft != 0) == hasLeft") = forAll(
    (z: StringZipper) =>
      (z.nLeft != 0) == z.hasLeft
  )

  property("(nRight != 0) == hasRight") = forAll(
    (z: StringZipper) =>
      (z.nRight != 0) == z.hasRight
  )

  property("nLeft can go left") = forAll(
    (z: StringZipper) =>
      !z.hasFocus || (z - z.nLeft).hasFocus
  )

  property("nRight can go right") = forAll(
    (z: StringZipper) =>
      !z.hasFocus || (z + z.nRight).hasFocus
  )

  property("dropLeft has no left") = forAll(
    (z: StringZipper) =>
      !z.dropLefts.hasLeft
  )

  property("dropRight has no right") = forAll(
    (z: StringZipper) =>
      !z.dropRights.hasRight
  )

  property("dropLeft keeps rights") = forAll(
    (z: StringZipper) =>
      z.dropLefts.nRight == z.nRight
  )

  property("dropRight keeps lefts") = forAll(
    (z: StringZipper) =>
      z.dropRights.nLeft == z.nLeft
  )

}
