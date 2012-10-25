package data.string

import org.scalacheck.Gen._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

object ArbitraryStringZipper {
  implicit val ArbStringZipper: Arbitrary[StringZipper] =
    Arbitrary(for {
      s <- alphaStr
      r <- choose(0, s.length)
      c <- alphaChar
      p <- arbitrary[Boolean]
      i <- choose(0, s.length / 8)
      j <- choose(0, s.length / 4)
    } yield (0 to j).foldLeft((0 to i).foldLeft({
        val x = (0 to r).foldLeft(StringZipper(s))((z, _) => z.right)
        if (p) x else x := c
      })((z, _) => z.left))((z, _) => z.right))

}
