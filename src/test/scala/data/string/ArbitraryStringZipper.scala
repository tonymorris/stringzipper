package data.string

import org.scalacheck.Gen._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

object ArbitraryStringZipper {
  implicit val ArbStringZipper: Arbitrary[StringZipper] =
    Arbitrary(for {
      s <- arbitrary[String]
      r <- choose(0, s.length)
    } yield (0 to r).foldLeft(StringZipper(s))((z, _) => z.right))
}