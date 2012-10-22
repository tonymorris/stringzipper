package data.string

import org.scalacheck.Properties
import org.scalacheck.Prop._
import ArbitraryStringZipper._

object TestStringZipper extends Properties("StringZipper") {
  property("length is consistent with unzipper") = forAll(
    (z: StringZipper) =>
      z.length forall (n => (-z) exists (_.length == n))
  )
}
