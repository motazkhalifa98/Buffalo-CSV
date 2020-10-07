/**
 * GenSeqTests.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: motazkha
 * Person#: 50231669
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa0.tests

import cse250.pa0.objects.Functions
import org.scalatest.FlatSpec

class GenSeqTests extends FlatSpec {

  behavior of "FunctionsTest.genSeq"
for (i <- 0 to 1000) {
  it should "be all even" in {
    assert(Functions.genSeq(i).forall(_ % 2 == 0))
  }
  it should "be same size" in {
    assert(Functions.genSeq(i).size == i)
  }
}
}
