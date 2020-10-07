/**
 * FunThreeTests.scala
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

class FunThreeTests extends FlatSpec {

  behavior of "FunctionsTest.funThree"
for( i <- 0 to 1000) {
  it should "non-decreasing function" in {
   if (i != 1000) assert(Functions.funThree(i) <= Functions.funThree(i+1))
  }
}
}
