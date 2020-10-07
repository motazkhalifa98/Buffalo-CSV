/**
 * CompSumTests.scala
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
 * cite: http://tutorial.math.lamar.edu/Classes/CalcI/SummationNotation.aspx
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa0.tests

import cse250.pa0.objects.Functions
import org.scalatest.FlatSpec

class CompSumTests extends FlatSpec {

  behavior of "FunctionsTest.compSum"
 for( i <- 1 to 50000) {
  it should "summation" in {
    assert(Functions.genSeq(i) == (i*(i+1)/2))
  }
 }
}
