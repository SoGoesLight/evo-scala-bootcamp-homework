package com.sogoeslight.homework.task3.controlstructures

import com.sogoeslight.homework.task3.controlstructures.ControlStructures._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ControlStructuresSpec extends AnyFlatSpec {

  "process" should "work as expected" in {
    process("divide 4 5") shouldEqual "4 divided by 5 is 0.8"
    process("sum 5 5 6 8.5") shouldEqual "the sum of 5 5 6 8.5 is 24.5"
    process("average 4 3 8.5 4") shouldEqual "the average of 4 3 8.5 4 is 4.875"
    process("min 4 -3 -17") shouldEqual "the minimum of 4 -3 -17 is -17"
    process("max 4 -3 -17") shouldEqual "the maximum of 4 -3 -17 is 4"
  }

}
