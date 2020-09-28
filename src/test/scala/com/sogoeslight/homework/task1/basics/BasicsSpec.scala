package com.sogoeslight.homework.task1.basics

import Basics._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class BasicsSpec extends AnyFlatSpec {
  "gcd" should "be correct" in {
    gcd(0, 0) shouldEqual 0
    gcd(2, 6) shouldEqual 2
    gcd(-2, 6) shouldEqual 2
    gcd(653356, 356) shouldEqual 4
  }

  "lcm" should "be correct" in {
    lcm(45, 4) shouldEqual 180
    lcm(7, 15) shouldEqual 105
    lcm(653356, 356) shouldEqual 58148684
  }

}
