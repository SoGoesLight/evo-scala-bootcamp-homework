package com.sogoeslight.homework.task2.classesandtraits

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Shape2DSpec extends AnyFlatSpec {

  "describePoint2D" should "be correct" in {
    Shape2D.describe(Shape2D.Point(15, 20)) shouldEqual "Point (x = 15.0, y = 20.0)"
  }

  "describeLine2D" should "be correct" in {
    Shape2D.describe(Shape2D.Line(5, 6, 10, 20)) shouldEqual "Line (x0 = 5.0, y0 = 6.0, x1 = 10.0, y1 = 20.0)"
  }

  "describeTriangle2D" should "be correct" in {
    Shape2D.describe(Shape2D.Triangle(1, 2, 3, 4, 5, 6)) shouldEqual "Triangle (num of sides = 3)"
  }

  "describeSquare2D" should "be correct" in {
    Shape2D.describe(Shape2D.Square(15, 20, 20)) shouldEqual "Square (side length = 20.0)"
  }

}
