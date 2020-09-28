package com.sogoeslight.homework.task2.classesandtraits

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Shape3DSpec extends AnyFlatSpec {

  "describePoint3D" should "be correct" in {
    Shape3D.describe(Shape3D.Point(15, 20, 25)) shouldEqual "Point (x = 15.0, y = 20.0, z = 25.0)"
  }

  "describeLine3D" should "be correct" in {
    Shape3D.describe(Shape3D.Line(5, 6, 10, 20, 2, 6)) shouldEqual
      "Line (x0 = 5.0, y0 = 6.0, x1 = 10.0, y1 = 20.0, z0 = 2.0, z1 = 6.0)"
  }

  "describePyramid3D" should "be correct" in {
    Shape3D.describe(Shape3D.Pyramid(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) shouldEqual
      "Pyramid (num of sides is more than 3)"
  }

  "describeCube3D" should "be correct" in {
    Shape3D.describe(Shape3D.Cube(15, 20, 25, 20)) shouldEqual "Cube (side length = 20.0)"
  }

  "describeSphere3D" should "be correct" in {
    Shape3D.describe(Shape3D.Sphere(15, 20, 25, 3456)) shouldEqual "Sphere is pretty round"
  }

}
