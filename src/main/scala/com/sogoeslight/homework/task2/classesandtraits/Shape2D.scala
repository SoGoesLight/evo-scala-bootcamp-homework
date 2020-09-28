package com.sogoeslight.homework.task2.classesandtraits

object Shape2D {

  // Homework
  //
  // Add additional 2D shapes such as triangle and square.
  //
  // Add method `area` to 2D shapes.
  //
  // If some of the implementation involves advanced math, it is OK
  // to leave it unimplemented

  sealed trait Shape2D extends Located with Bounded with Movable with Calculated

  sealed trait Located {
    def x: Double
    def y: Double
    def origin: Array[Double] = Array(x, y)
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Movable {
    def move(dx: Double, dy: Double): Shape2D
  }

  sealed trait Calculated {
    def area: Double
  }

  final case class Point(x: Double, y: Double) extends Shape2D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y

    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)

    override def area: Double = 0
  }

  final case class Line(x0: Double, y0: Double, x1: Double, y1: Double) extends Shape2D {
    override def x: Double = (x0 + x1) / 2
    override def y: Double = (y0 + y1) / 2

    override def minX: Double = x0 min x1
    override def maxX: Double = x0 max x1
    override def minY: Double = y0 min y1
    override def maxY: Double = y0 max y1

    override def move(dx: Double, dy: Double): Line =
      Line(x0 + dx, y0 + dy, x1 + dx, y1 + dy)

    override def area: Double = 0
  }

  final case class Triangle(x0: Double, y0: Double,
                            x1: Double, y1: Double,
                            x2: Double, y2: Double) extends Shape2D {
    override def x: Double = (x0 + x1 + x2) / 3
    override def y: Double = (y0 + y1 + y2) / 3

    override def minX: Double = x0 min x1 min x2
    override def maxX: Double = x0 max x1 max x2
    override def minY: Double = y0 min y1 min y2
    override def maxY: Double = y0 max y1 max y2

    override def move(dx: Double, dy: Double): Triangle =
      Triangle(x0 + dx, y0 + dy, x1 + dx, y1 + dy, x2 + dx, y2 + dy)

    override def area: Double = ??? // some unnecessary calculation here
  }

  final case class Square(x0: Double, y0: Double, side: Double) extends Shape2D {
    override def x: Double = x0 + side / 2
    override def y: Double = y0 + side / 2

    override def minX: Double = x0
    override def maxX: Double = x0 + side
    override def minY: Double = y0
    override def maxY: Double = y0 + side

    override def move(dx: Double, dy: Double): Square =
      Square(x0 + dx, y0 + dy, side)

    override def area: Double = side * side
  }

  def describe(x: Shape2D): String =
    x match {
      case Point(x, y)                      => s"Point (x = $x, y = $y)"
      case Line(x0, y0, x1, y1)             => s"Line (x0 = $x0, y0 = $y0, x1 = $x1, y1 = $y1)"
      case Triangle(x0, y0, x1, y1, x2, y2) => s"Triangle (num of sides = 3)"
      case Square(x0, y0, sideSize)         => s"Square (side length = $sideSize)"
      case _                                => """¯\_(ツ)_/¯"""
    }
}
