package com.sogoeslight.homework.task2.classesandtraits

object Shape3D {

  // Homework:
  //
  // Add 3D shapes classes
  // (origin, point, sphere, cube, cuboid, 3D triangle - you can add
  // others if you think they are a good fit).
  //
  // Add methods `surfaceArea` and `volume` to 3D shapes.
  //
  // If some of the implementation involves advanced math, it is OK
  // to leave it unimplemented

  sealed trait Shape3D extends Located with Bounded with Movable with Calculated

  sealed trait Located {
    def x: Double
    def y: Double
    def z: Double
    def origin: Array[Double] = Array(x, y, z)
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Movable {
    def move(dx: Double, dy: Double, dz: Double): Shape3D
  }

  sealed trait Calculated {
    def surfaceArea: Double
    def volume: Double
  }

  final case class Point(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z

    override def move(dx: Double, dy: Double, dz: Double): Point =
      Point(x + dx, y + dy, z + dz)

    override def surfaceArea: Double = 0
    override def volume: Double = 0
  }

  final case class Line(x0: Double, y0: Double, z0: Double, x1: Double, y1: Double, z1: Double) extends Shape3D {
    override def x: Double = (x0 + x1) / 2
    override def y: Double = (y0 + y1) / 2
    override def z: Double = (z0 + z1) / 2

    override def minX: Double = x0 min x1
    override def maxX: Double = x0 max x1
    override def minY: Double = y0 min y1
    override def maxY: Double = y0 max y1
    override def minZ: Double = z0 min z1
    override def maxZ: Double = z0 max z1

    override def move(dx: Double, dy: Double, dz: Double): Line =
      Line(x0 + dx, y0 + dy, x1 + dx, y1 + dy, z0 + dz, z1 + dz)

    override def surfaceArea: Double = 0
    override def volume: Double = 0
  }

  final case class Pyramid(
      x0: Double, y0: Double, z0: Double,
      x1: Double, y1: Double, z1: Double,
      x2: Double, y2: Double, z2: Double
  ) extends Shape3D {
    override def x: Double = (x0 + x1 + x2) / 3
    override def y: Double = (y0 + y1 + y2) / 3
    override def z: Double = (z0 + z1 + z2) / 3

    override def minX: Double = x0 min x1 min x2
    override def maxX: Double = x0 max x1 max x2
    override def minY: Double = y0 min y1 min y2
    override def maxY: Double = y0 max y1 max y2
    override def minZ: Double = z0 min z1 min z2
    override def maxZ: Double = z0 max z1 max z2

    override def move(dx: Double, dy: Double, dz: Double): Pyramid =
      Pyramid(x0 + dx, y0 + dy, z0 + dz,
              x1 + dx, y1 + dy, z1 + dz,
              x2 + dx, y2 + dy, z2 + dz)

    override def surfaceArea: Double = ??? // some unnecessary calculation here
    override def volume: Double = ??? // another "advanced math" here
  }

  final case class Sphere(x: Double, y: Double, z: Double, radius: Double) extends Shape3D {
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def minZ: Double = z - radius
    override def maxZ: Double = z + radius

    override def move(dx: Double, dy: Double, dz: Double): Sphere =
      Sphere(x + dx, y + dy, z + dz, radius)

    override def surfaceArea: Double =  ??? // some unnecessary calculations here
    override def volume: Double = ??? // another "advanced math" here
  }

  final case class Cube(x0: Double, y0: Double, z0: Double, side: Double) extends Shape3D {
    override def x: Double = x0 + side / 2
    override def y: Double = y0 + side / 2
    override def z: Double = z0 + side / 2

    override def minX: Double = x0
    override def maxX: Double = x0 + side
    override def minY: Double = y0
    override def maxY: Double = y0 + side
    override def minZ: Double = z0
    override def maxZ: Double = z0 + side

    override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(x0 + dx, y0 + dy, z0 + dz, side)

    override def surfaceArea: Double = side * side * 6
    override def volume: Double = side * side * side
  }

  def describe(x: Shape3D): String =
    x match {
      case Point(x, y, z)                     => s"Point (x = $x, y = $y, z = $z)"
      case Line(x0, y0, x1, y1, z0, z1)       => s"Line (x0 = $x0, y0 = $y0, x1 = $x1, y1 = $y1, z0 = $z0, z1 = $z1)"
      case Pyramid(x0, y0, z0,
                   x1, y1, z1,
                   x2, y2, z2)                => s"Triangle (num of sides is more than 3)"
      case Sphere(x0, y0, z0, sideSize)       => s"Sphere is pretty round"
      //case Cube(x0, y0, z0, sideSize)         => s"Square (side length = $sideSize)"
      case _                                  => """¯\_(ツ)_/¯""" // looks scary enough in sbt shell
    }
}