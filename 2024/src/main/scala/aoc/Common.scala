package aoc

import scala.collection.mutable
import scala.collection.SeqOps
import scala.annotation.nowarn
import zio.prelude.ZSet
import aoc.Common.toIntSafe

object Common:
    def timed[A](label: String, f: => A): Unit =
        val start = System.currentTimeMillis()
        val res   = f
        val end   = System.currentTimeMillis()

        println(s"$label answer - $res It took ${end - start}ms")

    extension [A, CC[_], C](collection: SeqOps[A, CC, C]) def middle: A = collection(collection.size / 2)

    extension [A](q: mutable.PriorityQueue[A])
        @nowarn("msg=discarded non-Unit value of type A")
        def enqueueAndKeepMaxSize(element: A, maxSize: Int): mutable.PriorityQueue[A] =
            q.enqueue(element)
            if q.size > maxSize then q.dequeue()

            q

    extension (num: Long)
        def toIntSafe: Int =
            if num > Int.MaxValue then throw IllegalArgumentException(s"Number $num is too large to convert to Int")
            else num.toInt

type MultiSetLong[A] = ZSet[A, Long]
object MultiSetLong:
    def apply[A](elements: A*): MultiSetLong[A] =
        fromIterable(elements)

    def fromIterable[A](elements: Iterable[A]): MultiSetLong[A] =
        ZSet.fromMap(
          elements
              .foldLeft(Map.empty[A, Long]) { (map, a) =>
                  map + (a -> map.get(a).fold(1L)(_ + 1))
              }
        )

    def fromMap[A](map: Map[A, Long]): MultiSetLong[A] =
        ZSet.fromMap(map)

    def empty[A]: MultiSetLong[A] = ZSet.empty

enum Direction:
    case Up, Down, Left, Right

case class Point(x: Long, y: Long):
    def up: Point    = Point(x, y - 1)
    def down: Point  = Point(x, y + 1)
    def left: Point  = Point(x - 1, y)
    def right: Point = Point(x + 1, y)

    def move(direction: Direction): Point = direction match
        case Direction.Up    => up
        case Direction.Down  => down
        case Direction.Left  => left
        case Direction.Right => right

    def topLeft: Point     = Point(x - 1, y - 1)
    def topRight: Point    = Point(x + 1, y - 1)
    def bottomLeft: Point  = Point(x - 1, y + 1)
    def bottomRight: Point = Point(x + 1, y + 1)

    def adjacent = List(up, down, left, right)
    def adjacentDiagonal = for
        x <- List(-1, 0, 1)
        y <- List(-1, 0, 1) if x != 0 || y != 0
    yield Point(this.x + x, this.y + y)

    def *(n: Long): Point = Point(x * n, y * n)
    def +(n: Long): Point = Point(x + n, y + n)

    def *(p: Point): Point = Point(x * p.x, y * p.y)
    def +(p: Point): Point = Point(x + p.x, y + p.y)

    def mod(xMod: Long, yMod: Long): Point = Point(x % xMod, y % yMod)

    def inBounds(map: Map2d[?]): Boolean =
        x >= 0 && x <= map.maxX && y >= 0 && y <= map.maxY

    def inBounds(map: Map2DVec[?]): Boolean =
        x >= 0 && x <= map.maxX && y >= 0 && y <= map.maxY

    def manhattanDistance(other: Point): Long =
        math.abs(x - other.x) + math.abs(y - other.y)

case class Map2DVec[V](underlying: Vector[Vector[V]]):
    val maxX = underlying.size - 1
    val maxY = underlying.head.size - 1

    def apply(x: Int): Vector[V] = underlying(x)
    def apply(x: Int, y: Int): V = underlying(x)(y)
    def apply(point: Point): V   = underlying(point.x.toIntSafe)(point.y.toIntSafe)

    def transpose: Map2DVec[V] = Map2DVec(underlying.transpose)

    def mapCols[V2](f: Vector[V] => Vector[V2]): Map2DVec[V2] = Map2DVec(underlying.map(f))

    def mapRows[V2](f: Vector[V] => Vector[V2]): Map2DVec[V2] = Map2DVec(underlying.transpose.map(f).transpose)

    override def toString: String =
        underlying.transpose.map(_.mkString).mkString("\n")

object Map2DVec:
    def fromLines(lines: List[String]): Map2DVec[Char] =
        val underlying = lines.map(_.toVector).toVector.transpose
        Map2DVec(underlying)

case class Map2d[V](underlying: Map[Point, V]):
    lazy val maxX = underlying.keys.maxBy(_.x).x
    lazy val minX = underlying.keys.minBy(_.x).x
    lazy val maxY = underlying.keys.maxBy(_.y).y
    lazy val minY = underlying.keys.minBy(_.y).y

    def map[V2](f: ((Point, V)) => (Point, V2)): Map2d[V2] = Map2d(underlying.map(f))

    def apply(k: Point): V = underlying(k)

    def get(k: Point): Option[V] = underlying.get(k)

    def updated(k: Point, v: V): Map2d[V] = Map2d(underlying.updated(k, v))

    override def toString: String =
        (for
            y <- minY to maxY
            x <- minX to maxX
        yield
            val point = Point(x, y)
            if underlying.contains(point) then underlying(point)
            else '.'
        ).grouped(((maxX - minX) + 1).toIntSafe).map(_.mkString).mkString("\n")

object Map2d:
    def fromLines(lines: List[String]): Map2d[Char] =
        val underlying = lines.zipWithIndex.flatMap { (line, y) =>
            line.zipWithIndex.map { (char, x) =>
                Point(x, y) -> char
            }
        }.toMap

        Map2d(underlying)
