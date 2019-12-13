package sumidiot.golf

import cats._
import cats.implicits._
import scala.util.Random

object Comonad extends App {

  case class PointedGrid[T](
    pt: (Int, Int),
    grid: Vector[Vector[T]]
  )

  object PointedGrid {
    def refocus[T](pg: PointedGrid[T])(p: (Int, Int)): PointedGrid[T] =
      pg.copy(pt = p)
  }

  implicit val pointedGridIsComonad: Comonad[PointedGrid] =
    new Comonad[PointedGrid] {
      override def map[A, B](pg: PointedGrid[A])(f: A => B): PointedGrid[B] =
        pg.copy(grid = pg.grid.map(_.map(f)))

      override def extract[T](pg: PointedGrid[T]): T = {
        val (r, c) = pg.pt
        pg.grid(Math.floorMod(r, pg.grid.size))(Math.floorMod(c, pg.grid(0).size))
      }

      override def coflatMap[A, B](pg: PointedGrid[A])(f: PointedGrid[A] => B): PointedGrid[B] =
        PointedGrid[B](
          pg.pt,
          // i need a Vector[Vector[B]]
          // i can make a bunch of pointedgrids by moving the pt around in the pg
          // and applying f to each
          Vector.tabulate(pg.grid.size, pg.grid(0).size) { case pt =>
            f(PointedGrid.refocus(pg)(pt))
          }
        )
    }
  
  trait Neighbors[F[_]] {
    def neighbors[T](f: F[T]): Iterable[F[T]]
  }

  implicit val PointedGridHasNeighbors: Neighbors[PointedGrid] =
    new Neighbors[PointedGrid] {
    
      override def neighbors[T](pg: PointedGrid[T]): List[PointedGrid[T]] =
        nbrCoords(pg.pt).map(PointedGrid.refocus(pg))

      def nbrCoords(x: Int, y: Int): List[(Int, Int)] =
        List(
          (x - 1, y - 1),
          (x    , y - 1),
          (x + 1, y - 1),
          (x - 1, y    ),
          (x + 1, y    ),
          (x - 1, y + 1),
          (x    , y + 1),
          (x + 1, y + 1)
          )
    
      def nbrCoords(xy: (Int, Int)): List[(Int, Int)] =
        nbrCoords(xy._1, xy._2)

    }
  
  
  type LifeGrid = Vector[Vector[Boolean]]

  def lifeStep(grid: LifeGrid): LifeGrid =
    PointedGrid((0, 0), grid).coflatMap(lifeStep[PointedGrid]).grid

  def alive(aliveAlready: Boolean, aliveNeighbors: Int): Boolean =
    if (aliveAlready) {
      (aliveNeighbors == 2) || (aliveNeighbors == 3)
    } else {
      aliveNeighbors == 3
    }


  def lifeStep[F[_]](pg: F[Boolean])(implicit cev: Comonad[F], nev: Neighbors[F]): Boolean =
    alive(cev.extract(pg), nev.neighbors(pg).count(_.extract))


  def showLife(lg: LifeGrid): Unit =
    for {
      row <- lg
    } {
      println(row.map(b => if (b) "X" else ".").mkString)
    }

  def randomLife(rows: Int, cols: Int): LifeGrid =
    Vector.tabulate(rows, cols) { case _ =>
      Random.nextBoolean
    }

  val blinker: LifeGrid =
    Vector(
      Vector(false, false, false, false, false),
      Vector(false, false, true, false, false),
      Vector(false, false, true, false, false),
      Vector(false, false, true, false, false),
      Vector(false, false, false, false, false)
      )

  val toad: LifeGrid =
    Vector(
      Vector(false, false, false, false, false, false),
      Vector(false, false, false, false, false, false),
      Vector(false, false, true, true, true, false),
      Vector(false, true, true, true, false, false),
      Vector(false, false, false, false, false, false),
      Vector(false, false, false, false, false, false)
      )

  val loaf: LifeGrid =
    Vector(
      Vector(false, false, false, false, false, false),
      Vector(false, false, true, true, false, false),
      Vector(false, true, false, false, true, false),
      Vector(false, false, true, false, true, false),
      Vector(false, false, false, true, false, false),
      Vector(false, false, false, false, false, false)
      )

  showLife(blinker)
  println("")
  showLife(lifeStep(blinker))
  println("")
  showLife(lifeStep(lifeStep(blinker)))
  
  showLife(toad)
  println("")
  showLife(lifeStep(toad))
  println("")
  showLife(lifeStep(lifeStep(toad)))

  showLife(loaf)
  println("")
  showLife(lifeStep(loaf))
  println("")
  showLife(lifeStep(lifeStep(loaf)))

}
