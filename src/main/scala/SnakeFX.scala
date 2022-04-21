import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.effect.DropShadow
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color._
import scalafx.scene.paint._
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text

import scala.util.Random

object SnakeFX extends JFXApp3 {

  val initialSnake: List[(Double,Double)] = List(
    (250,200),
    (225,200),
    (200,200)
  )

  case class State(snake: List[(Double,Double)],food: (Double, Double)){
    def newState(dir: Int): State = {
      val (x,y) = snake.head
      val (newx,newy) = dir match {
        case 1 => (x, y - 25)
        case 2 => (x, y + 25)
        case 3 => (x - 25, y)
        case 4 => (x + 25, y)
        case _ => (x, y)
      }
      val newSnake: List[(Double,Double)] = {
        if(newx < 0 || newx >= 600 || newy < 0 || newy >= 600 || snake.tail.contains(newx,newy))
          initialSnake
        else if (food == (newx,newy))
          food :: snake
        else
          (newx,newy) :: snake.init
      }

      val newFood = {
        if (food == (newx,newy))
          randomFood()
        else
          food
      }

      State(newSnake,newFood)
    }
  }

  def randomFood() : (Double,Double) = {
    (Random.nextInt(24) * 25, Random.nextInt(24) * 25)
  }

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage{
      width = 600
      height = 600
      scene = new Scene {
        fill = White
        content = new Rectangle {
          x = 200
          y = 200
          width = 25
          height = 25
          fill = Green
        }
      }
    }
  }

}
