import java.awt.Color
import scala.actors.Scheduler
import scala.swing._
import scala.util.Random

import processing.core._

import Utility._

object MonkeyPaint extends SimpleSwingApplication {
  class MonkeyApplet extends PApplet {
    val rng = RandomSeedField.text match {
      case Int(i) => new Random(i)
      case _ => new Random()
    }

    val anneal = new Anneal {
      val generator = rng
      val threshold = 10000
    }

    val displayStatus: Boolean = StatusCheckbox.selected
    var done = false

    // Processing has to wait until setup() to create images
    var painting: PImage = null

    override def setup() {
      frame.setTitle(InputImageField.text)

      val original = loadImage(InputImageField.text)
      val (scaleWidth, scaleHeight) = 
        scaleDims(original.width, original.height, InputWidthField.text,
                  InputHeightField.text)
      original.resize(scaleWidth,scaleHeight)
      original.loadPixels
      size(scaleWidth,scaleHeight)

      painting = createImage(width, height, PConstants.RGB)
      painting.loadPixels
      val white: Int = RGBColor(255, 255, 255)
      (0 until width*height).foreach(painting.pixels(_) = white)
      painting.updatePixels

      val font = loadFont("DejaVuSansCondensed-14.vlw")
      textFont(font, 14)
      textAlign(PConstants.RIGHT)

      val brush: Brush = {
        val fields = Brushes.pages(Brushes.selection.index).content match {
          case p: Panel => (for (component <- p.contents)
                            yield component match {
                              case tf: TextField => Some(tf.text)
                              case _ => None
                            }).flatten
          case _ => Seq()
        }
        Brushes.selection.index match {
          case 0 => new RandomWalk(rng, Point(width, height),
                                   intOrElse(fields(0), 200),
                                   intOrElse(fields(1), 1))
          case 1 => new SquareBrush(rng, Point(width, height),
                                    intOrElse(fields(0), 14))
          case 2 => new IsocelesBrush(rng, Point(width, height),
                                      intOrElse(fields(0), 8),
                                      intOrElse(fields(1), 16))

          case _ => new RandomWalk(rng, Point(width, height), 200, 1)
        }
      }

      val maxIterations = intOrElse(MaxIterField.text, -1)
      Scheduler.execute(new Runnable {
        def run() {
          while (maxIterations <= 0 || anneal.iterations < maxIterations) {
            val (color, points) = brush.stroke
            anneal.step(points.map {
              (p) => RGBColor.distance(RGBColor(original.pixels(p)), color) -
                     RGBColor.distance(RGBColor(original.pixels(p)),
                                       RGBColor(painting.pixels(p)))
            }.sum) {
              points.foreach((p) => painting.pixels(p) = color)
              painting.updatePixels
            }
          }
          done = true
        }})
    }

    override def draw() {
      // TODO: Add output to disk capability and make output to window optional
      image(painting, 0, 0)
      if (displayStatus) {
        val str = anneal.toString
        fill(0, 0, 0)
        rect(width-15-textWidth(str), height-25, textWidth(str)+10, 20)
        fill(255, 255, 255)
        text(str, width-10, height-10)
      }
      if (done)
        noLoop()
    }

    override def keyPressed() = if (key == PConstants.ESC) { key = 0 }
  }

/*

Dialog box components and other GUI stuff below!

*/

  val InputImageField = new TextField("", 50)
  val OpenInputButton = new Button {
    val chooser = new FileChooser
    action = Action("Open...") {
      chooser.showOpenDialog(null) match {
        case FileChooser.Result.Approve => {
          InputImageField.text = chooser.selectedFile.getPath
        }
        case _ =>
      }
    }
  }
  val InputWidthField = new TextField("", 4)
  val InputHeightField = new TextField("", 4)

  val RandomSeedField = new TextField("", 5)
  val MaxIterField = new TextField("", 6)

  val StatusCheckbox = new CheckBox("Status Display")
  StatusCheckbox.selected = true

  val BrushOptions = List(
    ("Random Walk", new FlowPanel(
      new Label("Steps"), new TextField("200", 4),
      new Label("Thickness"),new TextField("1", 2))),
    ("Square", new FlowPanel(
      new Label("Width"), new TextField("14", 3))),
    ("Triangle", new FlowPanel(
      new Label("Base"), new TextField("8", 3),
      new Label("Height"), new TextField("16", 3)))
    //,("Stamp",new FlowPanel())
  )
  val Brushes = new TabbedPane() {
    border = Swing.TitledBorder(Swing.LineBorder(Color.black), "Brush Style")
    BrushOptions.foreach((x) => this.pages += new TabbedPane.Page(x._1, x._2))
  }
  
  val StartButton = new Button {
    action = Action("Start!") {
      Scheduler.execute(new Runnable {
        def run() {
          PApplet.main(Array[String]("MonkeyPaint$MonkeyApplet"))
        }})
      top.visible = false
    }
  }

  val top: MainFrame = new MainFrame {
    title = "Monkey Paint"
    contents = new BorderPanel {
      import BorderPanel._
      add(new FlowPanel(
        OpenInputButton, InputImageField,
        new Label("Width"), InputWidthField,
        new Label("Height"), InputHeightField) {
          border = Swing.TitledBorder(Swing.LineBorder(Color.black),
                                      "Input Image")
        }, Position.North)

      add(new GridPanel(3, 2) {
        contents.append(new Label("Random Seed"), RandomSeedField,
                        new Label("Max iterations"), MaxIterField,
                        StatusCheckbox)
        border = Swing.TitledBorder(Swing.LineBorder(Color.black),
                                    "Miscellaneous")
      }, Position.West)

      add(Brushes, Position.Center)

      add(new BorderPanel {
        add(new FlowPanel(StartButton), Position.South)
        border = Swing.EmptyBorder
      }, Position.East)
    }
  }
}
