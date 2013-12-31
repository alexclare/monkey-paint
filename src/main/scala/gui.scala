import scala.concurrent._
import scala.swing._
import scala.util.Random

import java.awt.Color
import java.io.File
import java.util.concurrent.Executors

import processing.core._

import Utility._

class FileOpenButton(dumpField: TextField,
                     directory: Boolean) extends Button {
  val chooser = new FileChooser {
    fileSelectionMode =
      if (directory) FileChooser.SelectionMode.DirectoriesOnly
      else FileChooser.SelectionMode.FilesOnly
  }
  action = Action("Open...") {
    chooser.showOpenDialog(null) match {
      case FileChooser.Result.Approve => {
        dumpField.text = chooser.selectedFile.getPath + (
          if (directory) File.separator else "")
      }
      case _ =>
    }
  }
}

object MonkeyPaint extends SimpleSwingApplication {
  implicit val cxt = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

  class MonkeyApplet extends PApplet {
    implicit val me: PApplet = this

    val rng = {
      val seed: Option[Int] = optional(RandomSeedField.text) 
      seed match {
        case Some(i) => new Random(i)
        case None    => new Random()
      }
    }

    val anneal = new Anneal {
      val generator = rng
      val threshold = 10000
    }

    val displayStatus: Boolean = StatusCheckbox.selected
    val displayImage: Boolean = DisplayCheckbox.selected
    var done = false

    val painting = createGraphics(140, 140, PConstants.P3D)

    override def setup() {
      frame.setTitle(InputImageField.text)

      val original = loadAndScale(
        InputImageField.text, InputWidthField.text, InputHeightField.text)
      original.loadPixels

      if (displayImage)
        size(original.width, original.height)
      else {
        // Just a bit larger than Processing's minimum window size
        size(140, 140)
        background(255, 255, 255)
      }

      painting.setSize(original.width, original.height)
      painting.beginDraw()
      painting.loadPixels()
      val white: Int = RGBColor(255, 255, 255)
      for (i <- 0 until original.width * original.height) {
        painting.pixels(i) = white
      }
      painting.updatePixels()
      painting.endDraw()

      val font = loadFont("DejaVuSansCondensed-14.vlw")
      textFont(font, 14)
      textAlign(PConstants.RIGHT)

      val brush: Brush = {
        val fields = Brushes.pages(Brushes.selection.index).content match {
          case p: Panel => (for (component <- p.contents)
                            yield component match {
                              case cb: CheckBox => if (cb.selected)
                                Some("true") else Some("false")
                              case tf: TextField => Some(tf.text)
                              case _ => None
                            }).flatten
          case _ => Seq()
        }
        Brushes.selection.index match {
          case 0 => new RandomWalk(
            rng, Point(original.width, original.height),
            orElse(fields(0), 200), orElse(fields(1), 1))
          case 1 => new SquareBrush(
            rng, Point(original.width, original.height),
            orElse(fields(0), 14))
          case 2 => new IsocelesBrush(
            rng, Point(original.width, original.height),
            orElse(fields(0), 8), orElse(fields(1), 16))
          case 3 => {
            val img = loadAndScale(fields(0), fields(1), fields(2))
            new StampBrush(
              rng, Point(original.width, original.height),
              img, orElse(fields(3), 0.5),
              if (fields(4) == "true") true else false)
          }
          case _ => new RandomWalk(
            rng, Point(original.width, original.height), 200, 1)
        }
      }

      val maxIterations = orElse(MaxIterField.text, -1)
      val outputPath: String = OutputDirField.text
      val outputInterval = orElse(OutputIntervalField.text, -1)
      future {
        while (maxIterations <= 0 || anneal.iterations < maxIterations) {
          val (color, points) = brush.stroke
          anneal.step(points.map {
            (p) => RGBColor.distance(RGBColor(original.pixels(p)), color) -
            RGBColor.distance(RGBColor(original.pixels(p)),
                              RGBColor(painting.pixels(p)))
          }.sum) {
            painting.beginDraw()
            points.foreach((p) => painting.pixels(p) = color)
            painting.updatePixels()
            painting.endDraw()
          }

          if (outputInterval > 0 && anneal.iterations % outputInterval == 0) {
            val img = createGraphics(
              painting.width, painting.height, PConstants.P3D)
            img.beginDraw()
            img.copy(painting, 0, 0, painting.width, painting.height,
                     0, 0, img.width, img.height)
            img.endDraw()
            future {
              img.save(outputPath + anneal.iterations + ".png")
            }
          }
        }
        done = true
      }
    }

    override def draw() {
      if (displayImage)
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
  val InputWidthField = new TextField("", 4)
  val InputHeightField = new TextField("", 4)

  val RandomSeedField = new TextField("", 5)
  val MaxIterField = new TextField("", 6)

  val StatusCheckbox = new CheckBox("Status Display")
  StatusCheckbox.selected = true

  val OutputDirField = new TextField("", 40)
  val OutputIntervalField = new TextField("", 6)
  val DisplayCheckbox = new CheckBox("Display Image")
  DisplayCheckbox.selected = true

  val BrushOptions = List(
    ("Random Walk", new FlowPanel(
      new Label("Steps"), new TextField("200", 4),
      new Label("Thickness"),new TextField("1", 2))),
    ("Square", new FlowPanel(
      new Label("Width"), new TextField("14", 3))),
    ("Triangle", new FlowPanel(
      new Label("Base"), new TextField("8", 3),
      new Label("Height"), new TextField("16", 3))),
    ("Stamp", new FlowPanel {
      val FileField = new TextField("", 20)
      contents.append(
        new FileOpenButton(FileField, false), FileField,
        new Label("Width"), new TextField("", 3),
        new Label("Height"), new TextField("", 3),
        new Label("Threshold"), new TextField("0.5", 3),
        new CheckBox("Invert"))
    }))
  val Brushes = new TabbedPane() {
    border = Swing.TitledBorder(Swing.LineBorder(Color.black), "Brush Style")
    BrushOptions.foreach((x) => this.pages += new TabbedPane.Page(x._1, x._2))
  }
  
  val StartButton = new Button {
    action = Action("Start!") {
      future {
        PApplet.main(Array[String]("MonkeyPaint$MonkeyApplet"))
      }
      top.visible = false
    }
  }

  val top: MainFrame = new MainFrame {
    title = "Monkey Paint"
    contents = new BorderPanel {
      import BorderPanel._
      add(new FlowPanel(
        new FileOpenButton(InputImageField, false), InputImageField, 
        new Label("Width"), InputWidthField,
        new Label("Height"), InputHeightField) {
          border = Swing.TitledBorder(
            Swing.LineBorder(Color.black),"Input Image")
        }, Position.North)

      add(new GridPanel(3, 2) {
        contents.append(new Label("Random Seed"), RandomSeedField,
                        new Label("Max iterations"), MaxIterField,
                        StatusCheckbox)
        border = Swing.TitledBorder(
          Swing.LineBorder(Color.black), "Miscellaneous")
      }, Position.West)

      add(new FlowPanel(
        new Label("Prefix"),
        new FileOpenButton(OutputDirField, true), OutputDirField,
        new Label("Interval"), OutputIntervalField,
        DisplayCheckbox) {
        border = Swing.TitledBorder(
          Swing.LineBorder(Color.black), "Output")
      }, Position.South)

      add(Brushes, Position.Center)

      add(new BorderPanel {
        add(new FlowPanel(StartButton), Position.South)
        border = Swing.EmptyBorder
      }, Position.East)
    }
  }
}
