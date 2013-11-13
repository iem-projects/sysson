package at.iem.sysson

import scala.swing.event.WindowClosing
import scala.swing.{SimpleSwingApplication, MainFrame}
import processing.core.{PApplet, PConstants}
import PConstants._
import java.awt.event.{MouseEvent, ComponentEvent, ComponentAdapter}
import Implicits._
import de.sciss.numbers
import processing.pdf.PGraphicsPDF
import scala.annotation.tailrec

/*
requires the following additional jars from Processing:

itext.jar
pdf.jar

 */
object ForumImage extends SimpleSwingApplication {
  def timeIdx = 100

  lazy val data = openDefault2()

  lazy val top: MainFrame = {
    val v       = data.variableMap("ta")
    val numPlev = data.dimensionMap("plev").size
    val numLat  = data.dimensionMap("lat" ).size
    val numLon  = data.dimensionMap("lon" ).size

    // 0...16 ; 9 !
    val vec = (0 until numPlev).map { plev =>
      val sel = (v in "time" select timeIdx) in "plev" select plev
      // [plev][lat][lon]
      //sel.reducedDimensions.foreach(println)
      val dat = sel.readScaled1D()
      //    println(dat.size)
      val datn  = dat.normalize
      datn.grouped(numLat).toIndexedSeq
    }
    mkFrame(vec)
  }

  private def mkFrame(vec: Vec[Vec[Vec[Float]]]): MainFrame = new MainFrame {
    resizable = false
    pack()
    val embed = new NoiseSphere(vec)
    title = embed.getClass.getSimpleName
    peer.add(embed)
    embed.init()

    embed.addComponentListener(new ComponentAdapter {
      override def componentResized(e: ComponentEvent): Unit = {
        embed.removeComponentListener(this)
        // println(s"w ${embed.width}, h ${embed.height}; default? ${embed.defaultSize}")
        pack()
        centerOnScreen()
        embed.addComponentListener(this)
      }
    })

    listenTo(this)
    reactions += {
      case WindowClosing(_) =>
        try {
          embed.noLoop()
          embed.stop()
        } finally {
          sys.exit()
        }
    }
  }
}

class NoiseSphere(datAll: Vec[Vec[Vec[Float]]]) extends PApplet {
  val w       = 1600
  val h       = 800
  val dx      = 48 // 48
  val radio   = h / 4f
  // val pelos   = Array.tabulate(cuantos)(i => new Pelo((i.toFloat/(cuantos/11f))%1f, i.toFloat/(cuantos/13f)%1f))
  val pelos   = datAll.map { dat=>
    dat.zipWithIndex.flatMap { case (latSlice, lat) =>
      import numbers.Implicits._
      latSlice.zipWithIndex.map { case (v, lon) =>
        val z   = lat.linlin(0, dat     .size, 0, 1)
        val phi = lon.linlin(0, latSlice.size, 0, 1)
        val len = v  .linlin(0, 1.0f, 0.0f, 1.41f)
        new Pelo(z0 = z, phi0 = phi, len = len)
      }
    }
  }

  var rx      = 0f
  var ry      = 0f
  var plevIdx = 0
  var savePDF = false

  override def setup(): Unit = {
    size(w, h, OPENGL) // P3D
    noiseDetail(3)
    noLoop()
  }

  override def draw(): Unit = {
    if(savePDF) {
      // set up PGraphicsPDF for use with beginRaw()
      import de.sciss.file._

      @tailrec def mkFile(idx: Int = 1): File = {
        val f = userHome / "Desktop" / s"ForumImage$idx.pdf"
        if (f.exists()) mkFile(idx + 1) else f
      }

      val path  = mkFile().path
      println(s"Writing '$path'...")
      val pdf   = beginRaw(PDF, path).asInstanceOf[PGraphicsPDF]

      // set default Illustrator stroke styles and paint background rect.
      pdf.strokeJoin(MITER)
      pdf.strokeCap(SQUARE)
      pdf.fill(0)
      pdf.noStroke()
      pdf.rect(0,0, width,height)
    }

    background(0)
    val wh = width / 2f
    val hh = height / 2f
    pelos.zipWithIndex.collect { case (plevSlice, plev) if plev % 2 == 0 =>
      pushMatrix()
      translate(hh + (plev + 1) * dx, hh)

      val rxp = (mouseX - wh) * 0.005f
      val ryp = (mouseY - hh) * 0.005f
      rx      = (rx * 0.9f) + (rxp * 0.1f)
      ry      = (ry * 0.9f) + (ryp * 0.1f)
      rotateY(rx)
      rotateX(ry)
      fill(0)
      noStroke()
      sphere(radio)

      // pelos(plevIdx).foreach(_.dibujar())

      plevSlice.foreach(_.dibujar())
      // translate(100,0)
      popMatrix()
    }

    if (savePDF) {
      endRaw()
      savePDF = false
    }
  }

  //  override def mousePressed(): Unit = {
  //    plevIdx = (plevIdx + 1) % pelos.size
  //  }

  override def mouseMoved(e: MouseEvent): Unit = {
    super.mouseMoved(e)
    redraw()
  }

  override def keyPressed(): Unit =
    if (key == 's') {
      savePDF = true
      redraw()
    }

  class Pelo(z0: Float, phi0: Float, len: Float) {
    require(z0   >= 0f &&   z0 <= 1f, s"z0 is $z0")
    require(phi0 >= 0f && phi0 <= 1f, s"phi0 is $phi0")
    import math.{asin, cos, sin}

    val z1    = (z0 * 2 - 1) * radio // random(-radio, radio)
    val phi   = phi0 * TWO_PI // random(TWO_PI)
    // val largo = random(0.1f, 1.5f) // random(1.15f, 1.2f)
    val theta = asin(z1 / radio).toFloat

    def dibujar (): Unit = {
      val off     = 0f // (noise(millis() * 0.0005f, sin(phi).toFloat) - 0.5f) * 0.3f
      val offb    = 0f // (noise(millis() * 0.0007f, sin(z1).toFloat * 0.01f) - 0.5f) * 0.3f

      val thetaff = theta + off
      val phff    = phi   + offb
      val x       = (radio * cos(theta) * cos(phi)).toFloat
      val y       = (radio * cos(theta) * sin(phi)).toFloat
      val z       = (radio * sin(theta)           ).toFloat

      val xo      = (radio * cos(thetaff) * cos(phff)).toFloat
      val yo      = (radio * cos(thetaff) * sin(phff)).toFloat
      val zo      = (radio * sin(thetaff)            ).toFloat

      val xb      = xo * len
      val yb      = yo * len
      val zb      = zo * len

      beginShape(LINES)
      stroke(0)
      vertex(x, y, z)
      stroke(255, 150)
      vertex(xb, yb, zb)
      endShape()
    }
  }
}