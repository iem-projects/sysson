package at.iem.sysson
package util

import ucar.nc2
import java.io.{OutputStreamWriter, FileOutputStream, File}
import collection.immutable.{IndexedSeq => IIdxSeq}

object Export {
  /**
   * Outputs for each variable:
   * :var variable-name :units units :dim dimensions
   * Followed by a break down of the matrix into vectors in the form of
   * [ :sel dim-name dim-index ]*N :val values
   */
  def netcdfToCSV(out: File, in: nc2.NetcdfFile, delimiter: Char = ',') {
    import Implicits._

    val del = delimiter.toString
    var varsDone = Set.empty[String]
    val fos = new FileOutputStream(out)
    val w   = new OutputStreamWriter(fos, "UTF-8")

    implicit class RichString(s: String) {
      def escape = {
        val s1 = s .replaceAll("\"", "\\\"")
        val s2 = s1.replaceAll("\t", "\\t")
        val s3 = s2.replaceAll("\n", "\\n")
        s3
      }
    }

    def loop(v: nc2.Variable) {
      // first traverse dimensions on which this variable depends
      val dimensions  = v.dimensions.map(_.name)
      varsDone += v.name
      val varsToDo    = dimensions.filterNot(varsDone.contains)
      varsToDo.foreach { name =>
        in.variableMap.get(name).foreach(loop)
      }

      val units   = v.units.map(str => del + ":units" + del + "\"" + str.escape + "\"").getOrElse("")
      val dimStr  = dimensions.map(str => "\"" + str.escape + "\"").mkString(del)
      w.write(s""":var$del"${v.name.escape}"${units}$del:dim${del}$dimStr\n""")

      def iter(sec: VariableSection, prefix: String, dims: IIdxSeq[nc2.Dimension], shape: IIdxSeq[Int]) {
//        val rank      = sec.reducedRank
//        if (rank <= 1) { }
        if (dims.size <= 1) {
          val arr   = sec.read()
          val data  = if (arr.isFloat) arr.float1D else arr.double1D
          val dataS = data.mkString(del)
          w.write(s"$prefix:val${del}$dataS\n")
        } else {
          val dimName   = dims.head.name
          val dimNameS  = "\"" + dimName.escape + "\""
          val num       = shape.head
          var i = 0
          while (i < num) {
            iter(sec in dimName select i, s"${prefix}${dimNameS}${del}${i}$del", dims.tail, shape.tail)
          i += 1 }
        }
      }

      val dimsR   = v.reducedDimensions
      val shapeR  = v.reducedShape
      val mxIdx   = if (dimsR.nonEmpty) shapeR.indexOf(shapeR.max) else -1
      // move the largest dimension to the end to produce vectors as long as possible
      val dimsRR  = if (mxIdx < 0) dimsR  else dimsR .patch(mxIdx, Vector.empty, 1) :+ dimsR(mxIdx)
      val shapeRR = if (mxIdx < 0) shapeR else shapeR.patch(mxIdx, Vector.empty, 1) :+ shapeR(mxIdx)

      iter(v.selectAll, if (dimsRR.size <= 1) "" else s":sel$del", dimsRR, shapeRR)
      w.write("\n")
    }

    try {
      in.variables.foreach(loop)
    } finally {
      w.close()
    }
  }
}