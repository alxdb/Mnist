package adb.mnist.idx

import adb.mnist.R
import adb.mnist.idx.DataSet._
import adb.mnist.idx.DataType._
import adb.mnist.la.Vector

class LabelData(dataSet: DataSet) extends IdxData(dataSet, Label) {
	val labelOptions: Int = 10

	lazy val labels: Seq[Int] =
		for (label <- 0 until dims.head) yield {
			rawData(label)
		}

	lazy val outputs: Seq[Vector] =
		for (label: Int <- 0 until dims.head) yield {
			Vector(for (i <- 0 until labelOptions) yield if (i == rawData(label)) {
				implicitly[Fractional[R]].one
			} else {
				implicitly[Fractional[R]].zero
			})
		}
}
