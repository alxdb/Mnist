package adb.mnist.idx

import adb.mnist.R
import adb.mnist.idx.DataSet._
import adb.mnist.idx.DataType._
import adb.mnist.la.Vector

class ImageData(dataSet: DataSet) extends IdxData(dataSet, Image) {

	lazy val images: Seq[Array[Array[Int]]] =
		for (image <- 0 until dims.head) yield {
			val arr = Array.ofDim[Int](dims(1), dims(2))
			for (y <- 0 until dims(1); x <- 0 until dims(2)) {
				arr(y)(x) = rawData(x + y * dims(2) + image * dims(2) * dims(1))
			}
			arr
		}

	lazy val inputs: Seq[Vector] =
		for (image <- 0 until dims.head) yield {
			val inputLength: Int = dims.tail.product
			Vector(for (x <- 0 until inputLength) yield {
				rawData(x + image * inputLength) / implicitly[Fractional[R]].fromInt(255)
			})
		}

	val size: Int = dims.tail.product

}
