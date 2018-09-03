package adb.mnist.idx

import adb.mnist.idx.DataSet._
import adb.mnist.idx.DataType._

abstract class IdxData(dataSel: (DataSet, DataType)) {
	protected val (dims, rawData): (Seq[Int], Seq[Int]) = dataSel match {
		case (Test, Image) => getRawData("/test-images.idx")
		case (Test, Label) => getRawData("/test-labels.idx")
		case (Train, Image) => getRawData("/train-images.idx")
		case (Train, Label) => getRawData("/train-labels.idx")
		case _ => (Seq(), Seq()) // this should never happen, cannot infer completeness from Enumeration
	}

	def getRawData(file: String): (Seq[Int], Seq[Int]) = {
		val stream = getClass.getResourceAsStream(file)
    if (stream == null) throw new RuntimeException("file not found")
		if (stream.read() != 0 || stream.read() != 0) throw new RuntimeException("Invalid Idx Format")
		if (stream.read() != 8) throw new RuntimeException("Only unsigned bytes supported")
		val nDims = stream.read()
		val dims = Seq.fill(nDims)(stream.readInt())
		val data = Vector.fill(dims.product)(stream.read())
		(dims, data)
	}

	val length: Int = dims.head

}
