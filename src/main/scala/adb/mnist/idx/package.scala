package adb.mnist

import java.io.InputStream
import java.nio.ByteBuffer

package object idx {

	implicit class InputIntStream(stream: InputStream) {
		def readInt(): Int = {
			val bytes = new Array[Byte](4)
			if (stream.read(bytes) == -1) throw new RuntimeException("unexpected EOF")
			ByteBuffer.wrap(bytes).getInt
		}
	}

	object DataSet extends Enumeration {
		type DataSet = Value
		val Train, Test = Value
	}

	object DataType extends Enumeration {
		type DataType = Value
		val Image, Label = Value
	}
}
