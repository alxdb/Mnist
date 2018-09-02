package adb

import java.io.{InputStream, OutputStream}
import java.nio.ByteBuffer

import scala.util.Random

package object util {

	implicit class InputIntStream(stream: InputStream) {
		def readInt(): Int = {
			val bytes = new Array[Byte](4)
			if (stream.read(bytes) == -1) throw new RuntimeException("unexpected EOF")
			ByteBuffer.wrap(bytes).getInt
		}
	}

	implicit class OutputIntStream(stream: OutputStream) {
		def writeInt(x: Int): Unit = {
			val bytes = new Array[Byte](4)
			ByteBuffer.wrap(bytes).putInt(x)
			stream.write(bytes)
		}
	}

	implicit class InputDoubleStream(stream: InputStream) {
		def readDouble(): Double = {
			val bytes = new Array[Byte](8)
			if (stream.read(bytes) == -1) throw new RuntimeException("unexpected EOF")
			ByteBuffer.wrap(bytes).getDouble
		}
	}

	implicit class OutputDoubleStream(stream: OutputStream) {
		def writeDouble(x: Double): Unit = {
			val bytes = new Array[Byte](8)
			ByteBuffer.wrap(bytes).putDouble(x)
			stream.write(bytes)
		}
	}

	trait GenRandom[T] {
		def next(): T
	}

	object GenRandom {
		def instance[T](a: => T): GenRandom[T] = () => a
		implicit val doubleRandom: GenRandom[Double] = instance(Random.nextDouble())
		implicit val floatRandom: GenRandom[Float] = instance(Random.nextFloat())
	}

}
