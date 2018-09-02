package adb.mnist.la

import adb.mnist.R

import scala.collection.mutable

class Vector(length: Int) extends mutable.ArraySeq[R](length) {

	def +(other: Vector): Vector = {assert(length == other.length); Vector(this.zip(other).map(a => a._1 + a._2))}
	def *(other: Vector): Vector = {assert(length == other.length); Vector(this.zip(other).map(a => a._1 * a._2))}
	def -(other: Vector): Vector = {assert(length == other.length); Vector(this.zip(other).map(a => a._1 - a._2))}
	def /(other: Vector): Vector = {assert(length == other.length); Vector(this.zip(other).map(a => a._1 / a._2))}
	def *(other: R): Vector = Vector(this.map(a => a * other))
	def /(other: R): Vector = Vector(this.map(a => a / other))
	def dot(other: Vector): R = {assert(length == other.length); (this * other).sum}

}

object Vector {
	def apply(seq: Seq[R]): Vector = {
		val vec = new Vector(seq.length)
		for (i <- seq.indices) vec(i) = seq(i)
		vec
	}
}
