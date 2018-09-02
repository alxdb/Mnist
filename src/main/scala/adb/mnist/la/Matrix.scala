package adb.mnist.la

import adb.mnist.R

import scala.collection.immutable.VectorBuilder

class Matrix private (_data: Seq[Vector]) {

	private val data = new VectorBuilder[Vector]().++=(_data).result()
	def toArray: Array[Array[R]] = (for (row <- data) yield row.toArray).toArray
	def rows: Int = _data.length
	def cols: Int = _data.head.length

	def this(rows: Int, cols: Int) = this(for (_ <- 0 until rows) yield new Vector(cols))
	def this(_data: MatrixData) = this(for (row <- _data._data) yield Vector(row))

	def apply(row: Int): Vector = data(row)
	def update(other: Matrix): Unit = {
		assert(rows == other.rows)
		assert(cols == other.cols)
		for ((row, otherRow) <- data.zip(other.data)) {
			for (i <- row.indices) {
				row(i) = otherRow(i)
			}
		}
	}

	def *(other: Vector): Vector = Vector(for (row <- data) yield row dot other)
	def +(other: Matrix): Matrix = {assert(rows == other.rows); new Matrix(for ((row, otherRow) <- data.zip(other.data)) yield row + otherRow)}
	def -(other: Matrix): Matrix = {assert(rows == other.rows); new Matrix(for ((row, otherRow) <- data.zip(other.data)) yield row - otherRow)}
	def /(other: R): Matrix = new Matrix(for (row <- data) yield row / other)
	def *(other: R): Matrix = new Matrix(for (row <- data) yield row * other)
	def t(): Matrix = new Matrix(for (i <- 0 until cols) yield Vector(for (row <- data) yield row(i)))

}
