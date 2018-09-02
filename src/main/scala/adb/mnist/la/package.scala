package adb.mnist

package object la {
	case class MatrixData(_data: Array[Array[R]]) {
		for (row <- _data.tail) {
			assert(row.length == _data.head.length)
		}
	}

	def transposeProduct(a: Vector, b: Vector): Matrix = {
		val mat = new Matrix(a.length, b.length)
		for (row <- 0 until mat.rows) {
			for (col <- 0 until mat.cols) {
				mat(row)(col) = a(row) * b(col)
			}
		}
		mat
	}
}
