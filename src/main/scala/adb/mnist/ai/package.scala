package adb.mnist

package object ai {
	@SerialVersionUID(1L)
	case class NetworkData(weights: Array[Array[Array[R]]], biases: Array[Array[R]]) extends Serializable

	def sigmoid(z: R): R = 1 / (1 + math.exp(-z))
	def dSigmoid(z: R): R = sigmoid(z) * (1 - sigmoid(z))
}
