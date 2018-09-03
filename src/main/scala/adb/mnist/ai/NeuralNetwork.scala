package adb.mnist.ai

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import adb.mnist.R
import adb.mnist.la._

import scala.util.Random

class NeuralNetwork private (_data: NetworkData) {
	private val weights: Seq[Matrix] = for (w <- _data.weights) yield new Matrix(MatrixData(w))
	private val biases: Seq[Vector] = for (b <- _data.biases) yield Vector(b)

	def this(filename: String) = this {
			val ois = new ObjectInputStream(new FileInputStream(filename))
			val data = ois.readObject().asInstanceOf[NetworkData]
			ois.close()
			data
	}

	def this(sizes: Seq[Int]) = this {
		val weights: Array[Array[Array[R]]] = (for (s <- sizes.zip(sizes.tail)) yield {
			Array.fill(s._2, s._1)((Random.nextDouble() - 0.5) * 2)
		}).toArray
		val biases: Array[Array[R]] = (for (s <- sizes.tail) yield {
			Array.fill(s)((Random.nextDouble() - 0.5) * 2)
		}).toArray
		NetworkData(weights, biases)
	}

	def save(filename: String): Unit = {
		val oos = new ObjectOutputStream(new FileOutputStream(filename))
		oos.writeObject(NetworkData(weights.map(_.toArray).toArray, biases.map(_.toArray).toArray))
		oos.close()
	}

	def result(input: Vector): Vector = {
		weights.zip(biases).foldLeft(input){ case (i, (w, b)) => Vector((w * i + b).map(sigmoid)) }
	}

	def cost(result: Vector, expected: Vector): R = {
		(result - expected).map(x => x * x).sum * 0.5
	}

	def avgCost(ioPairs: Seq[(Vector, Vector)]): R = {
		ioPairs.map(x => cost(result(x._1), x._2)).sum / ioPairs.length
	}

	def backProp(input: Vector, output: Vector): (Seq[Matrix], Seq[Vector]) = {
		var activations = Seq[Vector]()
		var dActivations = Seq[Vector]()
		// FeedForward
		// Add act and d_act to lists such that, finally, the first element is the act of the last layer
		// i.e moving forward through the list moves backwards through the network
		val result = weights.zip(biases).foldLeft(input) {
			case (i, (w, b)) =>
				val z = w * i + b
				activations = Vector(z.map(sigmoid)) +: activations
				dActivations = Vector(z.map(dSigmoid)) +: dActivations
				Vector(activations.head)
		}

		// FeedBackward
		// after computing the output error, construct the layer deltas, such that the first element in the list
		// is the final output layer and so on
		val layerDeltas = weights.reverse.zip(dActivations.tail).scanLeft((result - output) * dActivations.head) {
			case (d, (w, z)) =>
				(w.t * d) * z
		}

		// Weight Delta
		// the activations and layer deltas are both backwards. each layer delta should be transpose multiplied with
		// the activations from the previous layer, hence the tail
		val weightDeltas = (activations.tail :+ input).zip(layerDeltas).map {
			case (a, d) =>
				transposeProduct(d, a)
		}

		(weightDeltas.reverse, layerDeltas.reverse)
	}

	def update(dWeights: Seq[Matrix], dBiases: Seq[Vector]): Unit = {
		weights.zip(dWeights).foreach {
			case (w, dw) =>
				w.update(w + dw)
		}
		biases.zip(dBiases).foreach {
			case (b, db) =>
				assert(b.length == db.length)
				for (i <- b.indices) {
					b(i) = db(i)
				}
		}
	}

}
