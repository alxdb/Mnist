package adb.mnist

import java.text.SimpleDateFormat
import java.util.Date

import adb.mnist.ai.NeuralNetwork
import adb.mnist.idx.{DataSet, ImageData, LabelData}
import adb.mnist.la.{Matrix, Vector}

import scala.util.Random

object Train {
	val subsets = 10
	val epochs = 30
	val learningRate = 3.0

	val seed = 1234
	val startingEpoch = 4

	def main(args: Array[String]): Unit = {
		Random.setSeed(seed)
		println("reading data")
		val labelData = new LabelData(DataSet.Train)
		val imageData = new ImageData(DataSet.Train)

		val network = if (startingEpoch == 0) {
			new NeuralNetwork(Seq(imageData.inputs.head.length, 16, 16, labelData.labelOptions))
		} else {
			new NeuralNetwork(s"seed=$seed-batch_size=$subsets-epoch=$startingEpoch-eta=$learningRate.nn")
		}
		for (epoch <- startingEpoch until epochs) {
			println(s"epoch = $epoch")
			for (batch <- labelData.labels.indices.grouped(subsets)) {
				if (batch.head % 1000 == 0) {
					println(s"batch = ${batch.head / subsets}")
					val approxCost = network.avgCost(for (example <- batch) yield (imageData.inputs(example), labelData.outputs(example)))
					println(s"approx cost = $approxCost")
				}
				val deltas: Seq[(Seq[Matrix], Seq[Vector])] = for (example <- batch) yield network.backProp(imageData.inputs(example), labelData.outputs(example))
				val initDelta: (Seq[Matrix], Seq[Vector]) = (deltas.head._1.map(w => new Matrix(w.rows, w.cols)), deltas.head._2.map(b => new Vector(b.length)))
				val delta: (Seq[Matrix], Seq[Vector]) = deltas.fold(initDelta)((accLayer, layer) => {
					val newW = layer._1.zip(accLayer._1).map { case (w, aw) => aw + (w * (learningRate / subsets)) }
					val newB = layer._2.zip(accLayer._2).map { case (b, ab) => ab + (b * (learningRate / subsets)) }
					(newW, newB)
				})
				network.update(delta._1, delta._2)
			}
			network.save(s"seed=$seed-batch_size=$subsets-epoch=$epoch-eta=$learningRate.nn")
		}
	}
}
