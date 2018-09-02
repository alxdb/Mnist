package adb.mnist

import adb.mnist.ai.NeuralNetwork
import adb.mnist.idx.{DataSet, ImageData, LabelData}

object Test {
	val labelData = new LabelData(DataSet.Test)
	val imageData = new ImageData(DataSet.Test)

	def main(args: Array[String]): Unit = {
		val network = new NeuralNetwork("seed=1234-batch_size=10-epoch=1-eta=3.0.nn")
		val result = network.result(imageData.inputs.head)
		val cost = network.cost(result, labelData.outputs.head)
		println(f"expected: " + labelData.labels.head + "\nresult: " + result + "\ncost: " + cost)
	}
}
