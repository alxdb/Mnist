package adb.mnist

import adb.mnist.ai.NeuralNetwork
import adb.mnist.idx.{DataSet, ImageData, LabelData}

object Test {
	val labelData = new LabelData(DataSet.Test)
	val imageData = new ImageData(DataSet.Test)

	def main(args: Array[String]): Unit = {
		val network = new NeuralNetwork("0-13.nn")
    val eval = imageData.inputs.zip(labelData.labels).foldLeft(0) {
      case (acc, (i, label)) =>
        val result = network.result(i).zipWithIndex.maxBy(_._1)._2
        if (result == label) {
          acc + 1
        } else {
          acc + 0
        }
    }
    println(f"eval : $eval / ${labelData.outputs.size} = accuracy of ${(eval.toDouble / labelData.outputs.size) * 100}%.2f%%")
	}
}
