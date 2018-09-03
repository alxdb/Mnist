package adb.mnist

import adb.mnist.ai.NeuralNetwork
import adb.mnist.idx.{DataSet, ImageData, LabelData}
import adb.mnist.la.{Matrix, Vector}

import scala.util.Random

object Train {
	val subsetSize = 10
	val epochs = 30
	val learningRate = 1.0

	val seed = 0
	val startingEpoch = 7

	def main(args: Array[String]): Unit = {
		Random.setSeed(seed)
		println("reading data")
		val labelData = new LabelData(DataSet.Train)
		val imageData = new ImageData(DataSet.Train)
    var inputs = imageData.inputs
    var outputs = labelData.outputs

    println("initializing network")
		val network = if (startingEpoch == 0) {
			new NeuralNetwork(Seq(imageData.size, 16, 16, labelData.labelOptions))
		} else {
			new NeuralNetwork(f"$seed-$startingEpoch%02d.nn")
		}
		for (epoch <- startingEpoch until epochs) {
			println(s"epoch = $epoch")
			for (batch <- labelData.labels.indices.grouped(subsetSize)) {
				val deltas: Seq[(Seq[Matrix], Seq[Vector])] = for (example <- batch) yield network.backProp(inputs(example), outputs(example))
				val initDelta: (Seq[Matrix], Seq[Vector]) = (deltas.head._1.map(w => new Matrix(w.rows, w.cols)), deltas.head._2.map(b => new Vector(b.length)))
				val delta: (Seq[Matrix], Seq[Vector]) = deltas.fold(initDelta)((accLayer, layer) => {
					val newW = layer._1.zip(accLayer._1).map { case (w, aw) => aw - (w * (learningRate / subsetSize)) }
					val newB = layer._2.zip(accLayer._2).map { case (b, ab) => ab - (b * (learningRate / subsetSize)) }
					(newW, newB)
				})
				network.update(delta._1, delta._2)
        if (batch.head % 1000 == 0) {
          val approxCost = network.avgCost(for (example <- batch) yield (inputs(example), outputs(example)))
          print(f"${((batch.last + 1).toDouble / labelData.outputs.size) * 100}%04.2f%%: cost ~= $approxCost%6.5f\r")
          Console.flush()
        }
			}
      println()
			network.save(f"$seed-${epoch + 1}%02d.nn")
      val eval = imageData.inputs.zip(labelData.labels).foldLeft(0) {
        case (acc, (i, label)) =>
          val result = network.result(i).zipWithIndex.maxBy(_._1)._2
          if (result == label) {
            acc + 1
          } else {
            acc + 0
          }
      }
      println(f"train eval : $eval / ${labelData.outputs.size} = accuracy of ${(eval.toDouble / labelData.outputs.size) * 100}%.2f%%")
      println("shuffling")
      val shuffled = Random.shuffle(inputs.zip(outputs))
      inputs = shuffled.map(_._1)
      outputs = shuffled.map(_._2)
		}
	}
}
