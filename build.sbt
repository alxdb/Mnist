name := "adb_mnist"

version := "0.1"

scalaVersion := "2.12.6"

mainClass in (Compile, packageBin) := Some("adb.mnist.Train")
