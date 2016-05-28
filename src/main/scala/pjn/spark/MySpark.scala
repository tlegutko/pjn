package pjn.spark

import org.apache.spark.{SparkConf, SparkContext}

object MySpark {
  def createSparkContext(): SparkContext = {
    createSparkContext("nameIsNotImportant")
  }

  def createSparkContext(name: String): SparkContext = {
    val sparkConf = new SparkConf()
      .setAppName(name)
      .setMaster("local[4]")
      .set("spark.local.dir", "/opt/spark_tmp")
    val sc = new SparkContext(sparkConf)
    sc.setLogLevel("ERROR")
    sc
  }
}
