package edu.gatech.cse6242

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import org.apache.spark.sql.SQLContext
import org.apache.spark.sql.functions._

object Q2 {

    // create case class for dataframe format
    case class Network(src: Int, tgt: Int, weight: Int)

	def main(args: Array[String]) {
    	val sc = new SparkContext(new SparkConf().setAppName("Q2"))
		val sqlContext = new SQLContext(sc)
		import sqlContext.implicits._

    	// read the file
    	val file = sc.textFile("hdfs://localhost:8020" + args(0))
        
        // map to file to dataframe via RDD
		val df = file.map(_.split("\t"))
                    .map(p => Network(p(0).trim.toInt, p(1).trim.toInt, p(2).trim.toInt)).toDF()

        // filter out any entries w/ weight < 5
        val filtered = df.filter("weight >= 5")

        // aggregate weight based on source
        val srcs = filtered.select(df("src"), df("weight"))
        val srcagg = srcs.groupBy(df("src")).agg(sum(df("weight")))

        // aggregate weight based on target
        val tgts = filtered.select(df("tgt"), df("weight"))
        val tgtagg = tgts.groupBy(df("tgt")).agg(sum(df("weight")))

        // join aggregates of source and target weights
        val joined = srcagg.join(tgtagg, srcagg("src") === tgtagg("tgt"))

        // rename columns of joined dataframe
        val colNames2 = Seq("node", "srcwt", "node2", "tgtwt")
        val joined2 = joined.toDF(colNames2: _*)

        // extract only the node and node-weight columns using algorithm provided
        val joined3 = joined2.withColumn("nodewt", joined2("tgtwt") - joined2("srcwt"))
                            .select("node", "nodewt")
                            
        // write file
        joined3.rdd.map(x => x.mkString("\t")).saveAsTextFile("hdfs://localhost:8020" + args(1))
  	}
}