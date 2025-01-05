package edu.gatech.cse6242;

import java.io.IOException;
import java.util.StringTokenizer;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.util.*;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

public class Q1 {

	public static class TokenizerMapper
       extends Mapper<Object, Text, Text, IntWritable>{

    private Text tgt = new Text();

    public void map(Object key, Text value, Context context
                    ) throws IOException, InterruptedException {
      String weight = null;
      StringTokenizer itr = new StringTokenizer(value.toString());
      itr.nextToken();
      tgt.set(itr.nextToken());
      while (itr.hasMoreTokens()) {
        weight = itr.nextToken();
      }
      int wt = Integer.parseInt(weight);
      context.write(tgt, new IntWritable(wt));
    }
  }

  public static class IntSumReducer
       extends Reducer<Text,IntWritable,Text,IntWritable> {
    private IntWritable result = new IntWritable();

    public void reduce(Text key, Iterable<IntWritable> values,
                       Context context
                       ) throws IOException, InterruptedException {
      int min = 10000;
      for (IntWritable val : values) {
        if (val.get() < min) {
          min = val.get();
        }
      }
      result.set(min);
      context.write(key, result);
    }
  }

  public static void main(String[] args) throws Exception {
    Configuration conf = new Configuration();
    Job job = Job.getInstance(conf, "Q1");
    job.setJarByClass(Q1.class);
    job.setMapperClass(TokenizerMapper.class);
    job.setCombinerClass(IntSumReducer.class);
    job.setReducerClass(IntSumReducer.class);
    job.setOutputKeyClass(Text.class);
    job.setOutputValueClass(IntWritable.class);
    FileInputFormat.addInputPath(job, new Path(args[0]));
    FileOutputFormat.setOutputPath(job, new Path(args[1]));
    System.exit(job.waitForCompletion(true) ? 0 : 1);
  }
}
