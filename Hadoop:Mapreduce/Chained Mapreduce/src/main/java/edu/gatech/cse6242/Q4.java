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

public class Q4 {

  public static class Mapper1
       extends Mapper<Object, Text, Text, IntWritable>{

    private Text src = new Text();
    private Text tgt = new Text();

    public void map(Object key, Text value, Context context
                    ) throws IOException, InterruptedException {
      StringTokenizer itr = new StringTokenizer(value.toString(), "\t");
      if (itr.hasMoreTokens()) {
        src.set(itr.nextToken());
        context.write(src, new IntWritable(1));
        while (itr.hasMoreTokens()) {
          tgt.set(itr.nextToken());
        }
        context.write(tgt, new IntWritable(-1));
      }
    }
  }

  public static class Reducer1
       extends Reducer<Text,IntWritable,Text,IntWritable> {
    private IntWritable result = new IntWritable();

    public void reduce(Text key, Iterable<IntWritable> values,
                       Context context
                       ) throws IOException, InterruptedException {
      int sum = 0;
      for (IntWritable val : values) {
        sum += val.get();
      }
      result.set(sum);
      context.write(key, result);
    }
  }

	public static class Mapper2
       extends Mapper<Object, Text, Text, IntWritable>{

    private Text diff = new Text();

    public void map(Object key, Text value, Context context
                    ) throws IOException, InterruptedException {
      StringTokenizer itr = new StringTokenizer(value.toString(), "\t");
      while (itr.hasMoreTokens()) {
        diff.set(itr.nextToken());
      }
      context.write(diff, new IntWritable(1));
    }
  }

	public static class Reducer2
       extends Reducer<Text,IntWritable,Text,IntWritable> {
    private IntWritable result = new IntWritable();

    public void reduce(Text key, Iterable<IntWritable> values,
                       Context context
                       ) throws IOException, InterruptedException {
      int sum = 0;
      for (IntWritable val : values) {
        sum += val.get();
      }
      result.set(sum);
      context.write(key, result);
    }
  }

  public static void main(String[] args)throws Exception {
      
    Configuration conf1 =new Configuration();
    Job j1 = Job.getInstance(conf1);
    j1.setJarByClass(Q4.class);
    j1.setMapperClass(Mapper1.class);
    j1.setCombinerClass(Reducer1.class);
    j1.setReducerClass(Reducer1.class);
    j1.setOutputKeyClass(Text.class);
    j1.setOutputValueClass(IntWritable.class);
    Path outputPath = new Path("temp");
    FileInputFormat.addInputPath(j1, new Path(args[0]));
    FileOutputFormat.setOutputPath(j1, outputPath);
    j1.waitForCompletion(true);

    Configuration conf2 = new Configuration();
    Job j2 = Job.getInstance(conf2);
    j2.setJarByClass(Q4.class);
    j2.setMapperClass(Mapper2.class);
    j2.setCombinerClass(Reducer2.class);
    j2.setReducerClass(Reducer2.class);
    j2.setOutputKeyClass(Text.class);
    j2.setOutputValueClass(IntWritable.class);
    FileInputFormat.addInputPath(j2, outputPath);
    FileOutputFormat.setOutputPath(j2, new Path(args[1]));
    System.exit(j2.waitForCompletion(true)?0:1);
  }
}

