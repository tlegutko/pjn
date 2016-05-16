#!/bin/bash

# requires spark/bin in PATH

sbt package
spark-submit --class "pjn.lsalsd.SparkLSAGenerator" --master local[4] target/scala-2.11/pjn_2.11-1.0.jar
