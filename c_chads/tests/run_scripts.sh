#!/bin/bash
for file in "scripts"/*
do
  ../bin/COLAN-21 $file
done
