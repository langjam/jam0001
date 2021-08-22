#!/bin/bash
tsort topo.txt >sorted.txt
cat header.txt functions.txt sorted.txt trailer.txt >final.bash
