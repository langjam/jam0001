#!/bin/bash
fname=_temp-$RANDOM
cat - fb.pl >${fname}
sort ${fname} >fb.pl
rm ${fname}
