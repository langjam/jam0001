#!/bin/bash

clear
set -e
trap 'catch' ERR

catch () {
    echo '*** FATAL ERROR ***'
    exit 1
}

# sequence

## create rect fact for every vertex that is not an edge/ellipse/text
## sequence.drawio file contains vertexes, and marks all edge and ellipse (and text)
## but does not mark rectangles (the default)
## this pass finds the defaults and creates explicit rect(...) facts
swipl -q \
      -g 'consult(sequence).' \
      -g 'consult(rects).' \
      -g 'printRects.' \
      -g 'halt.' \
      > temp.pl

# augment the factbase (fb.pl) after every inferencing step
cat sequence.pl temp.pl | sort >fb.pl

swipl -g 'consult(fb).' \
      -g 'consult(boundingBoxes).' \
      -g 'printBB.' \
      -g 'halt.' \
      >temp2.pl

# augment the factbase (fb.pl) after every inferencing step
cat fb.pl temp2.pl | sort >temp3.pl
mv temp3.pl fb.pl

##### containment inferencing #########

#_details_
allContains1 (){
swipl -g 'consult(fb).'  -g 'consult(onSameDiagram).' -g 'consult(contain1).' -g 'allContains1.' -g 'halt.' | ./augment-fb.bash 
}
printAllDeepContains (){
swipl -g 'consult(fb).'  -g 'consult(onSameDiagram).' -g 'consult(contain2).' -g 'printAllDeepContains.' -g 'halt.' | ./augment-fb.bash 
}
printAllDirectContains (){
swipl -g 'consult(fb).'  -g 'consult(onSameDiagram).' -g 'consult(contain3).' -g 'printAllDirectContains.' -g 'halt.' | ./augment-fb.bash 
}
designRuleRectanglesMustNotIntersectOnTheSameDiagram (){
swipl -g 'consult(fb).'  -g 'consult(onSameDiagram).' -g 'consult(designRuleRectanglesMustNotIntersect).' -g 'designRuleRectanglesMustNotIntersectOnTheSameDiagram.' -g 'halt.' | ./check-design-rule.bash 
}
printAllPortContains (){
swipl -g 'consult(fb).'  -g 'consult(onSameDiagram).' -g 'consult(containsport).' -g 'printAllPortContains.' -g 'halt.' | ./augment-fb.bash 
}
printAllDirections (){
swipl -g 'consult(fb).'  -g 'consult(onSameDiagram).' -g 'consult(portdirection).' -g 'printAllDirections.' -g 'halt.' | ./augment-fb.bash 
}
# pipeline
allContains1
printAllDeepContains
printAllDirectContains
designRuleRectanglesMustNotIntersectOnTheSameDiagram
printAllPortContains
printAllDirections

# convert fb.pl to "structured" form
swipl -g 'use_module(library(http/json))' \
      -g 'consult(fb).' \
      -g 'consult(component).' \
      -g 'consult(jsoncomponent).'\
      -g 'allc.'\
      -g 'halt.'
