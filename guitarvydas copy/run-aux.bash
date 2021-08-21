#!/bin/bash
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
assignNames (){
swipl -g 'consult(fb).'  -g 'consult(onSameDiagram).' -g 'consult(component).' -g 'consult(names).' -g 'printNames.' -g 'halt.' | ./augment-fb.bash 
}
assignCode (){
swipl -g 'consult(fb).'  -g 'consult(onSameDiagram).' -g 'consult(component).' -g 'consult(code).' -g 'printCode.' -g 'halt.' | ./augment-fb.bash 
}

# pipeline
allContains1
printAllDeepContains
printAllDirectContains
designRuleRectanglesMustNotIntersectOnTheSameDiagram
printAllPortContains
printAllDirections

assignNames
assignCode

# convert fb.pl to "structured" form
swipl -g 'use_module(library(http/json))' \
      -g 'consult(fb).' \
      -g 'consult(component).' \
      -g 'consult(names).' \
      -g 'consult(code).' \
      -g 'consult(jsoncomponent).'\
      -g 'allc.'\
      -g 'halt.'
