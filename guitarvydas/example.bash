
#!/bin/bash
# _containment_

clear
set -e
trap 'catch' ERR

catch () {
    echo '*** FATAL ERROR ***'
    exit 1
}
#_details_
setup () {
    cp ../phase2/fb.pl .
}
allContains1 (){
    swipl \
	-g 'consult(fb).'  \
	-g 'consult(onSameDiagram).' \
	-g 'consult(contain1).' \
	-g 'allContains1.' \
	-g 'halt.' | ./augment-fb.bash 
}
printAllDeepContains (){
    swipl \
	-g 'consult(fb).'  \
	-g 'consult(onSameDiagram).' \
	-g 'consult(contain2).' \
	-g 'printAllDeepContains.' \
	-g 'halt.' | ./augment-fb.bash 
}
printAllDirectContains (){
    swipl \
	-g 'consult(fb).'  \
	-g 'consult(onSameDiagram).' \
	-g 'consult(contain3).' \
	-g 'printAllDirectContains.' \
	-g 'halt.' | ./augment-fb.bash 
}
designRuleRectanglesMustNotIntersectOnTheSameDiagram (){
    swipl \
	-g 'consult(fb).'  \
	-g 'consult(onSameDiagram).' \
	-g 'consult(designRuleRectanglesMustNotIntersect).' \
	-g 'designRuleRectanglesMustNotIntersectOnTheSameDiagram.' \
	-g 'halt.' | ./check-design-rule.bash 
}
printAllPortContains (){
    swipl \
	-g 'consult(fb).'  \
	-g 'consult(onSameDiagram).' \
	-g 'consult(containsport).' \
	-g 'printAllPortContains.' \
	-g 'halt.' | ./augment-fb.bash 
}
printAllDirections (){
    swipl \
	-g 'consult(fb).'  \
	-g 'consult(onSameDiagram).' \
	-g 'consult(portdirection).' \
	-g 'printAllDirections.' \
	-g 'halt.' | ./augment-fb.bash 
}
#_fb pipeline_
setup
allContains1
printAllDeepContains
printAllDirectContains
designRuleRectanglesMustNotIntersectOnTheSameDiagram
printAllPortContains
printAllDirections
