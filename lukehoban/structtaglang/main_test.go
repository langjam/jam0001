package main

import (
	"fmt"
	"reflect"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestBasic(t *testing.T) {
	parser := NewParser("Color((_+__)%256,_%256,__%256)", "a")
	expr, err := parser.ParseExpression()
	assert.NoError(t, err)
	assert.IsType(t, expr, &Call{})
	assert.IsType(t, (*(expr.(*Call))).Func, &Identifier{})
	assert.Len(t, (*(expr.(*Call))).Args, 3)
}

func TestImage(t *testing.T) {
	res, err := EvalStruct(reflect.TypeOf(Image{}), []interface{}{})
	assert.NoError(t, err)
	assert.NotNil(t, res)
}

// Vector(x, y, z)
type Vector struct {
	X float64
	Y float64
	Z float64
}

// VectorTimes(v1, x)
type VectorTimes struct {
	Vector `λ:"_0.X*_1,_0.Y*_1,_0.Z*_1"`
}

// VectorPlus(v1, v2)
type VectorPlus struct {
	Vector `λ:"_0.X+_1.X,_0.Y+_1.Y,_0.Z+_1.Z"`
}

// VectorMinus(v1, v2)
type VectorMinus struct {
	Vector `λ:"_0.X-_1.X,_0.Y-_1.Y,_0.Z-_1.Z"`
}

// VectorDot(v1, v2)
type VectorDot struct {
	Value float64 `λ:"(_0.X*_1.X)+(_0.Y*_1.Y)+(_0.Z*_1.Z)"`
}

// VectorNorm(v)
type VectorNorm struct {
	Dot         VectorDot `λ:"_0,_0"`
	VectorTimes `λ:"_0,1.0/(Dot.Value^0.5)"`
}

// Ray(start, dir)
type Ray struct {
	// TODO: Avoid need to copy, perhaps by leaving off expression
	Start Vector `λ:"_0.X,_0.Y,_0.Z"`
	Dir   Vector `λ:"_1.X,_1.Y,_1.Z"`
}

// IntersectSphereDist(radius, eo, v)
type IntersectSphereDist struct {
	EODotEO VectorDot `λ:"_1,_1"`
	Disc    float64   `λ:"(_0^2)-(EODotEO.Value-_2^2)"`
	Value   *float64  `?:"0<Disc" λ:"_2-(Disc^0.5)"`
}

// IntersectSphere(ray)
type IntersectSphere struct {
	Center Vector               `λ:"0,0,0"`
	Radius float64              `λ:"1"`
	EO     VectorMinus          `λ:"Center,_0.Start"`
	V      VectorDot            `λ:"EO,_0.Dir"`
	VDist  *IntersectSphereDist `?:"0<V.Value" λ:"Radius,EO,V.Value"`
	Dist0  *float64             `?:"VDist" λ:"VDist.Value?0"`
	Dist   float64              `λ:"Dist0?0"`
}

// TraceRay(scene, ray, depth)
type TraceRay struct {
	// TODO: Sphere intersection, and then compute color from intersection
	// Isect    IntersectSphere `λ:"_1"`

	TraceRay *TraceRay `?:"_2>0" λ:"_0,_1,_2-1"` // Recursion!!!
}

// TracePixel(camera, x, y)
type TracePixel struct {
	RecenterX      float64     `λ:"(128.0-_1)/512"`
	RecenterY      float64     `λ:"(_2-128.0)/512"`
	Right          VectorTimes `λ:"_0.Right,RecenterX"`
	Up             VectorTimes `λ:"_0.Up,RecenterY"`
	RightUp        VectorPlus  `λ:"Up,Right"`
	RightUpForward VectorPlus  `λ:"RightUp,_0.Forward"`
	Point          VectorNorm  `λ:"RightUpForward"`
	Ray            Ray         `λ:"_0.Pos,Point"`
	Color          TraceRay    `λ:"_0,Ray,0"`
}

// Raytracer(camera)
type Raytracer struct {
	Pixels [256][256]TracePixel `λ:"_0,__0,__1"`
}

// Camera(up, right, forward)
type Camera struct {
	// TODO: Compute from pos and lookAt instead
	Up      Vector `λ:"_0.X,_0.Y,_0.Z"`
	Right   Vector `λ:"_1.X,_1.Y,_1.Z"`
	Forward Vector `λ:"_2.X,_2.Y,_2.Z"`
	Pos     Vector `λ:"_2.X,_2.Y,_2.Z"`
}

// Main()
type Main struct {
	Up        Vector    `λ:"0,0,1"`
	Right     Vector    `λ:"0,1,0"`
	Forward   Vector    `λ:"1,0,0"`
	Camera    Camera    `λ:"Up,Right,Forward"`
	Raytracer Raytracer `λ:"Camera"`
}

func TestIntersectSphere(t *testing.T) {
	t.Parallel()
	res, err := EvalStruct(reflect.TypeOf(IntersectSphere{}), []interface{}{
		Ray{
			Start: Vector{-2, 0, 0},
			Dir:   Vector{1, 0, 0},
		},
	})
	assert.NoError(t, err)
	assert.NotNil(t, res)
	assert.Equal(t, 1.0, res.(IntersectSphere).Dist)
}

func TestTraceRay(t *testing.T) {
	t.Parallel()
	res, err := EvalStruct(reflect.TypeOf(TraceRay{}), []interface{}{nil, Ray{
		Start: Vector{-2, 0, 0},
		Dir:   Vector{1, 0, 0},
	}, 2})
	assert.NoError(t, err)
	assert.NotNil(t, res)
	assert.Nil(t, res.(TraceRay).TraceRay.TraceRay.TraceRay)
}

func TestVectorNorm(t *testing.T) {
	t.Parallel()
	res, err := EvalStruct(reflect.TypeOf(VectorNorm{}), []interface{}{Vector{1.0, 2.0, 3.0}})
	assert.NoError(t, err)
	assert.NotNil(t, res)
	assert.Equal(t, 0.2672612419124244, res.(VectorNorm).X)
}

func TestRaytracer(t *testing.T) {
	t.Parallel()
	res, err := EvalStruct(reflect.TypeOf(Main{}), []interface{}{nil})
	assert.NoError(t, err)
	assert.NotNil(t, res)
	fmt.Printf("%v\n", res.(Main).Raytracer.Pixels[0][0].RightUpForward)
	fmt.Printf("%v\n", res.(Main).Raytracer.Pixels[0][0].Point)
	assert.Equal(t, 1.0, res.(Main).Raytracer.Pixels[0][0].RightUpForward.X)
	assert.Equal(t, 0.25, res.(Main).Raytracer.Pixels[0][0].RightUpForward.Y)
	assert.Equal(t, -0.25, res.(Main).Raytracer.Pixels[0][0].RightUpForward.Z)
}
