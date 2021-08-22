package main

import (
	"fmt"
	"math"
	"reflect"
)

type Evaluator struct {
	scope map[string]interface{}
}

func NewEvaluator() *Evaluator {
	return &Evaluator{
		scope: map[string]interface{}{},
	}
}

func (ev *Evaluator) Eval(expr Expression) (interface{}, error) {
	switch ex := expr.(type) {
	case *Identifier:
		v, ok := ev.scope[ex.Tok.String]
		if !ok {
			return nil, fmt.Errorf("no %q in scope", ex.Tok.String)
		}
		return v, nil
	case *BinaryOperator:
		left, err := ev.Eval(ex.Left)
		if err != nil {
			return nil, err
		}
		right, err := ev.Eval(ex.Right)
		if err != nil {
			return nil, err
		}
		switch ex.Tok.String {
		case "?":
			leftVal := reflect.ValueOf(left)
			if !leftVal.IsNil() {
				return leftVal.Elem().Interface(), nil
			}
			return right, nil
		case "%":
			return left.(int) % right.(int), nil
		case "<":
			return floatOrIntBinop("<", left, right, func(x, y int) interface{} { return x < y }, func(x, y float64) interface{} { return x < y })
		case ">":
			return floatOrIntBinop(">", left, right, func(x, y int) interface{} { return x > y }, func(x, y float64) interface{} { return x > y })
		case "+":
			return floatOrIntBinop("+", left, right, func(x, y int) interface{} { return x + y }, func(x, y float64) interface{} { return x + y })
		case "*":
			return floatOrIntBinop("*", left, right, func(x, y int) interface{} { return x * y }, func(x, y float64) interface{} { return x * y })
		case "-":
			return floatOrIntBinop("-", left, right, func(x, y int) interface{} { return x - y }, func(x, y float64) interface{} { return x - y })
		case "/":
			return floatOrIntBinop("/", left, right, func(x, y int) interface{} {
				return x / y
			}, func(x, y float64) interface{} {
				if y == 0 {
					return math.Inf(0)
				}
				return x / y
			})
		case "^":
			return floatOrIntBinop("^", left, right, func(x, y int) interface{} {
				return int(math.Pow(float64(x), float64(y)))
			}, func(x, y float64) interface{} {
				return math.Pow(x, y)
			})
		default:
			panic(fmt.Sprintf("nyi - operator %s", ex.Tok.String))
		}
	case *Literal:
		return ex.Value, nil
	case *Tuple:
		var ret []interface{}
		for _, item := range ex.Items {
			i, err := ev.Eval(item)
			if err != nil {
				return nil, err
			}
			ret = append(ret, i)
		}
		return ret, nil
	case *Lookup:
		x, err := ev.Eval(ex.Base)
		if err != nil {
			return nil, err
		}
		xval := reflect.ValueOf(x)
		if xval.Kind() == reflect.Ptr {
			if xval.IsNil() {
				return nil, fmt.Errorf("lookup on nil receiver %v.%s", x, ex.Property.String)
			}
			xval = xval.Elem()
		}
		propVal := xval.FieldByName(ex.Property.String)
		if !propVal.IsValid() {
			return nil, fmt.Errorf("no field %s on value %v", ex.Property.String, xval)
		}
		return xval.FieldByName(ex.Property.String).Interface(), nil
	case *Call:
		panic("nyi - eval call")
	default:
		panic(fmt.Sprintf("nyi - eval %s", expr.Kind()))
	}
}

func floatOrIntBinop(op string, left, right interface{}, fint func(int, int) interface{}, ffloat func(float64, float64) interface{}) (interface{}, error) {
	leftFloat, isLeftFloat := left.(float64)
	rightFloat, isRightFloat := right.(float64)
	leftInt, isLeftInt := left.(int)
	rightInt, isRightInt := right.(int)
	if isLeftFloat && isRightFloat {
		return ffloat(leftFloat, rightFloat), nil
	} else if isLeftFloat && isRightInt {
		return ffloat(leftFloat, float64(rightInt)), nil
	} else if isLeftInt && isRightFloat {
		return ffloat(float64(leftInt), rightFloat), nil
	} else if isLeftInt && isRightInt {
		return fint(leftInt, rightInt), nil
	}
	panic(fmt.Sprintf("not yet implemented %v %s %v", reflect.TypeOf(left), op, reflect.TypeOf(right)))
}

func EvalType(ev *Evaluator, expr, ifExpr Expression, ty reflect.Type, depth int) (interface{}, error) {
	fmt.Printf("Eval(%v) to %v\n", expr, ty)
	switch ty.Kind() {
	// An array is a `for i:=0i<n;i++ { ... }`
	case reflect.Array:
		var ret []interface{}
		for i := 0; i < ty.Len(); i++ {
			ev.scope[fmt.Sprintf("__%d", depth)] = i
			v, err := EvalType(ev, expr, ifExpr, ty.Elem(), depth+1)
			if err != nil {
				return nil, err
			}
			ret = append(ret, v)
		}
		return ret, nil
	// An struct is a `foo(...)`
	case reflect.Struct:
		v, err := ev.Eval(expr)
		if err != nil {
			return nil, err
		}
		arrV, ok := v.([]interface{})
		if !ok {
			arrV = []interface{}{v}
		}
		return EvalStruct(ty, arrV)
	// An int is a `(...).(int)`
	case reflect.Int, reflect.Uint8, reflect.Float64:
		v, err := ev.Eval(expr)
		if err != nil {
			return nil, err
		}
		i := reflect.ValueOf(v).Convert(ty).Interface()
		return i, err
	case reflect.Ptr:
		var v interface{}
		test, err := ev.Eval(ifExpr)
		if err != nil {
			return nil, err
		}
		var cond bool
		switch reflect.TypeOf(test).Kind() {
		case reflect.Bool:
			cond = test.(bool)
		case reflect.Ptr:
			cond = test != nil
		default:
			return nil, fmt.Errorf("invalid type %v for condition - must be bool or pointer", reflect.TypeOf(test))
		}
		if cond {
			v, err = EvalType(ev, expr, nil, ty.Elem(), depth+1)
			if err != nil {
				return nil, err
			}
		}
		return &v, nil
	default:
		panic(fmt.Sprintf("nyi - eval type %s", ty.Kind()))
	}
}

func EvalStruct(ty reflect.Type, args []interface{}) (interface{}, error) {
	val := reflect.New(ty).Elem()
	for i := 0; i < ty.NumField(); i++ {
		field := ty.Field(i)
		tag := field.Tag.Get("λ")
		if tag == "" {
			// If there is no tag, implicitly pass the i'th argument to the i'th struct field.  This ensures
			// that external struct types can also be used as constructors with positional arguments.
			tag = fmt.Sprintf("_%d", i)
		}
		parser := NewParser(tag, fmt.Sprintf("%s.%s.λ", ty.Name(), field.Name))
		expr, err := parser.ParseExpression()
		if err != nil {
			return nil, err
		}

		ifTag := field.Tag.Get("?")
		var ifExpr Expression
		if ifTag != "" {
			parser := NewParser(ifTag, fmt.Sprintf("%s.%s.?", ty.Name(), field.Name))
			ifExpr, err = parser.ParseExpression()
			if err != nil {
				return nil, err
			}
		}

		ev := NewEvaluator()
		// All args are in scope
		for i, a := range args {
			ev.scope[fmt.Sprintf("_%d", i)] = a
		}
		// Also, all struct fields before this one
		for j := 0; j < i; j++ {
			ev.scope[ty.Field(j).Name] = val.Field(j).Interface()
		}
		v, err := EvalType(ev, expr, ifExpr, field.Type, 0)
		if err != nil {
			return nil, err
		}

		x := reflect.ValueOf(v)
		Set(val.Field(i), x)
		if x.Kind() == reflect.Ptr {
			if x.IsNil() {
				fmt.Printf("%s.%s = nil\n", ty.Name(), field.Name)
			} else {
				fmt.Printf("%s.%s = &%v\n", ty.Name(), field.Name, x.Elem().Interface())
			}
		} else {
			fmt.Printf("%s.%s = %v\n", ty.Name(), field.Name, v)
		}
	}
	return val.Interface(), nil
}

func Set(dest reflect.Value, val reflect.Value) {
	dt := dest.Type()
	switch dt.Kind() {
	case reflect.Int, reflect.Uint8, reflect.Float64:
		dest.Set(val)
	case reflect.Array:
		arrVal := reflect.ValueOf(val.Interface())
		for i := 0; i < dt.Len(); i++ {
			Set(dest.Index(i), arrVal.Index(i))
		}
	case reflect.Struct:
		structVal := reflect.ValueOf(val.Interface())
		for i := 0; i < dt.NumField(); i++ {
			dest.Field(i).Set(structVal.Field(i))
		}
	case reflect.Ptr:
		if !val.Elem().IsNil() {
			someVal := reflect.ValueOf(val.Elem().Interface())
			x := reflect.New(dest.Type().Elem())
			x.Elem().Set(someVal)
			dest.Set(x)
		}
	default:
		panic(fmt.Sprintf("nyi - set %s", dt.Kind()))
	}
}
