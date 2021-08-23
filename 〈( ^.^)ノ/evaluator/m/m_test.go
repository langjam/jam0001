package m

import "testing"

func TestNormal(t *testing.T) {
	res, err := Do("(1 + 2) * 3")
	if err != nil {
		t.Errorf("Got error: %s", err.Error())
	}

	if res != "9" {
		t.Errorf("Expected 9, but got %s.", res)
	}
}

func TestRefs(t *testing.T) {
	res, err := Do("($1 + 2) * 3")
	if err != nil {
		t.Errorf("Got error: %s", err.Error())
	}

	if res != "15" {
		t.Errorf("Expected 15, but got %s.", res)
	}
}
