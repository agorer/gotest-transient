package some

import (
	"testing"
)

func TestSomeFunctionIsWorking(t *testing.T) {
	some_function()
}

func TestSomeFunctionIsFailing(t *testing.T) {
	some_function()
	t.Fatal("Something is not working!")
}

func TestSomeFunctionInnerTests(t *testing.T) {
	t.Run("inner working test", func(t *testing.T) {
		some_function()
		t.Fatal("Something else is wrong")
	})

	t.Run("inner failing test", func(t *testing.T) {
		some_function()
		t.Fatal("Something is not working!")
	})
}
