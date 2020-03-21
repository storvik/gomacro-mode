package test

import (
	"fmt"
	"net/http"
)

type element struct {
	number int    // Comment here
	str    string // Another comment here
}

type Printer interface {
	Print()
	String()
}

// Satify interface
func (e *element) Print() {
	fmt.Printf("%d, %s\n", e.number, e.str)
}

func (e *element) String() string {
	return fmt.Sprintf("%d, %s\n", e.number, e.str)
}

func anotherFunction(a, b int) int {
	c := a * b
	return c + a + b
}

func main() {
	fmt.Println("Hello there from Golang")

	e := &element{
		number: 54,      // This is a comment
		str:    "Hello", //This is also comment
	}

	http.HandleFunc("/", e.HelloServer)
	http.ListenAndServe(":8080", nil)
}

func (e *element) HelloServer(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello returning string, %s", e.String())
}
