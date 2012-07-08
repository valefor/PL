package main

import "fmt"

func main() {

	defer func() {
		fmt.Printf("Exit from main!\n")
	}()

	q6_average([]float64{3.1, 3.14, 2.89})

    q9_stackOperation()

	return
}

// q6: average
func q6_average(floatArray []float64) (av float64) {

	defer func() {
		fmt.Printf("The Average:%f\n", av)
	}()

	var total float64

	switch len(floatArray) {
	case 0:
		av = 0
	default:
		for _, v := range floatArray {
			total += v
		}
		av = total / float64(len(floatArray))
	}

	return
}

// q9: stack
func q9_stackOperation(){
    // Init a int stack
	var stack *IntStack
	stack = new(IntStack)
	stack.init(10)

    // Push stack
	stack.push(1)
	stack.push(2)
	stack.push(3)

    // Pop stack
	stack.pop()
	stack.pop()
}

type IntStack struct {
	pos   int
	capa  int
	stack []int
}

func (iS *IntStack) init(nr int) {

	defer func() {
		fmt.Printf("The initialized stack[%i] size:%d\n", iS.stack, nr)
	}()

	iS.pos = 0
	(iS.stack) = make([]int, nr, nr)
	iS.capa = cap(iS.stack)
}

func (iS *IntStack) push(i int) {

	defer func() {
		fmt.Printf("%d has been pushed into stack!\n", i)
	}()

	if iS.pos < iS.capa {
		(iS.stack)[iS.pos] = i
	} else {
		iS.stack = append(iS.stack, i)
		iS.capa = len(iS.stack)
	}
	iS.pos++
}

func (iS *IntStack) pop() (ret int) {

	defer func() {
		fmt.Printf("%d has been poped out from stack!\n", ret)
	}()

	if iS.pos >= 1 {
		ret = (iS.stack)[iS.pos-1]
		iS.pos--
	} else {
		fmt.Printf("The stack is empty!\n")
		ret = -9999
	}
	return
}
