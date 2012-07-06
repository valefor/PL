package main;

import "fmt"

func main() {

    defer func() {
        fmt.Printf("Exit from main!\n")
    } ()

    q2_forLoop()

    return
}

func q2_forLoop() {

    for i := 0;i < 10; i += 1 {
        fmt.Printf("Counting:I=%d.\n",i)
    }

    j := 0
    restart:
        fmt.Printf("Counting:J=%d.\n",j)
        if j++;j< 10 {
            goto restart
        }

    intArray := []int{ 0,1,2,3,4,5,6,7,8,9 }

    for k := range intArray {
        fmt.Printf("Counting:K=%k.\n",k)
    }
}
