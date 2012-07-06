package main

import (
    "fmt"
    "time"
)

var c chan int

func main() {
    
    defer func() {
        fmt.Printf("Exit from main!\n")
    } ()
    
    c = make(chan int)

    go ready("Tea",2)
    go ready("Coffee",1)
    fmt.Println("I'm waiting!")
    //time.Sleep(5 *time.Second)
    <-c
    <-c

    return
}

func ready(s string,sec int){
    time.Sleep(time.Duration(sec) * time.Second)
    fmt.Println(s,"is ready!")
    c <- 1
}
