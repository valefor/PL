package main

import(
  "net"
  "os"
  "fmt"
)

var funcMap = map[string]func(args ...string){
  "IP":IP,
  "Mask":Mask,
  "Resolve":Resolve,
  "Host":Host,
}

func main() {
  if len(os.Args) <2 {
    help(os.Args)
  }

  function,exits := funcMap[ os.Args[1] ]
  if !exits {
    fmt.Fprintf(os.Stderr,"\t Undefined command:%s\n",os.Args[1])
  } else {
    function(os.Args[2:]...)
  }

  return 
}

func IP(s ...string){
  for _,ipStr := range s {
    addr := net.ParseIP(ipStr)
    if addr == nil {
      fmt.Printf("Invalid address:%s\n",ipStr)
    } else {
      fmt.Println("The address is ",addr.String())
    }
  }

  return
}

func Mask(s ...string) {
  for _,ipStr := range s {
    addr := net.ParseIP(ipStr)
    if addr == nil {
      fmt.Printf("Invalid address:%s\n",ipStr)
    } else {
      mask := addr.DefaultMask()
      network := addr.Mask(mask)
      ones,bits := mask.Size()
      fmt.Println("The address is ",addr.String(),
        "|Default Mask length is ",bits,
        "|Leading ones count is",ones,
        "|Mask is (hex) ",mask.String(),
        "|Network is ",network.String(),
        )
    }
  }

  return
}

func Resolve(s ...string) {
  for _,hostName := range s {
    addr,err := net.ResolveIPAddr("ip",hostName)
    if err != nil {
      fmt.Printf("Resolution Error:%s\n",err.Error())
    } else {
      fmt.Println("Resolved address is ",addr.String())
    }
  }

  return
}

func Host(s ...string) {
  for _,ipStr := range s {
    addrs,err := net.LookupHost(ipStr)
    if err != nil {
      fmt.Printf("Can't find host name for this address:%s,Error:%s\n",ipStr,err.Error())
    } else {
    fmt.Println("The host names:")
      for _,hostName := range addrs{
        fmt.Println(hostName)
      }
    }
  }

  return
}

func help( args []string){
  fmt.Fprintf(os.Stderr,"Usage:%s <Command> [Args]\n",args[0])
  fmt.Fprintf(os.Stderr,"  Supported command:\n")
  for name,_ := range funcMap{
    fmt.Fprintf(os.Stderr,"    %s\n",name)
  }
  os.Exit(1)
}

