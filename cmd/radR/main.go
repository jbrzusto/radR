package main


import (
	"github.com/senseyeio/roger"
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	rClient, err := roger.NewRClient("127.0.0.1", 6311)
	if err != nil {
		log.Fatalf("can't start R client %s", err.Error())
	}
	sess, err := rClient.GetSession()
	_, err = sess.Eval(`
library(base);
library(datasets);
library(utils);
library(grDevices);
library(graphics);
library(stats);
library(methods);
#source("main/radR.R")
`)
	if err != nil {
		log.Fatalf("can't start radR: %s", err.Error())
	}
	reader := bufio.NewReader(os.Stdin)
	for {
		fmt.Print("> ")
		text, _ := reader.ReadString('\n')
		value, err := sess.Eval(text)
		if err != nil {
			fmt.Printf("err: %s\n", err.Error())
		} else {
			fmt.Printf("%v\n", value)
		}
	}
}
