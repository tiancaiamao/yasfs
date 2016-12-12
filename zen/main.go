package main

import (
	"fmt"
	"strings"
	"os/exec"
	"os"
)

func main() {
	dir, err := os.Open("test/")
	checkErr(err)
	defer dir.Close()

	files, err := dir.Readdirnames(0)
	checkErr(err)
	for _, f := range files {
		if strings.HasSuffix(f, ".in") {
			fmt.Println("testing file:", f)
			handleFile("test/" + f[:len(f)-3])
		}
	}
}

func checkErr(err error) {
	if err != nil {
		panic(err)
	}
}

func handleFile(file string) {
	err := compile(file)
	checkErr(err)

	err = run(file)
	checkErr(err)

	err = check(file)
	checkErr(err)
}

func compile(file string) error {
	cmd := exec.Command("./cc")
	input, err := os.Open(file + ".in")
	if err != nil {
		return err
	}

	output, err := os.Create(file + ".bc")
	if err != nil {
		return err
	}

	cmd.Stdin = input
	cmd.Stdout = output
	return cmd.Run()
}

func run(file string) error {
	cmd := exec.Command("./vm", file + ".bc")

	output, err := os.Create(file + ".out")
	if err != nil {
		return err
	}
	cmd.Stdout = output
	return cmd.Run()
}

func check(file string) error {
	return nil
}
