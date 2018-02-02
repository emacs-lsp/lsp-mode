// Binary test_server is a simple stdio-based server implementing the language
// server specification for testing.
//
//     Copyright (C) 2018  Google LLC
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"os"

	"github.com/sourcegraph/jsonrpc2"
)

func main() {
	var logfile string
	flag.StringVar(&logfile, "logfile", "", "file where the test server should write its logs")
	flag.Parse()
	fd, err := os.Create(logfile)
	if err != nil {
		panic(err)
	}
	defer fd.Close()
	logger := log.New(fd, "[test server] ", log.LstdFlags)
	logger.Print("starting")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	objectStream := jsonrpc2.NewBufferedStream(stdio{}, jsonrpc2.VSCodeObjectCodec{})
	defer objectStream.Close()
	s := &server{cancel}
	conn := jsonrpc2.NewConn(ctx, objectStream, jsonrpc2.HandlerWithError(s.handle), jsonrpc2.LogMessages(logger))
	defer conn.Close()
	logger.Print("waiting for quit")
	<-ctx.Done()
	logger.Print("quitting")
}

type server struct {
	cancel context.CancelFunc
}

func (s *server) handle(ctx context.Context, conn *jsonrpc2.Conn, req *jsonrpc2.Request) (interface{}, error) {
	switch req.Method {
	case "initialize":
		return struct {
			Capabilities struct{} `json:"capabilities"`
		}{}, nil
	case "shutdown", "exit":
		s.cancel()
		return nil, nil
	case "initialized", "textDocument/didOpen", "textDocument/didClose":
		return nil, nil
	}
	return nil, fmt.Errorf("unsupported method %s", req.Method)
}

type stdio struct{}

func (stdio) Read(p []byte) (int, error) {
	return os.Stdin.Read(p)
}

func (stdio) Write(p []byte) (int, error) {
	return os.Stdout.Write(p)
}

func (stdio) Close() error {
	// We don’t close the standard streams.
	return nil
}
