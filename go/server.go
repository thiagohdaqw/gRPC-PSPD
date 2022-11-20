package main

import (
    "context"
    "flag"
    "fmt"
    "log"
    "net"

    // "go/minmax"
    "google.golang.org/grpc"
)

var (
    port = flag.Int("port", 50051, "Porta do servidor")
)

type server struct{}

func (*server) Add(ctx context.Context, in *minmax.FindRequest) (*minmax.FindResponse, error) {
    return &minmax.FindResponse{Min: 0.0, Max: 1.0}
}

func main() {
    flag.Parse()
    lis, err := net.Listen("tcp", fmt.Sprintf(":%d", *port))
    if err != nil {
        log.Fatalf("Falha em escutar: %v", err)
    }

    s := grpc.NewServer()
    minmax.RegisterMinMaxServer(s, &server{})

    if err := s.Serve(lis); err != nil {
        log.Fatalf("Falha em abrir servidor: %s", err)
    }
}
