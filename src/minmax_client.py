import sys
import grpc
import minmax_pb2
import minmax_pb2_grpc

from concurrent import futures
from math import sqrt, ceil, inf
from random import uniform
from contextlib import contextmanager
from time import perf_counter

@contextmanager
def timeit():
    start = perf_counter()
    yield
    print(f"Tempo = {perf_counter() - start}s")

def run(args):
    numbers, target = args

    with grpc.insecure_channel(target) as channel:
        stub = minmax_pb2_grpc.MinMaxStub(channel)
        request = minmax_pb2.FindRequest(numbers=numbers)
        return stub.Find(request)

def main():
    MAX = 500_000
    numbers = [sqrt((i - uniform(0, MAX)/2)**2) for i in range(MAX)]

    n_workers = len(sys.argv) - 1
    offset = ceil(MAX/n_workers)
    numbers = [numbers[i*offset:(i+1)*offset] for i in range(n_workers)]

    with futures.ProcessPoolExecutor(max_workers=n_workers) as executor, timeit():
        response = minmax_pb2.FindResponse(min=inf, max=-inf)
        for r in executor.map(run, zip(numbers, sys.argv[1:])):
            response.min = min(r.min, response.min)
            response.max = max(r.max, response.max)

    print(f"MIN = {response.min}\nMAX = {response.max}")


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} IP:PORT...")
        sys.exit(1)

    main()
