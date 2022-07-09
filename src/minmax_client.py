import grpc
import minmax_pb2
import minmax_pb2_grpc
from math import sqrt
from random import uniform
from contextlib import contextmanager
from time import perf_counter

@contextmanager
def timeit():
    start = perf_counter()
    yield
    print(f"Tempo = {perf_counter() - start}s")

def run():
    max = 500_000
    numbers = [sqrt((i - uniform(0, max)/2)**2) for i in range(max)]

    with grpc.insecure_channel('localhost:50051') as channel, timeit():
        stub = minmax_pb2_grpc.MinMaxStub(channel)
        request = minmax_pb2.FindRequest(numbers=numbers)
        response = stub.Find(request)
    print(f"MIN = {response.min}\nMAX = {response.max}")

if __name__ == '__main__':
    run()