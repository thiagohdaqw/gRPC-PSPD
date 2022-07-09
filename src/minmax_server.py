from concurrent import futures
from functools import reduce
from math import inf

import grpc
import minmax_pb2
import minmax_pb2_grpc


def minmax(response: minmax_pb2.FindResponse, number: float):
    response.max = max(number, response.max)
    response.min = min(number, response.min)
    return response


class MinMax(minmax_pb2_grpc.MinMaxServicer):

    def Find(self, request, context):
        response = minmax_pb2.FindResponse(min=inf, max=-inf)
        return reduce(minmax, request.numbers, response)


def serve():
    server = grpc.server(futures.ThreadPoolExecutor())
    minmax_pb2_grpc.add_MinMaxServicer_to_server(MinMax(), server)
    server.add_insecure_port('[::]:50051')
    server.start()
    server.wait_for_termination()


if __name__ == '__main__':
    serve()
