use tonic::{transport::Server, Request, Response, Status};
use minmax::{FindRequest, FindResponse, min_max_server::{MinMax, MinMaxServer}};

pub mod minmax {
    tonic::include_proto!("minmax");
}

#[derive(Debug, Default)]
pub struct MinMaxService;

#[tonic::async_trait]
impl MinMax for MinMaxService {
async fn find(&self, request: Request<FindRequest>) -> Result<Response<FindResponse>, Status> {
    // let r = request.into_inner();
    println!("request = {:?}", request);
    let response = minmax::FindResponse {
        min: 0.0,
        max: 1.0
    };

    Ok(Response::new(response))
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let address = "127.0.0.1:50051".parse().unwrap();
    let minmax_service = MinMaxService::default();

    Server::builder()
        .add_service(MinMaxServer::new(minmax_service))
        .serve(address)
        .await?;

    Ok(())
}
