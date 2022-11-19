use minmax::{FindRequest, FindResponse, min_max_client::MinMaxClient};

pub mod minmax {
    tonic::include_proto!("minmax");
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut client = MinMaxClient::connect("http://127.0.0.1:50051").await?;

    let request = tonic::Request::new(FindRequest {
        numbers: vec![]
    });

    let response = client.find(request).await?;
    let responseValue = response.into_inner();
    println!("Got: {} {}", responseValue.min, responseValue.max);

    Ok(())
}
