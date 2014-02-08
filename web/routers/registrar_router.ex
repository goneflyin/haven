defmodule RegistrarRouter do
  use Dynamo.Router

  prepare do
    conn
    |> fetch([:params, :body, :headers])
    |> require_content_type_json
  end

  get "/" do
    conn.put_private :result_object, [message: "from RegistrarRouter /register"]
  end

  get "/headers" do
    conn.put_private :result_object, conn.fetch(:headers).req_headers
  end

  post "/" do
    conn.resp(200, create_response(conn.req_body))
  end

  def create_response(body) do
    body = JSON.decode(body)
             |> handle_decode
             |> JSON.encode!
  end

  defp fetch(conn, to_fetch) do
    conn.fetch to_fetch
  end

  defp require_content_type_json(conn) do
    unless conn.req_headers["content-type"] == "application/json" do
      halt!(conn.status(422).put_private(:result_object, [message: "content must be in JSON and content-type header must be set"]))
    end
  end

  def handle_decode({:ok, decoded}) do
    decoded
  end
  def handle_decode({status, input}) do
    {status, [message: "Unexpected Error [#{status}]", json: input]}
  end
end