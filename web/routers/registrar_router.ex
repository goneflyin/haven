defmodule RegistrarRouter do
  use Dynamo.Router

  alias Haven.Registry, as: Registry
  prepare do
    conn
    |> fetch([:params, :body, :headers])
    |> require_content_type_json
    |> add_content_type_json

  end

  get "/" do
    conn.put_private :result_object, [message: "from RegistrarRouter /register"]
  end

  post "/" do
    svc = add_service(JSON.decode(conn.req_body))
    conn.resp 200, JSON.encode!(Haven.Registry.dump())
  end

  get "/:name" do
    IO.puts "GET /#{conn.params[:name]}"
    json = Registry.get_services_by_name(conn.params[:name]) |> Registry.to_hash |> JSON.encode!
    conn.resp 200, json
  end

  defp add_service({:ok, service}) do
    Registry.from_hash(service)
      |> Registry.add_service
  end
  defp add_service({error, data}) do
    IO.puts "got an error [#{error}] decoding service json!"
    IO.puts "got some data with the error: #{data}"
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
    case require_content_type(conn, "application/json") do
      :ok ->
        conn
      :error ->
        halt!(conn.status(422).put_private(:result_object, [message: "content must be in JSON and content-type header must be set"]))
    end
  end

  defp require_content_type(conn, type) do
    case conn.req_headers["content-type"] do
      nil -> :ok
      val -> if String.contains?(val, type), do: :ok, else: :missing
    end
  end

  defp add_content_type_json(conn) do
    conn.put_resp_header "Content-Type", "application/json"
  end

  def handle_decode({:ok, decoded}) do
    decoded
  end
  def handle_decode({status, input}) do
    {status, [message: "Unexpected Error [#{status}]", json: input]}
  end
end