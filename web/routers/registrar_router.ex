defmodule RegistrarRouter do
  use Dynamo.Router

  prepare do
    conn.fetch [:params, :body]
  end

  get "*" do
    conn.put_private :result_object, [message: "from RegistrarRouter /register"]
  end

  post "*" do
    #param_keys = conn.params |> Binary.Dict.keys
    body = conn.req_body
    conn.resp 200, create_response(body)
    # conn.put_private :result_object, submission
  end

  def create_response(body) do
    JSON.decode(body)
      |> handle_decode
      |> JSON.encode!
  end

  def handle_decode({:ok, decoded}) do
    decoded
  end
  def handle_decode({:unexpected_error, input}) do
    [message: "Unexpected Error! Could not parse: #{input}"]
  end
end