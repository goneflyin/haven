require Logger

defmodule RegistrarRouter do
  use Jazz
  use Plug.Router
  import Plug.Conn

  alias Haven.Registry, as: Registry

  plug :match
  plug :dispatch

  get "/" do
    send_resp(conn, 200, JSON.encode!(Haven.Registry.dump()))
  end

  post "/" do
    {:ok, body, conn} = Plug.Conn.read_body(conn, length: 1_000_000)
    service_spec = JSON.decode!(body)
    add_service(service_spec)
    send_resp(conn, 200, JSON.encode!(Haven.Registry.dump()))
  end

  get "/:name" do
    json = Registry.get_service_for_name(name) |> Registry.to_hash |> JSON.encode!
    Logger.debug("get /:name --> json: #{inspect json}")
    send_resp(conn, 200, json)
  end

  defp add_service(service) do
    Registry.from_hash(service)
      |> Registry.add_service
  end

  def create_response(body) do
    JSON.decode!(body)
      |> handle_decode
      |> JSON.encode!
  end

  def handle_decode({:ok, decoded}) do
    decoded
  end
  def handle_decode({status, input}) do
    {status, [message: "Unexpected Error [#{status}]", json: input]}
  end
end
