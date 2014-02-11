defmodule Haven.Registry do

  defrecord Service, name: nil, uris: [], host: "127.0.0.1", port: 8888

  def add_service(svc = Service[]) do
    :gen_server.cast(:registry, { :add, svc })
  end

  def get_services(name) do
    :gen_server.call(:registry, { :get, name })
  end

  def dump do
    :gen_server.call(:registry, :dump)
  end

end