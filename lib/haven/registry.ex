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

  def from_hash(svc_hash) do
    Service.new(name: HashDict.get(svc_hash, "name"),
                uris: HashDict.get(svc_hash, "uris"),
                host: HashDict.get(svc_hash, "host"),
                port: HashDict.get(svc_hash, "port"))
  end

  def to_hash([service | rest]) do
    [to_hash(service) | to_hash(rest)]
  end
  def to_hash([]), do: []
  def to_hash(service = Service[]) do
    HashDict.new
      |> HashDict.put(:name, service.name)
      |> HashDict.put(:uris, service.uris)
      |> HashDict.put(:host, service.host)
      |> HashDict.put(:port, service.port)
  end
end