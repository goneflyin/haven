defmodule Haven.Registry do

  defmodule Service do
    defstruct name: nil, uris: [], host: "127.0.0.1", port: 8888
  end

  def clear do
    :gen_server.cast(:registry, :clear)
  end

  def add_service(svc = %Service{}) do
    :gen_server.cast(:registry, { :add, svc })
  end

  def get_services_by_name(name) do
    :gen_server.call(:registry, { :get_by_name, name })
  end

  def get_services_by_uri(url) do
    :gen_server.call(:registry, { :get_by_uri, url })
  end

  def dump do
    :gen_server.call(:registry, :dump)
  end

  def from_hash(svc_hash) do
    %Service{name: svc_hash["service"],
                uris: svc_hash["uris"],
                host: svc_hash["host"],
                port: svc_hash["port"]}
  end

  def to_hash([service | rest]) do
    [to_hash(service) | to_hash(rest)]
  end
  def to_hash([]), do: []
  def to_hash(service = %Service{}) do
    HashDict.new
      |> HashDict.put(:service, service.name)
      |> HashDict.put(:uris, service.uris)
      |> HashDict.put(:host, service.host)
      |> HashDict.put(:port, service.port)
  end
end
