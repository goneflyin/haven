require Logger

defmodule Haven.Registry.Index do

  alias Haven.Registry.Index
  alias Haven.Registry.Service

  defstruct uris: HashDict.new, names: HashDict.new, locations: HashDict.new

  def create do
    {:ok, %Haven.Registry.Index{}}
  end

  def get_service_for_name(%Index{names: names}, name) do
    HashDict.get(names, name)
  end

  def get_service_for_location(%Index{locations: locations}, host, port) do
    HashDict.get(locations, "#{host}:#{port}")
  end

  def get_service_for_uri(index, uri) do
    case for_uri(uri, index.uris) do
      [] -> nil
      [service|_] -> service
    end
  end

  def add_service(index, service = %Service{}) do
    %Index{
           uris:  add_service_to_uri_index(index.uris, service),
           names: add_service_to_name_index(index.names, service),
           locations: add_service_to_location_index(index.locations, service)
    }
  end

  defp add_service_to_uri_index(uri_index, service = %Service{uris: svc_uris}) do
    svc_uris
    |> Enum.reduce(uri_index, fn(uri, uri_index) ->
                                 add_for_uri(uri, service, uri_index) end)
  end

  defp add_service_to_name_index(name_index, service = %Service{name: svc_name}) do
    HashDict.put(name_index, svc_name, service)
  end

  defp add_service_to_location_index(location_index, service = %Service{host: svc_host, port: svc_port}) do
    HashDict.put(location_index, "#{svc_host}:#{svc_port}", service)
  end

  defp add_for_uri(uri, service, index) do
    String.split(uri, "/", trim: true)
      |> _add_for_uri(service, index)
  end

  defp _add_for_uri([], service, index) do
    services = HashDict.get(index, "", [])
    HashDict.put(index, "", [service | services])
  end
  defp _add_for_uri([root | []], service, index) do
    HashDict.put(index, root, _add_for_uri([], service, node_for_fragment(index, root)))
  end
  defp _add_for_uri([root | rest], service, index) do
    HashDict.put(index, root, _add_for_uri(rest, service, node_for_fragment(index, root)))
  end

  defp node_for_fragment(index, fragment) do
    case HashDict.get(index, fragment) do
      nil ->
        HashDict.put(index, fragment, HashDict.new)
          |> HashDict.get(fragment)
      node ->
        node
    end
  end

  defp for_uri(uri, index) when is_binary(uri) do
    _for_uri(String.split(uri, "/", trim: true), index, [])
  end
  defp for_uri(uri, index) when is_list(uri) do
    _for_uri(uri, index, [])
  end

  defp _for_uri([], index, answer) do
    HashDict.get(index, "", answer)
  end
  defp _for_uri("", index, answer) do
    _for_uri([], index, answer)
  end
  defp _for_uri(["" | rest], index, answer) do
    answer = case HashDict.get(index, "") do
               [] -> answer
               instances -> instances
             end
    _for_uri(rest, index, answer)
  end
  defp _for_uri([root | []], index, answer) do
    node_for_root = HashDict.get(index, root, index)
    _for_uri("", node_for_root, answer)
  end
  defp _for_uri([root | rest], index, answer) do
    node_for_root = HashDict.get(index, root, HashDict.new)
    _for_uri(rest, node_for_root, answer)
  end
end
