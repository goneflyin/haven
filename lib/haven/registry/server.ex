defmodule Haven.Registry.Server do
  use GenServer.Behaviour

  @moduledoc """
  Holds the set of Services and Service Instances that are registered with Haven
  and eligible to have traffic routed to them.

  A Service has a name as well as a set of URIs that it serves. Each Service can
  have 1 or more Instances which provide the service. For example:

  Service[name=collections_svc, uris=["/collections"]
    Instance[host=1.2.3.4, port=9000]
    Instance[host=1.2.3.4, port=9001]
  """
  alias Haven.Registry.Service

  def start_link(services) do
    # In Supervisor, the worker is specified by Haven.Registry.Server module
    # passing services as an argument.
    # Therefore, the supervisor will (by default) invoke the
    # Haven.Registry.Server.start_link(stack) function to START the worker.
    # Prefacing the args with {:local, :registry} causes the server to be
    # registered as :registry in the local node.
    :gen_server.start_link({ :local, :registry }, __MODULE__, services, [])
  end

  def init(_services) do
    { :ok, {HashDict.new, HashDict.new} }
  end

  def handle_call({:get_by_name, svc_name}, _from, {services_by_name, services_by_uri}) do
    { :reply, HashDict.get(services_by_name, svc_name, []), {services_by_name, services_by_uri} }
  end
  def handle_call({:get_by_uri, uri}, _from, {services_by_name, services_by_uri}) do
    { :reply, for_uri(uri, services_by_uri), {services_by_name, services_by_uri} }
  end
  def handle_call(:dump, _from, {services_by_name, services_by_uri}) do
    { :reply, {services_by_name, services_by_uri}, {services_by_name, services_by_uri} }
  end
  def handle_call(unknown, _from, {services_by_name, services_by_uri}) do
    { :reply, {:error, "Unable to handle_call for unknown"}, {services_by_name, services_by_uri} }
  end

  def handle_cast(:clear, {_, _}) do
    { :noreply, { HashDict.new, HashDict.new } }
  end
  def handle_cast({ :add, service = Service[name: svc_name, uris: svc_uris] }, {services_by_name, services_by_uri}) do
    # TODO: verify service instance is not already registered
    svcs = [ service | HashDict.get(services_by_name, svc_name, []) ]
    add_svc = fn(uri, s) -> add_for_uri(uri, service, s) end
    services_by_uri = Enum.reduce(svc_uris, services_by_uri, add_svc)
    # first_uri = List.first(svc_uris)
    # services_by_uri = add_for_uri(first_uri, service, services_by_uri)
    { :noreply, { HashDict.put(services_by_name, service.name, svcs), services_by_uri } }
  end

  def for_uri(uri, s) when is_binary(uri) do
    _for_uri(String.split(uri, "/", trim: true), s, [])
  end
  def for_uri(uri, s) when is_list(uri) do
    _for_uri(uri, s, [])
  end

  def _for_uri([], s, answer) do
    HashDict.get(s, "", answer)
  end
  def _for_uri("", s, answer) do
    _for_uri([], s, answer)
  end
  def _for_uri(["" | rest], s, answer) do
    answer = case HashDict.get(s, "") do
               [] -> answer
               instances -> instances
             end
    _for_uri(rest, s, answer)
  end
  def _for_uri([root | []], s, answer) do
    node_for_root = HashDict.get(s, root, HashDict.new)
    _for_uri("", node_for_root, answer)
  end
  def _for_uri([root | rest], s, answer) do
    node_for_root = HashDict.get(s, root, HashDict.new)
    _for_uri(rest, node_for_root, answer)
  end

  def add_for_uri(uri, handler, s) do
    String.split(uri, "/", trim: true)
      |> _add_for_uri(handler, s)
  end


  def _add_for_uri([], handler, s) do
    # IO.puts("_add_for_uri(A): uri: [], s: #{storage_to_chars(s)}")
    handlers = HashDict.get(s, "", [])
    HashDict.put(s, "", [handler | handlers])
  end
  def _add_for_uri([root | []], handler, s) do
    # IO.puts("_add_for_uri(B): uri: [#{root} | []], s: #{storage_to_chars(s)}")
    HashDict.put(s, root, _add_for_uri([], handler, node_for_fragment(s, root)))
  end
  def _add_for_uri([root | rest], handler, s) do
    # IO.puts("_add_for_uri(C): uri: [#{root} | #{list_to_chars(rest)}], s: #{storage_to_chars(s)}")
    HashDict.put(s, root, _add_for_uri(rest, handler, node_for_fragment(s, root)))
  end

  def node_for_fragment(s, fragment) do
    found_node = HashDict.get(s, fragment)
    # if found_node != nil do
    #   IO.puts("found_node: #{storage_to_chars(found_node)}")
    # else
    #   IO.puts("found_node: nil")
    # end

    case found_node do
      nil ->
        HashDict.put(s, fragment, HashDict.new)
          |> HashDict.get(fragment)
      node ->
        node
    end
  end


  def storage_to_chars(s) do
    list_to_chars(HashDict.to_list(s))
  end
  def list_to_chars([]) do
    "[]"
  end
  def list_to_chars(l) do
    Enum.map(l, &item_to_chars/1)
      |> Enum.reduce(fn(str, acc) -> acc <> str end)
  end
  def item_to_chars(l) when is_list(l) do
    list_to_chars(l)
  end
  def item_to_chars({k,v}) do
    "#{item_to_chars(k)}=#{item_to_chars(v)}"
  end
  def item_to_chars(s) when is_binary(s) do
    "\"#{s}\""
  end
  def item_to_chars(i) do
    "#{i}"
  end
end
