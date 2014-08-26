require IEx

defmodule Haven.Registry.Server do
  use GenServer

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

  ##########################
  # External API
  ##########################
  def start_link(store_pid) do
    # In Supervisor, the worker is specified by Haven.Registry.Server module
    # passing services as an argument.
    # Therefore, the supervisor will (by default) invoke the
    # Haven.Registry.Server.start_link(stack) function to START the worker.
    # Prefacing the args with {:local, :registry} causes the server to be
    # registered as :registry in the local node.
    :gen_server.start_link({ :local, :registry }, __MODULE__, store_pid, [])
  end

  ##########################
  # GenServer Implementation
  ##########################
  def init(_) do
    services_by_uri = Haven.Registry.Store.fetch_registry()
    { :ok, %{by_uri: services_by_uri} }
  end

  def handle_call({:get_by_name, svc_name}, _from, state) do
    svcs = for_name(svc_name)
    { :reply, svcs, state }
  end
  def handle_call({:get_by_uri, uri}, _from, state) do
    svcs = for_uri(uri, state.by_uri)
    { :reply, svcs, state }
  end
  def handle_call(:dump, _from, state) do
    { :reply, state.by_uri, state }
  end
  def handle_call(unknown, _from, state) do
    { :reply, {:error, "Unable to handle_call for #{unknown}"}, state }
  end

  def handle_cast(:clear, _) do
    { :noreply, %{by_uri: HashDict.new} }
  end
  def handle_cast({ :add, service = %Service{name: svc_name, uris: svc_uris} }, state) do
    register(service)
    add_svc = fn(uri, s) -> add_for_uri(uri, service, s) end
    services_by_uri = Enum.reduce(svc_uris, state.by_uri, add_svc)
    { :noreply, %{state|by_uri: services_by_uri}}
  end

  def terminate(reason, { services_by_uri }) do
    #  "Haven.Registry.Server#terminate(): reason = #{inspect reason}"
    result = Haven.Registry.Store.store_registry(services_by_uri)
    # IO.puts "Haven.Registry.Server#terminate: result = #{inspect result}"
    result
  end

  ##########################
  # Registry Implementation
  ##########################
  def register(service = %Service{name: svc_name}) do
    { :ok, monitor_pid } = find_or_create_monitor(svc_name)
    Haven.Monitor.Service.register(monitor_pid, service)
    :ok
  end

  def find_or_create_monitor(name) do
    case Haven.Monitor.Supervisor.start_monitor(name) do
      { :ok, monitor_pid} ->
        { :ok, monitor_pid }
      { :error, { :already_started, monitor_pid } } ->
        { :ok, monitor_pid }
      { :error, error } ->
        { :error, error }
    end
  end

  def for_name(name) do
    Haven.Monitor.Supervisor.pid_for_name(name)
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
