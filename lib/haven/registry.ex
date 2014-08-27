defmodule Haven.Registry do
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

  defmodule Service do
    defstruct name: nil, uris: [], host: "127.0.0.1", port: 8888
  end

  alias Haven.Registry.Index
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
    GenServer.start_link( __MODULE__, store_pid, name: :registry)
  end

  def clear do
    GenServer.cast(:registry, :clear)
  end

  def add_service(svc = %Service{}) do
    GenServer.cast(:registry, { :add, svc })
  end

  def get_service_for_name(name) do
    GenServer.call(:registry, { :get_by_name, name })
  end

  def get_service_for_uri(url) do
    GenServer.call(:registry, { :get_by_uri, url })
  end

  def dump do
    GenServer.call(:registry, :dump)
  end

  ##########################
  # GenServer Implementation
  ##########################
  def init(_) do
    services_by_uri = Haven.Registry.Store.fetch_registry()
    { :ok, %{by_uri: services_by_uri} }
  end

  def handle_call({:get_by_name, svc_name}, _from, state) do
    svc = Index.get_service_for_name(%Index{names: state.by_name}, svc_name)
    { :reply, svc, state }
  end
  def handle_call({:get_by_uri, uri}, _from, state) do
    svc = Index.get_service_for_uri(%Index{uris: state.by_uri}, uri)
    { :reply, svc, state }
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
  def handle_cast({ :add, service = %Service{} }, state) do
    # register(service)
    index = Index.add_service(%Index{uris: state.by_uri}, service)
    { :noreply, %{state|by_uri: index.uris}}
  end

  def terminate(_reason, state) do
    Haven.Registry.Store.store_registry(state.by_uri)
  end

  # TODO: Move to/from hash functions to Service module
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

# Previous aborted start to use monitors for services instead of structs.
# Still valid and referenced classes do exist, but rework in registry took
# precedence.

  # def register(service = %Service{name: svc_name}) do
  #   { :ok, monitor_pid } = find_or_create_monitor(svc_name)
  #   Haven.Monitor.Service.register(monitor_pid, service)
  #   :ok
  # end

  # def find_or_create_monitor(name) do
  #   case Haven.Monitor.Supervisor.start_monitor(name) do
  #     { :ok, monitor_pid} ->
  #       { :ok, monitor_pid }
  #     { :error, { :already_started, monitor_pid } } ->
  #       { :ok, monitor_pid }
  #     { :error, error } ->
  #       { :error, error }
  #   end
  # end
