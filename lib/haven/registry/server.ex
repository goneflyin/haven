require Logger
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
    svcs = case Index.get_service_for_name(%Index{names: state.by_name}, svc_name) do
            nil -> []
            svc -> [svc]
           end
    # svcs = for_name(svc_name)
    { :reply, svcs, state }
  end
  def handle_call({:get_by_uri, uri}, _from, state) do
    svcs = case Index.get_service_for_uri(%Index{uris: state.by_uri}, uri) do
            nil -> []
            svc -> [svc]
           end
    # svcs = for_uri(uri, state.by_uri)
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
    # register(service)
    index = Index.add_service(%Index{uris: state.by_uri}, service)
    # add_svc = fn(uri, s) -> add_for_uri(uri, service, s) end
    # services_by_uri = Enum.reduce(svc_uris, state.by_uri, add_svc)
    { :noreply, %{state|by_uri: index.uris}}
  end

  def terminate(reason, state) do
    Haven.Registry.Store.store_registry(state.by_uri)
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
end
