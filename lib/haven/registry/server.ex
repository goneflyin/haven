defmodule Haven.Registry.Server do
  use GenServer.Behaviour

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
    { :ok, HashDict.new }
  end

  def handle_call({:get, svc_name}, _from, services) do
    # Handles async call to :gen_server.cast(pid, :tst)
    # Ignoring the calling process (_from) but gen_server doesn't!
    # Passing in the list/set of services as state
    # Will process and reply back to calling process which is waiting for reply
    { :reply, HashDict.get(services, svc_name, []), services }
  end
  def handle_call(:dump, _from, services) do
    { :reply, services, services }
  end

  def handle_cast({ :add, service = Service[name: svc_name] }, services) do
    # Handles async fire-and-forget call to :gen_server.cast(pid, { :add, <service> })
    # TODO: verify service instance is not already registered
    svcs = [ service | HashDict.get(services, svc_name, []) ]
    { :noreply, HashDict.put(services, service.name, svcs) }
  end
end