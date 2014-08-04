defmodule Haven.Registry.Store do
  use GenServer

  ##############
  # External API
  ##############
  def start_link() do
    :gen_server.start_link({:local, :registry_store}, __MODULE__, { HashDict.new }, [])
  end

  def store_registry(pid, registry) do
    :gen_server.cast(pid, {:store, registry})
  end

  def fetch_registry(pid) do
    result = :gen_server.call(pid, :fetch)
    result
  end

  #############
  # GenServer Implementation
  def init(registry) do
    { :ok, registry }
  end

  # EX: params -- symbol/method or action, pid of calling process, state
  def handle_call(:fetch, _from, stored_registry) do
    { :reply, stored_registry, stored_registry }
  end

  # EX: params -- tuple of method or action and params, state
  def handle_cast({:store, new_registry}, _stored_registry) do
    { :noreply, new_registry } # no need to reply; set state to value provided
  end
end