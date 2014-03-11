defmodule Haven.Registry.Store do
  use GenServer.Behaviour

  ##############
  # External API
  ##############
  def start_link() do
    :gen_server.start_link(__MODULE__, [], [])
  end

  def store_registry(pid, registry) do
    :gen_server.cast(pid, {:store, registry})
  end

  def fetch_registry(pid) do
    :gen_server.call(pid, :fetch)
  end

  #############
  # GenServer Implementation
  # EX: params -- symbol/method or action, pid of calling process, state
  def handle_call(:fetch, _from, stored_registry) do
    { :reply, stored_registry, stored_registry }
  end

  # EX: params -- tuple of method or action and params, state
  def handle_cast({:store, new_registry}, _stored_registry) do
    { :no_reply, new_registry } # no need to reply; set state to value provided
  end
end