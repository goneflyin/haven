defmodule Haven.Registry.Store do
  use GenServer.Behaviour

  ##############
  # External API
  ##############
  def start_link() do
    :gen_server.start_link(__MODULE__, { HashDict.new, HashDict.new }, [])
  end

  def store_registry(pid, registry) do
    IO.puts "store_registry: pid = #{inspect pid} ; registry = #{inspect registry}"
    :gen_server.cast(pid, {:store, registry})
  end

  def fetch_registry(pid) do
    IO.puts "fetch_registry: pid = #{inspect pid}"
    result = :gen_server.call(pid, :fetch)
    IO.puts "fetch_registry: result = #{inspect result}"
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