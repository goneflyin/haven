defmodule Haven.Registry.Store do
  use GenServer

  ##############
  # External API
  ##############
  def start_link() do
    # old way
    # result = :gen_server.start_link({:local, :registry_store}, __MODULE__, { HashDict.new }, [])
    # new way
    Agent.start_link(fn -> HashDict.new end, name: __MODULE__)
  end

  def store_registry(registry) do
    # old way
    # :gen_server.cast(pid, {:store, registry})
    # new way
    Agent.update(__MODULE__, fn -> registry end)
  end

  def fetch_registry() do
    # old way
    # :gen_server.call(pid, :fetch)
    # new way
    { Agent.get(__MODULE__, &(&1)) }
  end
end
