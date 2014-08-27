defmodule Haven.Registry.Store do
  use GenServer

  ##############
  # External API
  ##############
  def start_link() do
    Agent.start_link(fn -> HashDict.new end, name: __MODULE__)
  end

  def store_registry(registry) do
    Agent.update(__MODULE__, fn(_) -> registry end)
  end

  def fetch_registry() do
    Agent.get(__MODULE__, &(&1))
  end
end
