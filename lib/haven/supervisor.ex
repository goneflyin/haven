defmodule Haven.Supervisor do
  use Supervisor

  def start_link(_) do
    result = {:ok, sup} = Supervisor.start_link(__MODULE__, [])
    start_registry(sup)
    result
  end

  def start_registry(sup) do
    result = Supervisor.start_child(sup, supervisor(Haven.Registry.Supervisor, []))
    result
  end

  def init(_) do
    supervise([], strategy: :one_for_one)
  end
end
