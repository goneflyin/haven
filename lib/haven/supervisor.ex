defmodule Haven.Supervisor do
  use Supervisor.Behaviour

  def start_link(_) do
    result = {:ok, sup} = :supervisor.start_link({:local, :haven_sup}, __MODULE__, [])
    start_registry(sup)
    result
  end

  def start_registry(sup) do
    result = :supervisor.start_child(sup, supervisor(Haven.Registry.Supervisor, []))
    result
  end

  def init(_) do
    supervise [], strategy: :one_for_one
  end
end