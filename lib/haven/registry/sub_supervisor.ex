defmodule Haven.Registry.SubSupervisor do
  use Supervisor

  def start_link(registry_store) do
    :supervisor.start_link({:global, :registry_sub_sup}, __MODULE__, registry_store)
  end

  def init(registry_store) do
    children = [ worker(Haven.Registry.Server, [registry_store]) ]
    supervise children, strategy: :one_for_one
  end
end