defmodule Haven.Registry.SubSupervisor do
  use Supervisor.Behaviour

  def start_link(registry_store) do
    :supervisor.start_link(__MODULE__, registry_store)
  end

  def init(registry_store) do
    IO.puts "Haven.Registry.SubSupervisor#init(): registry_store = #{inspect registry_store}"
    children = [ worker(Haven.Registry.Server, [registry_store]) ]
    supervise children, strategy: :one_for_one
  end
end