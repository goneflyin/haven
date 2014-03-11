defmodule Haven.Registry.Supervisor do
  use Supervisor.Behaviour

  # A convenience to start the supervisor
  # Should not be needed since this supervisor should be started by HavenSupervisor
  def start_link() do
    IO.puts "Haven.Registry.Supervisor#start_link(): begun"
    result = { :ok, sup } = :supervisor.start_link(__MODULE__, [])
    IO.puts "Haven.Registry.Supervisor#start_link(): result = #{inspect result}"
    start_workers(sup)
    result
  end

  # The callback invoked when the supervisor starts
  def start_workers(sup) do
    { :ok, registry_store } = :supervisor.start_child(sup, worker(Haven.Registry.Store, []))
    :supervisor.start_child(sup, supervisor(Haven.Registry.SubSupervisor, [registry_store]))
  end

  def init(_) do
    IO.puts "Haven.Registry.Supervisor#init()"
    supervise [], strategy: :one_for_one
  end
end
