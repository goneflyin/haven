require Logger

defmodule Haven.Registry.Supervisor do
  use Supervisor

  # A convenience to start the supervisor
  # Should not be needed since this supervisor should be started by HavenSupervisor
  def start_link() do
    result = { :ok, sup } = Supervisor.start_link( __MODULE__, [], name: :registry_sup)
    start_workers(sup)
    Logger.info "Haven.Registry.Supervisor#start_link:    registry_sup     = #{inspect sup}"
    IO.puts ""
    result
  end

  # The callback invoked when the supervisor starts
  def start_workers(sup) do
    { :ok, registry_store } = Supervisor.start_child(sup, worker(Haven.Registry.Store, []))
    Logger.info "Haven.Registry.Supervisor#start_workers: registry_store   = #{inspect registry_store}"
    { :ok, registry_sub_sup } = Supervisor.start_child(sup, supervisor(Haven.Registry.SubSupervisor, [registry_store]))
    Logger.info "Haven.Registry.Supervisor#start_workers: registry_sub_sup = #{inspect registry_sub_sup}"
    { :ok, mon_sup } = Supervisor.start_child(sup, supervisor(Haven.Monitor.Supervisor, []))
    Logger.info "Haven.Registry.Supervisor#start_workers: mon_sup          = #{inspect mon_sup}"
  end

  def init(_) do
    supervise [], strategy: :one_for_one
  end
end
