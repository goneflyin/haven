defmodule Haven.Monitor.Supervisor do
  use Supervisor

  def start_link() do
    { :ok, _sup } = :supervisor.start_link({:local, :mon_sup}, __MODULE__, [])
  end

  def init(_) do
    supervise [], strategy: :one_for_one
  end

  def start_monitor(name) do
    :supervisor.start_child(:mon_sup, worker(Haven.Monitor.Service, [name], [id: name]))
  end

  def pid_for_name(name) do
    pid_for_name(name, :supervisor.which_children(:mon_sup))
  end
  def pid_for_name(name, []) do
    { :none, "No service found for name '#{name}'" }
  end
  def pid_for_name(name, [{name, pid, _, _} | _monitors]) do
    { :ok, pid }
  end
  def pid_for_name(name, [_monitor | monitors]) do
    pid_for_name(name, monitors)
  end
end
