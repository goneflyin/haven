defmodule Haven.Monitor.Supervisor do
  use Supervisor.Behaviour

  def start_link() do
    { :ok, sup } = :supervisor.start_link({:local, :mon_sup}, __MODULE__, [])
  end

  def init(_) do
    supervise [], strategy: :one_for_one
  end

  def start_monitor(name) do
    monitor_spec = worker(Haven.Monitor.Service, [name], [id: name])
    IO.puts "supervising a new service monitor -- monitor_spec: #{inspect monitor_spec}"
    :supervisor.start_child(:mon_sup, monitor_spec)
  end
end
