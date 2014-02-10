defmodule Haven.Registry.Supervisor do
  use Supervisor.Behaviour

  # A convenience to start the supervisor
  def start_link(default_services) do
    :supervisor.start_link(__MODULE__, default_services)
  end

  # The callback invoked when the supervisor starts
  def init(default_services) do
    children = [ worker(Haven.Registry.Server, [default_services]) ]
    supervise children, strategy: :one_for_one
  end

end
