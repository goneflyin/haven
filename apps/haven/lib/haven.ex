defmodule Haven do

  use Application

  @doc """
  The entry point to Haven. Starts a supervisor and triggers all dependent processes.
  """
  def start(_type, args) do
    start_registry(args[:registry])
  end

  def start_registry(_) do
    Haven.Supervisor.start_link([])
  end

end
