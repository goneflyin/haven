defmodule Haven do
  use Application

  @doc """
  The application callback used to start this
  application and its Dynamos.
  """
  def start(_type, args) do
    start_registry(args[:registry])
  end

  def start_registry(registry_args) do
    Haven.Supervisor.start_link([])
  end

end
