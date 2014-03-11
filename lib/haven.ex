defmodule Haven do
  use Application.Behaviour

  @doc """
  The application callback used to start this
  application and its Dynamos.
  """
  def start(_type, args) do
    Enum.each [:ssl, :ibrowse], &Application.Behaviour.start(&1)
    start_registry(args[:registry])
    # TODO: Eventually need to have a single root supervisor and include
    #   the Dynamo and Registry supervisors both in that supervisor tree.
    Haven.Dynamo.start_link([max_restarts: 5, max_seconds: 5])
  end

  def start_registry(registry_args) do
    Haven.Supervisor.start_link([])
  end

end
