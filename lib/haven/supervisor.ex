defmodule Haven.Supervisor do
  use Supervisor

  def start_link(_) do
    result = {:ok, sup} = Supervisor.start_link(__MODULE__, [])
    start_registry(sup)
    start_cowboy()
    result
  end

  def start_registry(sup) do
    {:ok, _} = Supervisor.start_child(sup, supervisor(Haven.Registry.Supervisor, []))
  end

  def start_cowboy() do
    # cowboy_spec = Plug.Adapters.Cowboy.child_spec(:http, ApplicationRouter, [])
    # { :ok, cowboy_worker } = Supervisor.start_child(sup, worker(cowboy_spec, []))
    #     IO.puts "Haven.Supervisor#start_cowboy: cowboy_worker   = #{inspect cowboy_worker}"
    Plug.Adapters.Cowboy.http(ApplicationRouter, [])
  end

  def init(_) do
    supervise([], strategy: :one_for_one)
  end
end
