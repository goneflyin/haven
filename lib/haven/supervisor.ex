defmodule Haven.Supervisor do
  use Supervisor.Behaviour

  def start_link(_) do
    result = {:ok, sup} = :supervisor.start_link(__MODULE__, [])
    IO.puts "Haven.Supervisor#start_link(): result = #{inspect result}"
    start_registry(sup)
    result
  end

  def start_registry(sup) do
    IO.puts "Haven.Supervisor#start_registry(): sup = #{inspect sup}"
    result = :supervisor.start_child(sup, supervisor(Haven.Registry.Supervisor, []))
    IO.puts "Haven.Supervisor#start_registry(): result = #{inspect result}"
    result
  end

  def init(_) do
    IO.puts "Haven.Supervisor#init()"
    supervise [], strategy: :one_for_one
  end
end