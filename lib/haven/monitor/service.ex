defmodule Haven.Monitor.Service do
  use GenServer.Behaviour

  alias Haven.Registry.Service

  ######################
  # External API
  def start_link(service) do
    :gen_server.start_link(__MODULE__, service, [])
  end

  def add_instance(pid, instance) do # {host, port, uris}
    :gen_server.call(pid, {:add_instance, instance})
  end

  def get_name(pid) do
    :gen_server.call(pid, :get_name)
  end

  ##########################
  # GenServer Implementation
  def init(service = Service[name: name]) do
    { :ok, service }
  end

  def handle_call({:add_instance, instance}, _from, state) do
    IO.puts "need to do something to add instance [#{inspect instance}] to service #{state}"
    { :reply, :ok, state }
  end

  def handle_call(:get_name, _from, service) do
    { :reply, { :ok, service.name }, service }
  end
end