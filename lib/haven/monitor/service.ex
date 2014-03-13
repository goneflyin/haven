defmodule Haven.Monitor.Service do
  use GenServer.Behaviour

  alias Haven.Registry.Service

  defrecord State, name: nil, uris: []

  ######################
  # External API
  def start_link(name) do
    :gen_server.start_link(__MODULE__, name, [])
  end

  def register(pid, service) do
    :gen_server.call(pid, {:register, service})
  end

  # def add_instance(pid, instance) do # {host, port, uris}
  #   :gen_server.call(pid, {:add_instance, instance})
  # end

  def get_name(pid) do
    :gen_server.call(pid, :get_name)
  end

  ##########################
  # GenServer Implementation
  def init(name) do
    { :ok, State.new(name: name) }
  end

  # def handle_call({:add_instance, instance}, _from, state) do
  #   IO.puts "need to do something to add instance [#{inspect instance}] to service #{state}"
  #   { :reply, :ok, state }
  # end

  def handle_call(:get_name, _from, state) do
    { :reply, { :ok, Keyword.get(state, :name) }, state }
  end

  def handle_call({:register, instance = Service[name: name]}, _from, state = State[name: name]) do
    IO.puts "Service.Monitor#handle_call :register -- instance: #{inspect instance}, state: #{inspect state}"
    # TODO: Fail Fast if new registration data conflicts sufficiently with existing service data
    #    - name: can't conflict, it is the key used to identify and find this service
    #       --> THEREFORE fail if names don't match -- this registration should never get here if it's not
    #           a service of the same name
    #       --> should be validated by pattern matching in function signature
    #    - host/port: represents an Instance -- handle that in the Instance Monitor
    #    - uris: here is where it gets interesting -- what if uris of this "new" instance vary from the known
    #            uris already registered for this service?

    # TODO: Create Instance Monitor for instances

    IO.puts "Calling -- Service.Monitor#register_uris(new hashset for #{inspect state.uris}, #{inspect instance.uris})"
    { :ok, service_uris } = register_uris(state.uris, HashSet.new(instance.uris))
    { :reply, { :ok }, state }
  end

  def register_uris([], uris) do
    # Case where no instance was yet registered
    { :ok, uris }
  end
  def register_uris(uris, uris) do
    # Case where instance uris are identical to current uris
    { :ok, uris }
  end
  def register_uris(service_uris, instance_uris) do
    # Case where instance uris differ from current uris
    HashSet.union(service_uris, HashSet.new(instance_uris))
  end
end