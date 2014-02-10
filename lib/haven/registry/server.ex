defmodule Haven.Registry.Server do
  use GenServer.Behaviour

  def start_link(services) do
    # In Supervisor, the worker is specified by Haven.Registry.Server module
    # passing services as an argument.
    # Therefore, the supervisor will (by default) invoke the
    # Haven.Registry.Server.start_link(stack) function to START the worker.
    :gen_server.start_link({ :local, :registry }, __MODULE__, services, [])
  end

  def init(services) do
    { :ok, services }
  end

  def handle_call(:pop, _from, [svc|rest]) do
    # Handles async call to :gen_server.cast(pid, :tst)
    # Ignoring the calling process (_from) but gen_server doesn't!
    # Passing in the list/set of services as state
    # Will process and reply back to calling process which is waiting for reply
    { :reply, "You popped and here it is: #{svc}", rest }
  end
  def handle_call(:peek, _from, [svc|rest]) do
    { :reply, "You peeked and here it is: #{svc}", [svc|rest] }
  end
  def handle_call(:dump, _from, services) do
    { :reply, services, services }
  end

  def handle_cast({ :push, service }, services) do
    # Handles async fire-and-forget call to :gen_server.cast(pid, { :tst_cast, <service> })
    { :noreply, [service|services] }
  end
end