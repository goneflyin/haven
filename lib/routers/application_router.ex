# require IEx

defmodule ApplicationRouter do
  import Plug.Conn
  use Plug.Router

  @header "X-Haven-REST-Proxy"

  plug :match
  plug :dispatch

  forward "/services", to: RegistrarRouter

  get "/foo" do
    send_resp(conn, 200, "Hello Bar")
  end

  match _ do
    # TODO: Need to put this header somewhere it is guaranteed to always be added
    conn
      |> put_resp_header(@header, "true")
      |> ProxyHandler.handle
  end
end


# AppRouter -- just forwards registry calls to the registry router and handlers to the handler router
# Registry handler -- registers the service info, which means...
#    - creates/passes-data-on-to ServiceMonitors
#    - indexes which service is handled by each registered URI
#    - raises problems when conflicts appear, e.g. a service tries to register for a URI that is already handled
# Handler router -- calls Handler.prehandle for all calls
# Handler
#    - prehandle will say...
#        :ok for handleable calls,
#        :unknown for calls where the route has no handler
#        :response for calls where some signal implied a data/service query and the data is being provided
#             (or will it? wouldn't it just handle these itself? so it's pretty much :ok or :unknown)
#    - handle will take the call data, make the actual call, and return / process the response
# ServiceMonitor -- known by name, monitors all instances, knowns all URIs that should be handled
# InstanceMonitor -- known by host/port, only directly known by ServiceMonitor, can send messages to
#   ServiceMonitor when responses are in certain ranges or health checks come back as insufficiently healthy
#   Will make health check calls
