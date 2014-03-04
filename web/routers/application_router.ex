defmodule ApplicationRouter do
  use Dynamo.Router

  alias Haven.Registry
  alias Haven.Registry.Service

  forward "/services", to: RegistrarRouter

  match "/*" do
    Registry.get_services_by_uri(conn.path_info_segments())
      |> handle(conn)
    # conn.resp 200, "Would forward to: #{conn.path_info_segments()}"
  end

  def handle([], conn) do
    conn.resp 200, "Nothing to forward to!"
  end
  def handle([service = Service[host: host, port: port] | _], conn) do
    IO.puts "Service found for forwarding!"
    Apex.ap service
    conn = conn.fetch([:cookies, :params, :headers, :body])
    scheme = conn.scheme
    url = '#{conn.scheme}://#{host}:#{port}'
    headers = conn.req_headers
    # cookies = conn.req_cookies
    body = conn.req_body
    method = conn.method
    IO.puts "Url:"
    Apex.ap url
    IO.puts "Scheme:"
    Apex.ap scheme
    IO.puts "Headers:"
    Apex.ap headers
    IO.puts "Method:"
    Apex.ap method
    IO.puts "Body:"
    Apex.ap body
    res = :ibrowse.send_req(url, headers, method, body)
    IO.puts "did it and got..."
    Apex.ap res
    conn.resp 200, "dunno what im a doin here"
  end

end
