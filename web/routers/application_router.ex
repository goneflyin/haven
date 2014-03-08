require IEx

defmodule ApplicationRouter do
  use Dynamo.Router

  alias Haven.Registry
  alias Haven.Registry.Service

  @header "X-Haven-REST-Proxy"

  forward "/services", to: RegistrarRouter

  match "/*" do
    conn = conn.put_resp_header(@header, "true")
    conn = conn.delete_resp_header "cache-control"
    conn = conn.delete_resp_header "connection"
    # Apex.ap conn.resp_headers
    Registry.get_services_by_uri(conn.path_info_segments())
      |> handle(conn)
    # conn.resp 200, "Would forward to: #{conn.path_info_segments()}"
  end

  def handle([], conn) do
    conn.status(418)
  end
  def handle([Service[host: host, port: port] | _], conn) do
#    Apex.ap service
    conn = conn.fetch([:cookies, :params, :headers, :body])
    # scheme = conn.scheme
    url = '#{conn.scheme}://#{host}:#{port}#{conn.path}?#{conn.query_string}'
    headers = convert_keys_to_atoms conn.req_headers
    # cookies = conn.req_cookies
    body = conn.req_body
    method = conn.method |> String.downcase |> binary_to_atom
#    IO.puts "Url: #{inspect url}"
#    IO.puts "Scheme: #{inspect scheme}"
#    IO.puts "Headers: "
#    Apex.ap headers
#    IO.puts "Method: #{inspect method}"
#    IO.puts "Body: #{inspect body}"
    # IEx.pry
    :ibrowse.send_req(url, headers, method, body) |> relay_response(conn)
    # conn.status 200, "dunno what im a doin here"
  end

  def convert_keys_to_atoms([]), do: []
  def convert_keys_to_atoms([{dynamo_header_key, dynamo_header_value} | rest]) do
    [{binary_to_atom(dynamo_header_key), dynamo_header_value} | convert_keys_to_atoms(rest)]
  end
  def convert_keys_to_atoms(dict) do
    convert_keys_to_atoms(Binary.Dict.to_list(dict))
  end

  def relay_response({:ok, status, headers, body}, conn) do
#    IO.puts "status returned"
#    IO.inspect status
#    IO.puts "headers returned"
#    IO.inspect headers
    Enum.reduce headers, conn, fn({header_key, value}, conn) -> conn.put_resp_header(header_key, value) end
    conn = status |> list_to_integer |> conn.status
    conn = body |> list_to_bitstring |> conn.resp_body
    # conn = conn.resp_body("Body will go here")
  end
end
