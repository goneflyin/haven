require IEx

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
    url = '#{conn.scheme}://#{host}:#{port}#{conn.path}?#{conn.query_string}'
    headers = convert_keys_to_atoms conn.req_headers
    # cookies = conn.req_cookies
    body = conn.req_body
    method = conn.method |> String.downcase |> binary_to_atom
    IO.puts "Url: #{inspect url}"
    IO.puts "Scheme: #{inspect scheme}"
    IO.puts "Headers: "
    Apex.ap headers
    IO.puts "Method: #{inspect method}"
    IO.puts "Body:"
    Apex.ap body
    # IEx.pry
    res = :ibrowse.send_req(url, headers, method, body)
    IO.puts "did it and got..."
    IO.inspect res
    conn.resp 200, "dunno what im a doin here"
  end

  def convert_keys_to_atoms([]), do: []
  def convert_keys_to_atoms([{dynamo_header_key, dynamo_header_value} | rest]) do
    [{binary_to_atom(dynamo_header_key), dynamo_header_value} | convert_keys_to_atoms(rest)]
  end
  def convert_keys_to_atoms(dict) do
    convert_keys_to_atoms(Binary.Dict.to_list(dict))
  end

end
