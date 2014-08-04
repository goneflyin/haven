defmodule ProxyHandler do

  alias Haven.Registry
  alias Haven.Registry.Service

  def handle(conn) do
    conn = conn.delete_resp_header "cache-control"
    conn = conn.delete_resp_header "connection"
    Registry.get_services_by_uri(conn.path_info_segments())
      |> _handle(conn)
  end

  def _handle([], conn) do
    conn.status(418)
  end
  def _handle([%Service{host: host, port: port} | _], conn) do
    conn = conn.fetch([:cookies, :params, :headers, :body])
    # scheme = conn.scheme
    url = '#{conn.scheme}://#{host}:#{port}#{conn.path}?#{conn.query_string}'
    headers = convert_keys_to_atoms conn.req_headers
    # cookies = conn.req_cookies
    body = conn.req_body
    method = conn.method |> String.downcase |> String.binary_to_atom
    :ibrowse.send_req(url, headers, method, body) |> relay_response(conn)
  end

  def convert_keys_to_atoms([]), do: []
  def convert_keys_to_atoms([{dynamo_header_key, dynamo_header_value} | rest]) do
    [{String.binary_to_atom(dynamo_header_key), dynamo_header_value} | convert_keys_to_atoms(rest)]
  end
  def convert_keys_to_atoms(dict) do
    convert_keys_to_atoms(Binary.Dict.to_list(dict))
  end

  def relay_response({:ok, status, headers, body}, conn) do
    Enum.reduce headers, conn, fn({header_key, value}, conn) -> conn.put_resp_header(header_key, value) end
    conn = status |> String.list_to_integer |> conn.status
    conn = body |> String.list_to_bitstring |> conn.resp_body
    conn
  end
  def relay_response({:error, {:conn_failed, {:error, :econnrefused}}}, conn) do
    conn.status(502)
  end
end