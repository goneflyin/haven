defmodule ProxyHandler do

  alias Haven.Registry
  alias Haven.Registry.Service
  alias Plug.Conn

  def handle(conn) do
    Conn.delete_resp_header(conn, "cache-control")
    Conn.delete_resp_header(conn, "connection")

    Registry.get_services_by_uri(conn.path_info)
      |> _handle(conn)
  end

  def _handle([], conn) do
    Conn.send_resp(conn, 418, "Nothing to see here, move along, try some other URL whydontcha")
  end
  def _handle([%Service{host: host, port: port} | _], conn) do
    conn = conn |> Conn.fetch_cookies |> Conn.fetch_params
    # scheme = conn.scheme
    url = '#{conn.scheme}://#{host}:#{port}/#{conn.path_info}?#{conn.query_string}'
    headers = convert_keys_to_atoms conn.req_headers
    # cookies = conn.req_cookies
    # TODO: handle additional possible return conditions from read_body
    {:ok, body, conn} = Conn.read_body(conn)
    method = conn.method |> String.downcase |> String.to_atom
    # IO.inspect url
    # IO.inspect headers
    # IO.inspect method
    # IO.inspect body

    :ibrowse.send_req(url, headers, method, body) |> relay_response(conn)
  end

  def convert_keys_to_atoms([]), do: []
  def convert_keys_to_atoms([{dynamo_header_key, dynamo_header_value} | rest]) do
    [{String.to_atom(dynamo_header_key), dynamo_header_value} | convert_keys_to_atoms(rest)]
  end
  def convert_keys_to_atoms(dict) do
    convert_keys_to_atoms(Binary.Dict.to_list(dict))
  end

  def relay_response({:ok, status, headers, body}, conn) do
    # IO.puts "relay_response: status  = #{inspect status} [binary: #{is_binary(status)}]"
    # IO.puts "relay_response: headers = #{inspect headers} [binary: #{is_binary(headers)}]"
    # IO.puts "relay_response: body    = #{inspect body}"
    # IO.puts" relay_response: conn.state = #{inspect conn.state}"

    Enum.reduce headers, conn, fn({header_key, value}, conn) ->
                                 # IO.puts "relay_response--reduction: header_key = #{inspect header_key} [binary: #{is_binary(header_key)}]"
                                 # IO.puts "relay_response--reduction: value      = #{inspect value} [binary: #{is_binary(value)}]"
                                 # IO.puts "relay_response--reduction: conn       = #{inspect conn}"
                                 Conn.put_resp_header(conn, as_binary(header_key), as_binary(value))
    end

    conn = Conn.send_resp(conn, as_integer(status), body)
  end
  def relay_response({:error, {:conn_failed, {:error, :econnrefused}}}, conn) do
    conn.status(502)
  end

  def as_integer(i) when is_list(i), do: List.to_integer(i)
  def as_integer(i), do: i

  def as_binary(s) when is_list(s), do: List.to_string(s)
  def as_binary(s), do: s
end
