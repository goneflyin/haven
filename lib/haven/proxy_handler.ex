defmodule ProxyHandler do

  require Logger

  alias Haven.Registry
  alias Haven.Registry.Service
  alias Plug.Conn

  @filter_headers ["Content-Length"]

  def handle(conn) do
    Registry.get_service_for_uri(conn.path_info)
    |> _handle(conn)
  end

  def _handle(%Service{host: host, port: port}, conn) do
    # TODO: handle multiple services (load balancing)
    conn = conn
    |> Conn.fetch_cookies
    |> Conn.fetch_params

    url = '#{conn.scheme}://#{host}:#{port}/#{conn.path_info}?#{conn.query_string}'
    headers = convert_keys_to_atoms conn.req_headers

    # TODO: handle additional possible return conditions from read_body
    {:ok, body, conn} = Conn.read_body(conn)
    method = conn.method |> String.downcase |> String.to_atom

    :ibrowse.send_req(url, headers, method, body) |> relay_response(conn)
  end
  def _handle(_, conn) do
    Conn.send_resp(conn, 418, "No service registered for URI: #{conn.path_info}")
  end

  def convert_keys_to_atoms([]), do: []
  def convert_keys_to_atoms([{header_key, header_value} | rest]) do
    [{String.to_atom(header_key), header_value} | convert_keys_to_atoms(rest)]
  end
  def convert_keys_to_atoms(dict) do
    convert_keys_to_atoms(Binary.Dict.to_list(dict))
  end

  def relay_response({:ok, status, headers, body}, conn) do
    Enum.filter(headers, &filter_header?/1)
      |> Enum.reduce(conn, &add_header_to_conn/2)
      |> Conn.send_resp(as_integer(status), body)
  end
  def relay_response({:error, {:conn_failed, {:error, :econnrefused}}}, conn) do
    Logger.info "Service Unavailable: #{conn.host}:#{conn.port}/#{conn.path_info}"
    Conn.send_resp(conn, 502, "") # TODO: DO we need a better error message here?
  end

  def as_integer(i) when is_list(i), do: List.to_integer(i)
  def as_integer(i), do: i

  def as_binary(s) when is_list(s), do: List.to_string(s)
  def as_binary(s), do: s

  def filter_header?({header, _}) do
    Enum.find_index(@filter_headers,
                    fn (skip_header) -> as_binary(header) == skip_header end) == nil
  end

  def add_header_to_conn({header_key, value}, conn) do
    Logger.debug "adding_header: {#{inspect header_key}, #{inspect value}}"
    Conn.put_resp_header(conn, as_binary(header_key), as_binary(value))
  end
end
