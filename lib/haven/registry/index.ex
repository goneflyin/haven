defmodule Haven.Registry.Index do

  alias Haven.Registry.Service

  def create do
    {:ok, HashDict.new}
  end

  def get_service_for_name(_, _) do
    nil
  end

  def get_service_for_uri(index, uri) do
    case for_uri(uri, index) do
      [] -> nil
      [service|tail] -> service
    end
  end

  def add_service(index, service = %Service{uris: svc_uris}) do
    svc_uris
    |> Enum.reduce(index, fn(uri, index) -> add_for_uri(uri, service, index) end)
  end

  def add_for_uri(uri, handler, s) do
    String.split(uri, "/", trim: true)
      |> _add_for_uri(handler, s)
  end

  def _add_for_uri([], handler, s) do
    # IO.puts("_add_for_uri(A): uri: [], s: #{storage_to_chars(s)}")
    handlers = HashDict.get(s, "", [])
    HashDict.put(s, "", [handler | handlers])
  end
  def _add_for_uri([root | []], handler, s) do
    # IO.puts("_add_for_uri(B): uri: [#{root} | []], s: #{storage_to_chars(s)}")
    HashDict.put(s, root, _add_for_uri([], handler, node_for_fragment(s, root)))
  end
  def _add_for_uri([root | rest], handler, s) do
    # IO.puts("_add_for_uri(C): uri: [#{root} | #{list_to_chars(rest)}], s: #{storage_to_chars(s)}")
    HashDict.put(s, root, _add_for_uri(rest, handler, node_for_fragment(s, root)))
  end

  def node_for_fragment(s, fragment) do
    found_node = HashDict.get(s, fragment)
    # if found_node != nil do
    #   IO.puts("found_node: #{storage_to_chars(found_node)}")
    # else
    #   IO.puts("found_node: nil")
    # end

    case found_node do
      nil ->
        HashDict.put(s, fragment, HashDict.new)
          |> HashDict.get(fragment)
      node ->
        node
    end
  end


  defp for_uri(uri, s) when is_binary(uri) do
    _for_uri(String.split(uri, "/", trim: true), s, [])
  end
  defp for_uri(uri, s) when is_list(uri) do
    _for_uri(uri, s, [])
  end

  defp _for_uri([], s, answer) do
    HashDict.get(s, "", answer)
  end
  defp _for_uri("", s, answer) do
    _for_uri([], s, answer)
  end
  defp _for_uri(["" | rest], s, answer) do
    answer = case HashDict.get(s, "") do
               [] -> answer
               instances -> instances
             end
    _for_uri(rest, s, answer)
  end
  defp _for_uri([root | []], s, answer) do
    node_for_root = HashDict.get(s, root, HashDict.new)
    _for_uri("", node_for_root, answer)
  end
  defp _for_uri([root | rest], s, answer) do
    node_for_root = HashDict.get(s, root, HashDict.new)
    _for_uri(rest, node_for_root, answer)
  end
end
