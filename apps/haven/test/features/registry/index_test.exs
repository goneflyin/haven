defmodule IndexTest do
  use ExUnit.Case, async: false

  alias Haven.Registry.Index
  alias Haven.Registry.Service

  @coll_svc  %Service{name: "coll_svc", port: 1234, host: "1.4.9.16", uris: ["/collections", "/other"]}
  @other_svc %Service{name: "other_svc", port: 2345, host: "1.4.9.16", uris: ["/collections/something/deeper"]}

  setup do
    {:ok, index} = Index.create
    {:ok, index: index}
  end

  test "getting service for name when empty should return nil", %{index: index} do
    assert(Index.get_service_for_name(index, "not_there_svc") == nil)
  end

  test "getting service by uri when empty should return nil", %{index: index} do
    assert(Index.get_service_for_uri(index, "/coll") == nil)
  end

  test "service is returned once added", %{index: index} do
    index = Index.add_service(index, @coll_svc)

    assert(Index.get_service_for_name(index, "coll_svc") == @coll_svc)
    assert(Index.get_service_for_uri(index, "/collections") == @coll_svc)
    assert(Index.get_service_for_location(index, "1.4.9.16", 1234) == @coll_svc)
  end

  test "adding same service twice only creates single entry", %{index: index} do
    index = Index.add_service(index, @coll_svc)
    index = Index.add_service(index, @coll_svc)

    assert(Index.get_service_for_name(index, "coll_svc") == @coll_svc)
    assert(Index.get_service_for_uri(index, "/collections") == @coll_svc)
  end

  test "deeper uris are forwarded to the deepest mapped service", %{index: index} do
    index = Index.add_service(index, @coll_svc)
    index = Index.add_service(index, @other_svc)

    assert(Index.get_service_for_uri(index, "/collections/some_id") == @coll_svc)
    assert(Index.get_service_for_uri(index, "/collections/something/deeper") == @other_svc)
  end
end
