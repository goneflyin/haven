defmodule IndexTest do
  use Haven.TestCase

  alias Haven.Registry.Index
  alias Haven.Registry.Index.Service
  alias Haven.Registry.Index.Instance


  test "getting service for name when empty should return empty list" do
    assert(Index.get_service_for_name({nil, nil}, "not_there_svc") == [])
  end

  # test "getting service by uri when empty should return empty list" do
  #   assert(Registry.get_services_by_name("/coll") == [])
  # end

  # test "service is returned once added" do
  #   coll_svc = Service.new(name: "coll_svc", port: 1234, host: "1.4.9.16", uris: ["/collections", "/other"])
  #   Registry.add_service(coll_svc)

  #   assert(Registry.get_services_by_name("coll_svc") == [coll_svc])
  #   assert(Registry.get_services_by_uri("/collections") == [coll_svc])
  # end

  # test "adding same service twice only creates single entry" do
  #   coll_svc = Service.new(name: "coll_svc", port: 1234, host: "1.4.9.16", uris: ["/collections", "/other"])
  #   Registry.add_service(coll_svc)
  #   Registry.add_service(coll_svc)

  #   assert(Registry.get_services_by_name("coll_svc") == [coll_svc])
  #   assert(Registry.get_services_by_uri("/collections") == [coll_svc])
  # end

  # test "deeper uris are forwarded to the deepest mapped service" do
  #   coll_svc       = Service.new(name: "coll_svc", port: 1234, host: "1.4.9.16", uris: ["/collections", "/other"])
  #   other_coll_svc = Service.new(name: "coll_svc", port: 2345, host: "1.4.9.16", uris: ["/collections/something/deeper"])
  #   Registry.add_service(coll_svc)

  #   assert(Registry.get_services_by_uri("/collections/some_id") == [coll_svc])
  #   assert(Registry.get_services_by_uri("/collections/something/deeper") == [other_coll_svc])
  # end
end