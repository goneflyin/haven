defmodule RegistryTest do
  use Haven.TestCase

  alias Haven.Registry
  alias Haven.Registry.Service

  setup do
    Registry.clear
  end

  test "getting service by name when empty should return empty list" do
    assert(Registry.get_services_by_name("coll_svc") == [])
  end

  test "getting service by uri when empty should return empty list" do
    assert(Registry.get_services_by_name("/coll") == [])
  end

  test "service is returned once added" do
    coll_svc = Service.new(name: "coll_svc", port: 1234, host: "1.4.9.16", uris: ["/collections", "/other"])
    Registry.add_service(coll_svc)

    assert(Registry.get_services_by_name("coll_svc") == [coll_svc])
    assert(Registry.get_services_by_uri("/collections") == [coll_svc])
  end
end