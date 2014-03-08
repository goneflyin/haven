defmodule Haven.Registry.Index do
  defrecord Service, name: nil, uris: [], instances: []
  defrecord Instance, host: nil, port: nil

  def get_service_for_name({name_index, _}, name) do
    []
  end
end