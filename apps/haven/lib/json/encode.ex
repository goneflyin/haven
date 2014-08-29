use Jazz

defimpl JSON.Encoder, for: PID do
  def to_json(term), do: [pid: inspect(term)] |> JSON.Encoder.to_json
  def typeof(_), do: :object
end

defimpl JSON.Encoder, for: HashDict do
  def encode(self, options) do
    HashDict.to_list(self) |> Enum.into(%{})
  end
end

defimpl JSON.Decoder, for: HashDict do
  def decode(_new, parsed, _options) do
    parsed |> Enum.into(HashDict.new)
  end
end
