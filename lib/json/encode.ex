defimpl JSON.Encoder, for: PID do
  def to_json(term), do: [pid: inspect(term)] |> JSON.Encoder.to_json
  def typeof(_), do: :object
end
