defimpl JSON.Encode, for: PID do
  def to_json(term), do: [pid: inspect(term)] |> JSON.Encode.to_json
  def typeof(_), do: :object
end
