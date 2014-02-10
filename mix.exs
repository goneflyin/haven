defmodule Haven.Mixfile do
  use Mix.Project

  def project do
    [ app: :haven,
      version: "0.0.1",
      build_per_environment: true,
      dynamos: [Haven.Dynamo],
      compilers: [:elixir, :dynamo, :app],
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ registered: [:registry],
      applications: [:cowboy, :dynamo],
      mod: { Haven, [ registry: [:shopping_cart_svc, :identity_api, :collections_svc] ] } ]
  end

  defp deps do
    [ { :cowboy, github: "extend/cowboy" },
      { :dynamo, "~> 0.1.0-dev", github: "elixir-lang/dynamo" },
      { :json,   github: "cblage/elixir-json"},
      { :ex_doc, github: "elixir-lang/ex_doc" }  ]
  end
end
