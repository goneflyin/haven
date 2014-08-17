defmodule Haven.Mixfile do
  use Mix.Project

  def project do
    [ app: :haven,
      version: "0.1.0",
      elixir: "~> 0.15.0",
      build_per_environment: true,
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ registered: [:registry],
      applications: [:cowboy, :plug, :ibrowse],
      mod: { Haven, [] } ]
  end

  defp deps do
    [ { :cowboy,     "~> 1.0.0" },
      { :plug,       "~> 0.5.3" },
      { :jazz,       "~> 0.2.0" },
      { :ibrowse, tag: "v4.1.1", github: "cmullaparthi/ibrowse" },
      { :httpotion,  "~> 0.2.3" },
      { :ex_doc,     "~> 0.5.1" },
      { :apex,       "~> 0.3.0" }
    ]
  end
end
