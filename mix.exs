defmodule OJSON.Mixfile do
  use Mix.Project

  def project() do
    [
      app: :ojson,
      version: "1.0.0",
      elixir: "~> 1.4",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      description: description(),
      name: "ojson",
      package: package(),
      source_url: "https://github.com/potatosalad/erlang-ojson"
    ]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application() do
    # Specify extra applications you'll use from Erlang/Elixir
    [
      extra_applications: []
    ]
  end

  # Dependencies can be Hex packages:
  #
  #   {:my_dep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:my_dep, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps() do
    [
      {:benchfella, "~> 0.3", only: :bench},
      {:earmark, "~> 1.2", only: :docs},
      {:ex_doc, "~> 0.15", only: :docs},
      {:exjsx, github: "talentdeficit/exjsx", only: :bench},
      {:jazz, github: "meh/jazz", only: :bench},
      {:jiffy, github: "davisp/jiffy", only: :bench},
      {:json, github: "cblage/elixir-json", only: :bench},
      {:jsone, github: "sile/jsone", only: :bench},
      {:poison, github: "devinus/poison", only: :bench},
      {:quixir, "~> 0.9", only: :test}
    ]
  end

  defp description() do
    """
    Ordered JSON (OJSON) - deterministic or stable serialization
    """
  end

  defp package() do
    [
      name: :ojson,
      files: [
        "CHANGELOG*",
        "include",
        "lib",
        "LICENSE*",
        "mix.exs",
        "priv",
        "README*",
        "rebar.config",
        "src"
      ],
      licenses: ["Mozilla Public License Version 2.0"],
      links: %{
        "GitHub" => "https://github.com/potatosalad/erlang-ojson"
      },
      maintainers: ["Andrew Bennett"]
    ]
  end
end
