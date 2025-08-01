# SPDX-FileCopyrightText: 2017-2019 myhtmlex authors <https://github.com/Overbryd/myhtmlex>
# SPDX-FileCopyrightText: 2019-2022 Pleroma Authors <https://pleroma.social>
# SPDX-License-Identifier: LGPL-2.1-only

defmodule FastHtml.Mixfile do
  use Mix.Project

  def project do
    [
      app: :fast_html,
      version: "2.4.1",
      elixir: "~> 1.14",
      deps: deps(),
      package: package(),
      compilers: [:elixir_make] ++ Mix.compilers(),
      make_env: make_env(),
      make_error_message: make_error_message(),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      name: "FastHtml",
      description: """
        A module to decode HTML into a tree,
        porting all properties of the underlying
        library lexbor, being fast and correct
        in regards to the html spec.
      """,
      docs: docs(),
      test_coverage: [summary: [threshold: 50]]
    ]
  end

  def package do
    [
      maintainers: ["Ariadne Conill", "rinpatch"],
      licenses: ["LGPL-2.1-only"],
      links: %{
        "GitLab" => "https://git.pleroma.social/pleroma/elixir-libraries/fast_html/",
        "Issues" => "https://git.pleroma.social/pleroma/elixir-libraries/fast_html/issues",
        "lexbor" => "https://github.com/lexbor/lexbor"
      },
      files: hex_files()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :nimble_pool],
      mod: {FastHtml.Application, []}
    ]
  end

  defp deps do
    [
      # documentation helpers
      {:ex_doc, "~> 0.19", only: :dev},
      # benchmarking helpers
      # {:benchee, "~> 1.0", only: :bench, optional: true},
      {:dialyxir, "~> 1.0", only: [:dev, :test], runtime: false},
      # {:myhtmlex, "~> 0.2.0", only: :bench, runtime: false, optional: true},
      # {:mochiweb, "~> 2.18", only: :bench, optional: true},
      #{:html5ever,
      # git: "https://github.com/rusterlium/html5ever_elixir.git", only: :bench, optional: true},
      {:nimble_pool, "~> 1.1"},
      {:elixir_make, "~> 0.4", runtime: false}
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: ["README.md", "CHANGELOG.md"]
    ]
  end

  defp hex_files do
    # This is run every time mix is executed, so it will fail in the hex package,
    # therefore check if git is even available
    if File.exists?(".git") and System.find_executable("git") do
      System.cmd("git", ["submodule", "update", "--init", "--recursive"])
      {files, 0} = System.cmd("git", ["ls-files", "--recurse-submodules"])

      files
      |> String.split("\n")
      # Last element is "", which makes hex include all files in the folder to the project
      |> List.delete_at(-1)
      |> Enum.reject(fn path ->
        Path.dirname(path) == "bench_fixtures" or
          (Path.dirname(path) != "priv" and String.starts_with?(Path.basename(path), "."))
      end)
    else
      []
    end
  end

  defp make_env, do: %{}

  defp make_error_message,
    do:
      "Please check you have: a C compiler, GNU Make, CMake and Erlang development headers installed before reporting an issue."
end
