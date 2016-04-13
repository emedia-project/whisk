defmodule Whisk.Mixfile do
	use Mix.Project

	def project do
		[app: :whisk,
		 version: "1.0.0",
		 elixir: "~> 1.2",
		 build_embedded: Mix.env == :prod,
		 start_permanent: Mix.env == :prod,
		 deps: deps]
	end

	def application do
		[applications: []]
	end

	defp deps do
		[ 
			{:bucs, ~r/.*/, git: "https://github.com/botsunit/bucs.git", branch: "master"},
		]
	end
end
