# File: lib/Whisk.ex
# This file was generated from src/whisk.erl
# Using mix.mk (https://github.com/botsunit/mix.mk)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Whisk do
	def unquote(:"start_link")(arg1, arg2, arg3) do
		:erlang.apply(:"whisk", :"start_link", [arg1, arg2, arg3])
	end
	def unquote(:"connect")(arg1, arg2) do
		:erlang.apply(:"whisk", :"connect", [arg1, arg2])
	end
	def unquote(:"connect")(arg1, arg2, arg3) do
		:erlang.apply(:"whisk", :"connect", [arg1, arg2, arg3])
	end
	def unquote(:"disconnect")(arg1) do
		:erlang.apply(:"whisk", :"disconnect", [arg1])
	end
	def unquote(:"timeout")(arg1, arg2) do
		:erlang.apply(:"whisk", :"timeout", [arg1, arg2])
	end
	def unquote(:"version")(arg1) do
		:erlang.apply(:"whisk", :"version", [arg1])
	end
	def unquote(:"stat")(arg1) do
		:erlang.apply(:"whisk", :"stat", [arg1])
	end
	def unquote(:"stats")(arg1) do
		:erlang.apply(:"whisk", :"stats", [arg1])
	end
	def unquote(:"stats")(arg1, arg2) do
		:erlang.apply(:"whisk", :"stats", [arg1, arg2])
	end
	def unquote(:"set")(arg1, arg2, arg3) do
		:erlang.apply(:"whisk", :"set", [arg1, arg2, arg3])
	end
	def unquote(:"set")(arg1, arg2, arg3, arg4) do
		:erlang.apply(:"whisk", :"set", [arg1, arg2, arg3, arg4])
	end
	def unquote(:"get")(arg1, arg2) do
		:erlang.apply(:"whisk", :"get", [arg1, arg2])
	end
	def unquote(:"delete")(arg1, arg2) do
		:erlang.apply(:"whisk", :"delete", [arg1, arg2])
	end
	def unquote(:"init")(arg1) do
		:erlang.apply(:"whisk", :"init", [arg1])
	end
	def unquote(:"handle_call")(arg1, arg2, arg3) do
		:erlang.apply(:"whisk", :"handle_call", [arg1, arg2, arg3])
	end
	def unquote(:"handle_cast")(arg1, arg2) do
		:erlang.apply(:"whisk", :"handle_cast", [arg1, arg2])
	end
	def unquote(:"handle_info")(arg1, arg2) do
		:erlang.apply(:"whisk", :"handle_info", [arg1, arg2])
	end
	def unquote(:"terminate")(arg1, arg2) do
		:erlang.apply(:"whisk", :"terminate", [arg1, arg2])
	end
	def unquote(:"code_change")(arg1, arg2, arg3) do
		:erlang.apply(:"whisk", :"code_change", [arg1, arg2, arg3])
	end
end
