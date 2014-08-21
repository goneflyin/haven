# Haven

Haven is an Elixir-based, software-driven REST-ful router to simplify
the job of maintaining your infrastructure. It provides the following
features:

1. Service Discovery
2. Load Balancing
3. Health Monitoring

## Current State

At this early stage, only the service discovery and a very simple
round-robin form of load balancing is provided.

Additionally, Haven uses Plug, a framework similar to Rack (in Ruby).
It is possible that this will be replaced by direct use of Cowboy, a
popular web service framework written in Erlang and very widely used
in that community.

Resources:

* [Elixir website](http://elixir-lang.org/)
* [Elixir getting started guide](http://elixir-lang.org/getting_started/1.html)
* [Elixir docs](http://elixir-lang.org/docs)
* [Cowboy source code](https://github.com/ninenines/cowboy)
* [Cowboy user guide](http://ninenines.eu/docs/en/cowboy/HEAD/guide)
* [Cowboy function reference](http://ninenines.eu/docs/en/cowboy/HEAD/manual)
* [Plug source code](https://github.com/elixir-lang/plug)
