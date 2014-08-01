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

Additionally, Haven uses Dynamo, an early web-framework written by,
and for, the Elixir community. However, this will be replaced in the
near future by direct use of Cowboy, a popular web service framework
written in Erlang and very widely used in that community. Dynamo is
also based on Cowboy, but adds an Elixir-oriented abstraction
layer. It has, since the inception of Haven, gone into
maintenance-mode and is no longer recommended for production use in
new projects.

Resources:

* [Elixir website](http://elixir-lang.org/)
* [Elixir getting started guide](http://elixir-lang.org/getting_started/1.html)
* [Elixir docs](http://elixir-lang.org/docs)
* [Dynamo source code](https://github.com/elixir-lang/dynamo)
* [Dynamo guides](https://github.com/elixir-lang/dynamo#learn-more)
* [Dynamo docs](http://elixir-lang.org/docs/dynamo)
* [Cowboy source code](https://github.com/ninenines/cowboy)
* [Cowboy user guide](http://ninenines.eu/docs/en/cowboy/HEAD/guide)
* [Cowboy function reference](http://ninenines.eu/docs/en/cowboy/HEAD/manual)
