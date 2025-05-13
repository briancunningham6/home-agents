
defmodule OpenAiAgent.Application do
  use Application

  def start(_type, _args) do
    children = [
      # Start the Telemetry supervisor
      OpenAiAgentWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: OpenAiAgent.PubSub},
      # Start the agent registry
      {agent_registry, []},
      # Start the Endpoint
      OpenAiAgentWeb.Endpoint
    ]

    opts = [strategy: :one_for_one, name: OpenAiAgent.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
