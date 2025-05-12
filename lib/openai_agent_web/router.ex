
defmodule OpenAiAgentWeb.Router do
  use Phoenix.Router
  import Phoenix.LiveView.Router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
  end

  scope "/", OpenAiAgentWeb do
    pipe_through :browser
    live "/", AgentLive
  end
end
