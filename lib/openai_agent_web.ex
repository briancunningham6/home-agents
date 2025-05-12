
defmodule OpenAiAgentWeb do
  def live_view do
    quote do
      use Phoenix.LiveView
      import Phoenix.Component
      
      unquote(view_helpers())
    end
  end

  defp view_helpers do
    quote do
      use Phoenix.Component
      
      # Import basic rendering functionality
      import Phoenix.Template
      
      # Import LiveView helpers
      import Phoenix.LiveView.Helpers
    end
  end
