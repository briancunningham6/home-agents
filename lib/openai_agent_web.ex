
defmodule OpenAiAgentWeb do
  def live_view do
    quote do
      use Phoenix.LiveView,
        layout: {OpenAiAgentWeb.Layouts, :app}

      unquote(view_helpers())
    end
  end

  def view do
    quote do
      use Phoenix.View,
        root: "lib/openai_agent_web/templates"

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

  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
