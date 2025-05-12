
defmodule OpenAiAgentWeb.AgentLive do
  use OpenAiAgentWeb, :live_view

  def mount(_params, _session, socket) do
    {:ok, assign(socket, agents: list_agents(), new_agent: %{})}
  end

  def handle_event("create_agent", %{"agent" => params}, socket) do
    case create_agent(params) do
      {:ok, _agent} ->
        {:noreply,
         socket
         |> put_flash(:info, "Agent created successfully")
         |> assign(agents: list_agents())}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Error creating agent: #{inspect(reason)}")}
    end
  end

  defp list_agents do
    :agent_registry.list_agents()
  end

  defp create_agent(params) do
    :agent.run_agent(params["prompt"], params["tools"] || [], %{})
  end
end
