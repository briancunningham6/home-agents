
import Config


config :openai_agent, OpenAiAgentWeb.Endpoint,
  http: [ip: {0, 0, 0, 0}, port: 4005],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "sN/ZQkHHJ1dQz9PtVF6o75Xt4fhqD7lDy+vwxvlAWW0p0ou5ZkSkSu3fIhjaTUX6",
  watchers: []

# Configure your database
config :openai_agent, OpenAiAgent.Repo,
  database: "openai_agent_dev",
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

# For development, we disable any cache and enable
# debugging and code reloading.
config :phoenix, :stacktrace_depth, 20
config :phoenix, :plug_init_mode, :runtime
