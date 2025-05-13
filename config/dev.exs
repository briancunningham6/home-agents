
import Config

config :openai_agent, OpenAiAgentWeb.Endpoint,
  http: [ip: {0, 0, 0, 0}, port: 4005],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "your_dev_key_here",
  watchers: []
