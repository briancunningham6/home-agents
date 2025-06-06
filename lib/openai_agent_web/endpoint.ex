
defmodule OpenAiAgentWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :openai_agent

  socket "/live", Phoenix.LiveView.Socket

  plug Plug.Static,
    at: "/",
    from: :openai_agent,
    gzip: false,
    only: ~w(assets fonts images favicon.ico robots.txt)

  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]

    # The session will be stored in the cookie and signed,
  # ensuring integrity. Phoenix 1.7+ uses secret_key_base
  # from your endpoint configuration for this by default.
  plug Plug.Session,
    store: :cookie,
    key: "_openai_agent_key",
    signing_salt: "your_signing_saltdsfsdfdsfsdfs"
    # encryption_salt: "your_encryption_salt"  # If you set encrypt: true
    # For most Phoenix 1.7+ apps, just key and store are needed if secret_key_base is set.

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()

  plug Plug.MethodOverride
  plug Plug.Head
  plug OpenAiAgentWeb.Router
end
