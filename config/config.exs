import Config

# ... other configurations ...

# This line imports environment-specific configuration.
# It MUST be at or near the end of the file to ensure it
# can override any general configurations set above.
import_config "#{config_env()}.exs"
