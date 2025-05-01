# Erlang Agent for OpenAI API

A comprehensive, distributed Erlang framework for OpenAI API integration with built-in supervision trees, dynamic API client generation, and tool execution.

## Features

- **Distributed Architecture**: Each API endpoint runs in its own supervised process
- **Dynamic API Client Generation**: Auto-generates client modules from the OpenAI API spec
- **Comprehensive API Coverage**: Supports all OpenAI API endpoints
- **Fault Tolerance**: Built-in supervision trees and error handling
- **Rate Limiting**: Smart rate limiting to prevent API quota issues
- **Streaming Support**: Handles streaming responses
- **Tool Execution**: Register and execute custom tools in your agent
- **Configuration Management**: Centralized configuration with environment variable fallbacks

## Architecture

The system is organized as a hierarchical supervision tree:

```
agent (top-level supervisor)
├── openai_sup (OpenAI API supervisor)
│   ├── openai_generator_sup
│   │   └── openai_generator (generates API client modules)
│   ├── openai_clients_sup (supervises API client processes)
│   │   ├── openai_chat
│   │   ├── openai_completions
│   │   ├── openai_embeddings
│   │   └── ... (one process per API group)
│   ├── openai_rate_limiter (handles rate limiting)
│   └── openai_config (manages configuration)
├── agent_tools (tools registry)
└── agent_registry (tracks active agent processes)
```

## Usage

### Starting the Agent

```erlang
% Start the application
application:ensure_all_started(agent).

% OR use the convenience function
agent:start().
```

### Running an Agent with Tools

```erlang
% Run an agent with tools
Prompt = <<"What is the current time?">>,
ToolNames = [shell, file_read],
Response = agent:run_agent(Prompt, ToolNames).

% Run with custom options
Options = #{
    model => <<"gpt-4">>,
    system_message => <<"You are a helpful assistant with access to system tools.">>,
    timeout => 120000  % 2 minutes
},
Response = agent:run_agent(Prompt, ToolNames, Options).
```

### Defining Custom Tools

```erlang
% Define a custom tool
ToolName = my_custom_tool,
ToolSchema = #{
    <<"name">> => <<"my_custom_tool">>,
    <<"description">> => <<"A custom tool that does something useful">>,
    <<"parameters">> => #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"input">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"Input for the tool">>
            }
        },
        <<"required">> => [<<"input">>]
    }
},
agent:define_tool(ToolName, ToolSchema).

% Register a function to execute the tool
ExecutorFn = fun(_Name, Arguments) ->
    Input = maps:get(<<"input">>, Arguments, <<"">>),
    % Do something with Input
    Result = process_input(Input),
    Result
end,
agent:execute_tool(ToolName, ExecutorFn, #{}).
```

### Direct API Access

You can also access the OpenAI API directly:

```erlang
% Ensure chat API client is available
agent:ensure_api_client(chat).

% Create a chat completion
Model = <<"gpt-4o">>,
Messages = [
    #{role => <<"system">>, content => <<"You are a helpful assistant.">>},
    #{role => <<"user">>, content => <<"What is the capital of France?">>}
],
Options = #{},
{ok, Response} = openai_chat:create_chat_completion(Model, Messages, Options).
```

## Environment Variables

- `OPENAI_API_KEY`: Your OpenAI API key
- `OPENAI_ORGANIZATION`: Your OpenAI organization ID (optional)

## Building

```bash
# Clone the repository
git clone https://github.com/yourusername/erlang-agent.git
cd erlang-agent

# Compile
erlc *.erl

# Run
erl -pa .
```

## License

MIT
