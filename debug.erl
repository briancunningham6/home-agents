%% debug.erl
%% Debugging script to understand and fix the client generation issue
-module(debug).
-export([main/0]).

main() ->
    io:format("Starting debug~n"),
    
    % Initialize applications
    io:format("Starting applications~n"),
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    application:ensure_all_started(jsx),
    application:ensure_all_started(agent),
    
    % Check if output directory exists
    OutputDir = "/tmp/openai-erlang",
    io:format("Checking output directory: ~s~n", [OutputDir]),
    case filelib:ensure_dir(OutputDir ++ "/") of
        ok -> io:format("Output directory ready~n");
        {error, Reason} -> io:format("Error creating directory: ~p~n", [Reason])
    end,
    
    % Load API structure
    io:format("Available API groups: ~p~n", [openai_api_structure:get_api_groups()]),
    io:format("Chat endpoints: ~p~n", [openai_api_structure:get_endpoints(chat)]),
    
    % Try to generate the chat module directly
    io:format("Attempting to generate chat module~n"),
    GenerateResult = openai_generator:generate_module(chat, OutputDir),
    io:format("Generation result: ~p~n", [GenerateResult]),
    
    % Check if module exists
    ModuleName = openai_chat,
    ModuleExists = code:which(ModuleName) =/= non_existing,
    io:format("Module ~p exists? ~p~n", [ModuleName, ModuleExists]),
    
    % If module exists, try to ensure it starts
    case ModuleExists of
        true ->
            io:format("Ensuring client starts~n"),
            ClientResult = openai_clients_sup:start_client(chat, #{}),
            io:format("Client start result: ~p~n", [ClientResult]);
        false ->
            io:format("Module does not exist, cannot start client~n")
    end,
    
    % List all modules related to openai
    AllMods = [M || {M, _} <- code:all_loaded(), 
                  string:str(atom_to_list(M), "openai_") =:= 1],
    io:format("All loaded openai modules: ~p~n", [AllMods]),
    
    io:format("Debugging complete~n").