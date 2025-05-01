%% openai_generator.erl
%% Service that generates client modules for OpenAI API endpoints
-module(openai_generator).
-behaviour(gen_server).

-export([
    start_link/1,
    generate_module/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_OUTPUT_DIR, "/tmp/openai-erlang").

-record(state, {
    output_dir = ?DEFAULT_OUTPUT_DIR :: string(),
    compile_options = [] :: list(),
    generated_modules = #{} :: map()
}).

%% API Functions
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% Generate a module for a specific API group
generate_module(ApiGroup, OutputDir) ->
    gen_server:call(?SERVER, {generate_module, ApiGroup, OutputDir}, infinity).

%% gen_server callbacks
init(Options) ->
    % Get output directory from options or use default
    OutputDir = maps:get(output_dir, Options, ?DEFAULT_OUTPUT_DIR),
    
    % Create output directory if it doesn't exist
    filelib:ensure_dir(OutputDir ++ "/"),
    
    % Get compile options
    CompileOptions = maps:get(compile_options, Options, []),
    
    {ok, #state{
        output_dir = OutputDir,
        compile_options = CompileOptions
    }}.

handle_call({generate_module, ApiGroup, OutputDir}, _From, State) ->
    % Use specified output dir or fall back to default
    FinalOutputDir = case OutputDir of
        undefined -> State#state.output_dir;
        "" -> State#state.output_dir;
        Dir -> Dir
    end,
    
    % Generate the client module
    Result = try
        openai_client_template:generate_module(ApiGroup, FinalOutputDir)
    catch
        E:R:S ->
            {error, {module_generation_failed, E, R, S}}
    end,
    
    % Update state with the newly generated module
    NewState = case Result of
        {ok, Module} ->
            NewGeneratedModules = maps:put(ApiGroup, {Module, os:timestamp()}, State#state.generated_modules),
            State#state{generated_modules = NewGeneratedModules};
        _ ->
            State
    end,
    
    % Notify the clients supervisor to start the new client if generation was successful
    case Result of
        {ok, _Module} ->
            openai_clients_sup:start_client(ApiGroup, #{});
        _ ->
            ok
    end,
    
    {reply, Result, NewState};

handle_call({list_generated_modules}, _From, State) ->
    {reply, State#state.generated_modules, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.