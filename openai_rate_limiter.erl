%% openai_rate_limiter.erl
%% Service that handles rate limiting for OpenAI API requests
-module(openai_rate_limiter).
-behaviour(gen_server).

-export([
    start_link/1,
    check_rate_limit/1,
    register_request/1,
    register_rate_limit_headers/2
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
-define(DEFAULT_RATE_LIMIT, 60). % 60 requests per minute default
-define(DEFAULT_TOKENS_PER_SECOND, 1). % 1 token per second default
-define(RATE_LIMIT_WINDOW, 60000). % 1 minute in milliseconds

-record(state, {
    rate_limits = #{} :: map(),         % Map of {endpoint => rate limit}
    request_history = #{} :: map(),     % Map of {endpoint => [{timestamp, request_id}]}
    tokens_per_interval = #{} :: map(), % Map of {endpoint => tokens per second}
    current_tokens = #{} :: map()       % Map of {endpoint => current available tokens}
}).

%% API Functions
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% Check if a request would exceed rate limits
check_rate_limit(Endpoint) ->
    gen_server:call(?SERVER, {check_rate_limit, Endpoint}).

%% Register a new request
register_request(Endpoint) ->
    gen_server:cast(?SERVER, {register_request, Endpoint, make_ref()}).

%% Register rate limit information from response headers
register_rate_limit_headers(Endpoint, Headers) ->
    % Extract rate limit headers if present
    RateLimitData = extract_rate_limit_headers(Headers),
    gen_server:cast(?SERVER, {update_rate_limits, Endpoint, RateLimitData}).

%% gen_server callbacks
init(Options) ->
    % Initialize with default rate limits for known endpoints
    DefaultRateLimits = #{
        completions => 60,
        chat => 60,
        embeddings => 120,
        images => 50,
        audio => 50,
        moderations => 60,
        assistants => 60,
        threads => 60,
        files => 60
    },
    
    % Get rate limits from options or use defaults
    RateLimits = maps:merge(DefaultRateLimits, maps:get(rate_limits, Options, #{})),
    
    % Initialize tokens per interval
    TokensPerInterval = maps:map(
        fun(_Key, Limit) -> Limit / 60 end, % Convert per-minute to per-second
        RateLimits
    ),
    
    % Start token replenishment timer
    erlang:send_after(1000, self(), replenish_tokens),
    
    {ok, #state{
        rate_limits = RateLimits,
        tokens_per_interval = TokensPerInterval,
        current_tokens = RateLimits % Start with full tokens
    }}.

handle_call({check_rate_limit, Endpoint}, _From, State) ->
    % Check if we have tokens available for this endpoint
    AvailableTokens = maps:get(Endpoint, State#state.current_tokens, ?DEFAULT_RATE_LIMIT),
    
    % If we have at least one token, allow the request
    {reply, AvailableTokens >= 1, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({register_request, Endpoint, RequestId}, State) ->
    % Record the request
    Now = os:timestamp(),
    CurrentHistory = maps:get(Endpoint, State#state.request_history, []),
    NewHistory = [{Now, RequestId} | CurrentHistory],
    
    % Consume a token
    CurrentTokens = maps:get(Endpoint, State#state.current_tokens, ?DEFAULT_RATE_LIMIT),
    NewTokens = maps:put(Endpoint, max(0, CurrentTokens - 1), State#state.current_tokens),
    
    % Clean up history older than the rate limit window
    CleanedHistory = clean_history(NewHistory),
    
    NewRequestHistory = maps:put(Endpoint, CleanedHistory, State#state.request_history),
    
    {noreply, State#state{
        request_history = NewRequestHistory,
        current_tokens = NewTokens
    }};

handle_cast({update_rate_limits, Endpoint, RateLimitData}, State) ->
    % Update rate limits based on headers
    NewRateLimits = case maps:get(limit, RateLimitData, undefined) of
        undefined -> State#state.rate_limits;
        Limit -> maps:put(Endpoint, Limit, State#state.rate_limits)
    end,
    
    % Update tokens per interval
    NewTokensPerInterval = maps:map(
        fun(_Key, Limit) -> Limit / 60 end, % Convert per-minute to per-second
        NewRateLimits
    ),
    
    {noreply, State#state{
        rate_limits = NewRateLimits,
        tokens_per_interval = NewTokensPerInterval
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(replenish_tokens, State) ->
    % Replenish tokens for all endpoints based on their refill rate
    NewTokens = maps:map(
        fun(Endpoint, CurrentTokens) ->
            % Get the tokens per interval for this endpoint
            TokensPerInterval = maps:get(Endpoint, State#state.tokens_per_interval, ?DEFAULT_TOKENS_PER_SECOND),
            
            % Get the max tokens for this endpoint
            MaxTokens = maps:get(Endpoint, State#state.rate_limits, ?DEFAULT_RATE_LIMIT),
            
            % Replenish tokens up to the max
            min(MaxTokens, CurrentTokens + TokensPerInterval)
        end,
        State#state.current_tokens
    ),
    
    % Schedule next token replenishment
    erlang:send_after(1000, self(), replenish_tokens),
    
    {noreply, State#state{current_tokens = NewTokens}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

%% Clean history to remove entries older than the rate limit window
clean_history(History) ->
    Now = os:timestamp(),
    WindowStart = timer:now_diff(Now, {0, ?RATE_LIMIT_WINDOW div 1000, 0}),
    
    lists:filter(
        fun({Timestamp, _RequestId}) ->
            timer:now_diff(Timestamp, {0, 0, 0}) >= WindowStart
        end,
        History
    ).

%% Extract rate limit headers from response
extract_rate_limit_headers(Headers) ->
    % Try to find rate limit headers
    % OpenAI uses: x-ratelimit-limit-requests, x-ratelimit-remaining-requests, x-ratelimit-reset-requests
    LimitHeader = proplists:get_value("x-ratelimit-limit-requests", Headers),
    RemainingHeader = proplists:get_value("x-ratelimit-remaining-requests", Headers),
    ResetHeader = proplists:get_value("x-ratelimit-reset-requests", Headers),
    
    % Parse headers if present
    Limit = case LimitHeader of
        undefined -> undefined;
        LimitStr -> try list_to_integer(LimitStr) catch _:_ -> undefined end
    end,
    
    Remaining = case RemainingHeader of
        undefined -> undefined;
        RemainingStr -> try list_to_integer(RemainingStr) catch _:_ -> undefined end
    end,
    
    Reset = case ResetHeader of
        undefined -> undefined;
        ResetStr -> try list_to_integer(ResetStr) catch _:_ -> undefined end
    end,
    
    #{
        limit => Limit,
        remaining => Remaining,
        reset => Reset
    }.