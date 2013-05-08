-module(artifice_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
               {"/", cowboy_static, [
                   {directory, {priv_dir, artifice, [<<"static">>]}},
                   {file, <<"index.html">>},
                   {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
               ]},
               {"/ws", artifice_client, []},
               {"/[...]", cowboy_static, [
                   {directory, {priv_dir, artifice, [<<"static">>]}},
                   {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
               ]}
        ]}
    ]),
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    {ok, _Pid} = cowboy:start_http(
                   artifice_http_listener, 100,
                   [{port, 8080}],
                   [{env, [{dispatch, Dispatch}]}]
                  ),
    artifice_sup:start_link().

stop(_State) ->
    ok.
