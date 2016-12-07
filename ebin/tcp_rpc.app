%%%-------------------------------------------------------------------
%%% @author Blaise Fringel
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Dec 2016 10:24 PM
%%%-------------------------------------------------------------------
{application, tcp_rpc, [
  {description, "RPC server"},
  {vsn, "0.1"},
  {modules, [tr_server,
    tr_sup,
    tr_app]},
  {registered, [tr_sup]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {tcp_rpc, []}},
  {env, []}
]}.