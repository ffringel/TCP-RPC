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
  {vsn, "0.1.0"},
  {modules, [tr_app,
    tr_sup,
    tr_server]},
  {registered, [tr_sup, tr_server]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {tr_app, []}}
]}.