%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
-define(ONE_DAY, (24 * 60 * 60)).
%% -define(PRINT(Format, Args), 
%%     io:format("(~p:~p:~p) : " ++ Format, 
%%               [self(), ?MODULE, ?LINE] ++ Args)).
-define(ERROR, error_logger:error_msg).

-define(PRINT(Format, Args), ok).
