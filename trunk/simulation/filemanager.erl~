-module(filemanager).

-export([get_data/1, get_data_by_fullpath/1, write_result/2, write_raw/2]).

get_data(SourceFile) -> 
%% Get the working directory, set complete path y read all lines
    {ok, Cwd} = file:get_cwd(),
    Path = Cwd ++ SourceFile,
    readlines(Path).
    
get_data_by_fullpath(SourceFile) ->
	readlines(SourceFile).
	
%% Read files to get lanes/ lights config
readlines(FileName) ->
    {ok, Device} =  file:open(FileName, [read]),
    Result = get_all_lines(Device, []),
    lists:map(fun(X) -> 
   	         Stripped = string:strip(X, right, $\n),
   		 {ok, ItemsTokens, _} = erl_scan:string(Stripped ++ "."),
		 {ok, Term} = erl_parse:parse_term(ItemsTokens),
		 Term
   	      end,
   	      Result
   	    ).

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        {error, Reason} -> Reason;
        eof  	      -> file:close(Device), lists:reverse (Accum);
        Line 	      -> get_all_lines(Device, [Line|Accum])      
    end.
 
%% Write down the results
write_result(Path, Data) ->
    file:write_file(Path, io_lib:fwrite("~p.\n", [lists:flatten(Data)]),[append]).
    
%% Write down the results as atoms
write_raw(Path, Data) ->
    file:write_file(Path, io_lib:fwrite("~s\n", [lists:flatten(Data)]),[append]).
