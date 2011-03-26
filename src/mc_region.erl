-module(mc_region).

-export([load_file/1]).

-define(LOC_OFFSET,  24/unsigned-big-integer).
-define(LOC_COUNT,    8/unsigned-big-integer).
-define(TIMESTAMP,   32/unsigned-big-integer).
-define(DATA_LENGTH, 32/signed-big-integer).
-define(DATA_CPTYPE,  8/signed-big-integer).

load_file(Path) ->
    case file:read_file(Path) of
	{ok, Data} ->
	    << Locations:4096/binary, Timestamps:4096/binary, BChunks/binary >> = Data,
	    ChunksL = locations_to_list(Locations, Timestamps),
            load_chunks(ChunksL, BChunks);
	{error, enoent} ->
	    io:format("File not found !~n")
    end.

locations_to_list(<<>>, _Timestamps) ->
    [];
%% locations_to_list(<< 
%% 		     0:?LOC_OFFSET, 
%% 		     0:?LOC_COUNT, 
%% 		     Rest/binary 
%% 		  >>, 
%% 		  <<
%% 		    Timestamp:?TIMESTAMP, 
%% 		    Timestamps/binary
%% 		  >>) ->
%%     locations_to_list(Rest, Timestamps);
locations_to_list(<<
		    Offset:?LOC_OFFSET,
		    Count:?LOC_COUNT,
		    Rest/binary
		  >>, 
		  <<
		    Timestamp:?TIMESTAMP,
		    Timestamps/binary
		  >>) ->
    [{Offset, Count, Timestamp}|locations_to_list(Rest, Timestamps)].

load_chunks([], _CData) ->
    {ok};
load_chunks([{0,0,_CTime}|CList], CData) ->
    load_chunks(CList, CData);
load_chunks([{COffset, CSize, CTime}|CList], CData) ->
    io:format("Chunk Input : ~p ~p~n", [COffset, CSize]),
    % CData is the binary after the header
    Offset = COffset * 4096 - 8192,
    DataSize = CSize * 512,
    io:format("Chunk Seek : ~p ~p~n", [Offset+8192, DataSize*8]),
    <<
      _:Offset/binary,
      CLength:?DATA_LENGTH,
      CType:?DATA_CPTYPE,
      ZippedData:DataSize/binary,
      _/binary
    >> = CData,
    {ok, RawData} = case CType of
	2 -> zlib_read_chunk(ZippedData);
	_ -> {error, enoimpl}
    end,
    NBTData = nbt:parse_nbt(RawData),
    io:format("~p~n", [NBTData]).
%    load_chunks(CList, CData).

zlib_read_chunk(ZData) ->
    Z = zlib:open(),
    zlib:inflateInit(Z),
    Data = zlib:inflate(Z, ZData),
    zlib:close(Z),
    io:format("~p~n", [list_to_binary(Data)]),
    {ok, list_to_binary(Data)}.
