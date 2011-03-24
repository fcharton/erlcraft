-module(mc_region).

-export([load_file/1]).

-define(LOC_OFFSET,  24/unsigned-big-integer).
-define(LOC_COUNT,    8/unsigned-big-integer).
-define(TIMESTAMP,   32/unsigned-big-integer).
-define(DATA_LENGTH, 32/unsigned-big-integer).
-define(DATA_CPTYPE,  8/unsigned-big-integer).

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
locations_to_list(<< 
		     0:?LOC_OFFSET, 
		     0:?LOC_COUNT, 
		     Rest/binary 
		  >>, 
		  <<
		    Timestamp:?TIMESTAMP, 
		    Timestamps/binary
		  >>) ->
    locations_to_list(Rest, Timestamps);
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

load_chunks([], CData) ->
    {ok};
load_chunks([{COffset, CSize, CTime}|CList], Chunks) ->
    Offset = COffset * 4096,
    <<
      _:Offset/binary,
      CLength:?DATA_LENGTH,
      CType:?DATA_CPTYPE,
      Data/binary
    >> = Chunks,
    io:format("Chunk : ~p ~p~n", [CLength, CType]),
    load_chunks(CList, Chunks).

