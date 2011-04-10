-module(mc_region).

-export([load_chunk/3]).

-define(LOC_OFFSET,  24/unsigned-big-integer).
-define(LOC_COUNT,    8/unsigned-big-integer).
-define(TIMESTAMP,   32/unsigned-big-integer).
-define(DATA_LENGTH, 32/signed-big-integer).
-define(DATA_CPTYPE,  8/signed-big-integer).

mod(X, Y) ->
    abs(X rem Y).

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.
%%

load_chunk(Root, X, Z) ->
    % Getting chunk group number
    CGX = floor(X/32.0),
    CGZ = floor(Z/32.0),
    % Getting position inside chunk group
    CP = 4 * (mod(X, 32) + mod(Z, 32) * 32),
    FileName = string:to_lower(
                         string:join([
                                 "r", 
                                 erlang:integer_to_list(CGX), 
                                 erlang:integer_to_list(CGZ), 
                                 "mcr"
                         ], ".")),
    Path = string:join([Root, "region", FileName], "/"),
    case file:read_file(Path) of
    {ok, Data} ->
        << Locations:4096/binary, Timestamps:4096/binary, BChunks/binary >> = Data,
        ChunksL = locations_to_list(Locations, Timestamps),
        load_chunks(ChunksL, BChunks),
	{ok, lists:nth(CP, ChunksL)};
    {error, enoent} ->
        io:format("File not found !~n"),
	{error, enofile}
    end.

locations_to_list(<<>>, _Timestamps) ->
    [];
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

load_chunks(Data, CData) ->
    load_chunks(Data, CData, 0, 0).

load_chunks([], CData,  _X, _Z) ->
    {ok};
load_chunks([{0,0,_CTime}|CList], CData, X, Z) ->
    load_chunks(CList, CData, ((X+1) rem 32), Z+trunc((X+1)/32));
load_chunks([{COffset, CSize, CTime}|CList], CData, X, Z) ->
    io:format("Chunk Input (~p/~p): ~p ~p~n", [X, Z, COffset, CSize]),
    % CData is the binary after the header
    Offset = COffset * 4096 - 8192,
    DataSize = CSize * 4096,
    %io:format("Chunk Seek : ~p ~p~n", [Offset+8192, DataSize]),
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
    NBTData = nbt:parse_nbt_data(RawData),
    load_chunks(CList, CData, ((X+1) rem 32), Z+trunc((X+1)/32)).

zlib_read_chunk(ZData) ->
    Z = zlib:open(),
    zlib:inflateInit(Z),
    Data = list_to_binary(zlib:inflate(Z, ZData)),
    Size = size(Data),
    zlib:close(Z),
    {ok, Data}.
