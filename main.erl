-module(main).
-export([scan/2]).

scan(File1, File2) ->
  {ok, Content} = file:read_file(File1),
  Lst = binary_to_list(Content),
  {ok, Lst2, _} = lexer:string(Lst),
  {ok,Device2} = file:open(File2, write),
  {ok, Content2} = file:read_file("header.html"),
  io:format(Device2, "~s", [binary_to_list(Content2)]),
  io:format(Device2,"~s~n", ["<body>"]),
  write(Lst2, Device2),
  io:format(Device2,"~n~s~n~s~n", ["</body>", "</html>"]),
  file:close(Device2).

write([], _) -> true;
write([H | T], Device2) ->
  {_, Str, _} = H,
  io:format(Device2, "~s", [Str]), write(T, Device2).