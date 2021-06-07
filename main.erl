-module(main).
-export([secuencial/1, multiproceso/1, crear_lista/1, utiliza_pmap/1, scan/1, scan/3, pmap/1]).

% Funciones secuencial y multiproceso
% Se llaman en la consola para ejecutar el programa
%% secuencial solo tiene un thread
%% multiproceso es de n cantidad de threads dependiendo del arreglo
secuencial(Lst) -> timer:tc(?MODULE,scan,[Lst]).
multiproceso(Lst) -> timer:tc(?MODULE,utiliza_pmap,[Lst]).

% Función utiliza_pmap con un parámetro
% Realiza la ejecución de la función crear_lista y lo guarda como lista
% Y se lo pasa como argumento a la función pmap
utiliza_pmap(Lst) -> Lst2=crear_lista(Lst), pmap(Lst2).

% Función crear_lista con un argumento
% Aquí se crea una lista de funciones lambdas de scan con tres parámetros
crear_lista([H]) -> [crear_funcion(H)];
crear_lista([H|T]) -> [crear_funcion(H)|crear_lista(T)].
crear_funcion(Dir) -> fun() -> scan(Dir, "main.c", "sal.html") end.

% Función scan con un parámetro
% Realiza la ejecución de la función scan con tres parámetros
scan([H]) -> scan(H, "main.c", "sal.html");
scan([H|T]) -> scan(H, "main.c", "sal.html"), scan(T).

% Función scan con tres parámetros
% Función principal del programa donde escribe los datos recibidos de los tokens
scan(Dir, File1, File2) ->
  {ok, Content} = file:read_file(filename:join([Dir, File1])),
  Lst = binary_to_list(Content),
  {ok, Lst2, _} = lexer:string(Lst),
  {ok,Device2} = file:open(filename:join([Dir, File2]), write),
  {ok, Content2} = file:read_file("header.html"),
  io:format(Device2, "~s", [binary_to_list(Content2)]),
  io:format(Device2,"~s~n", ["<body>"]),
  write(Lst2, Device2),
  io:format(Device2,"~n~s~n~s~n", ["</body>", "</html>"]),
  file:close(Device2).

% Funcion write con dos parámetros
% Aquí se escriben los datos obtenidos en el archivo HTML
write([], _) -> true;
write([H | T], Device2) ->
  {_, Str, _} = H,
  io:format(Device2, "~s", [Str]), write(T, Device2).

% @author ralozano
% Función pmap con un parámetro
% Aquí recibe el mensaje de los threads
pmap(L) ->
  S = self(),
  Pids = [do(S, F)  || F <- L],
  [receive {Pid, Val} -> Val end || Pid <- Pids].

% @author ralozano
% Función do con dos parámetros
% Realiza la creación de los threads
do(Parent, F) ->
  spawn(fun() ->
    Parent ! {self(), F()} end).