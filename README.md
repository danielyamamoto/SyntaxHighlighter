To build the code, write the next lines in the Erlang console:
1. leex:file("lexer.xrl").
2. c(main).
3. c(lexer).
4. main:secuencial(["1","2","3","4","5","6","7","8","9"]).
4. main:multiproceso(["1","2","3","4","5","6","7","8","9"]).

Open the "sal.html" file from the folder you want and the words should be in colors by groups!