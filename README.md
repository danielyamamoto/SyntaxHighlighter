To build the code, write the next lines in the Erlang console:
1. leex:file("lexer.xrl").
2. c(main).
3. c(lexer).
4. main:scan("main.c", "sal.html").

Open the "sal.html" file and the words should be in colors by groups!