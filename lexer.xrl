% Para el lexer

Definitions.

Digit	= [0-9]
Letter	= [a-zA-Z]
Letter_ = ({Letter}|_)
LT = [<]
GT = [>]
AS = [=]
LP = [(]
RP = [)]
LB = [{]
RB = [}]
LSB = [[]
RSB = []]
DOL = [$]
PUN = [#]
SC = [;]
DC = [:]
COMMA = [,]
DM = ["]
SM = [']
VIR = [~]
SLASH = [/] 
ISLASH = [\]
PIPE = [|]
AMP = [&]
SIGN_EX = [!]
DOT = [.]
PLUS = [+]
MINUS = [-]
MULT = [*]
MOD = [%]
EXP = [^]
DIV = ({SLASH})
SW_SPACE = [\s]
SW_NEWLINE = [\n]
SW_TAB = [\t]
SW_CARRIAGE = [\r]

% | " | ' |
MARKS = ({DM}|{SM})

% | = | += | -= | *= | /= | %= |
Assignation = ({AS}|{PLUS}{AS}|{MINUS}{AS}|{MULT}{AS}|{DIV}{AS}|{MOD}{AS})

% | // |
Comment = ({SLASH}{SLASH})

% | + | - | * | / | % |
MathOp = ({PLUS}|{MINUS}|{MULT}|{DIV}|{MOD})

% | < | > | <= | >= | == | || | && | != |
Comparations = ({LT}|{GT}|{LT}{AS}|{GT}{AS}|{AS}{AS}|{PIPE}{PIPE}|{AMP}{AMP}|{SIGN_EX}{AS})

% | ( | ) | { | } | [ | ] | # | ; | : | . | " | ' | | | , | & | $ | ! | ~ | \ | ^ |
Specials = ({LP}|{RP}|{LB}|{RB}|{LSB}|{RSB}|{PUN}|{SC}|{DC}|{DOT}|{MARKS}|{PIPE}|{COMMA}|{AMP}|{DOL}|{SIGN_EX}|{VIR}|{ISLASH}|{EXP})

Rules.

{Letter_}({Letter_}|{Digit})*   :
    Atom = list_to_atom(TokenChars),
    {token,
        case reserved_word(Atom) of
            yellow -> {'RW', "<span class=\"RW\">" ++ TokenChars ++ "</span>", TokenLine};
            green -> {'TYPE_VARIABLES', "<span class=\"TYPE_VARIABLES\">" ++ TokenChars ++ "</span>", TokenLine};
            persian_green -> {'EXT_TYPE_VARIABLES', "<span class=\"EXT_TYPE_VARIABLES\">" ++ TokenChars ++ "</span>", TokenLine};
            pink -> {'CONDITIONALS', "<span class=\"CONDITIONALS\">" ++ TokenChars ++ "</span>", TokenLine};
            sapphire -> {'VARIABLES', "<span class=\"VARIABLES\">" ++ TokenChars ++ "</span>", TokenLine};
            purple -> {'CICLES', "<span class=\"CICLES\">" ++ TokenChars ++ "</span>", TokenLine};
            darkblue -> {'ID', "<span class=\"ID\">" ++ TokenChars ++ "</span>", TokenLine}
        end}.

% Token de enteros
{Digit}+ : {token, {'INTEGER', "<span class=\"INT\">" ++ TokenChars ++"</span>", TokenLine}}.

% Token de flotantes
{Digit}+{DOT}{Digit}+ : {token, {'FLOAT', "<span class=\"FLOAT\">" ++ TokenChars ++"</span>", TokenLine}}.

% Token de strings
{MARKS}{Letter_}*{MARKS} : {token, {'STRING', "<span class=\"STRING\">" ++ TokenChars ++"</span>", TokenLine}}.

% Token de char
{SM}{Letter}{SM} : {token, {'CHAR', "<span class=\"CHAR\">" ++ TokenChars ++"</span>", TokenLine}}.

% Token de <librerias>
{LT}{MARKS}?({Letter_}|{Digit})*({DOT}{Letter_}*)?{MARKS}?{GT} : {token, {'LIBRARY', "<span class=\"LIBRARY\">" ++ TokenChars ++"</span>", TokenLine}}.

% Token de define
{PUN}{Letter_}+ : {token, {'DEFINE', "<span class=\"DEFINE\">" ++ TokenChars ++"</span>", TokenLine}}.

% Token de operaciones relacionales
{Comparations} : {token, {'COMPARATIONS', "<span class=\"COMPARATIONS\">" ++ TokenChars ++"</span>", TokenLine}}.

% Token de operaciones matemáticas
{MathOp} : {token, {'MATH_OPERATORS', "<span class=\"MATH_OPERATORS\">" ++ TokenChars ++"</span>", TokenLine}}.

% Token de signos especiales
{Specials} : {token, {'SPECIALS', "<span class=\"SPECIALS\">" ++ TokenChars ++"</span>", TokenLine}}.

% Token de asignación
{Assignation} : {token, {'ASSIGNATION', "<span class=\"ASSIGNATION\">" ++ TokenChars ++"</span>", TokenLine}}.

% Token de comentarios
{Comment}({Letter_}|{Digit}|{Comparations}|{MathOp}|{Specials}|{Assignation}|{SW_SPACE})* : {token, {'COMMENT', "<span class=\"COMMENT\">" ++ TokenChars ++"</span>", TokenLine}}.

% Tokens de SW
{SW_SPACE} : {token, {'SW_SPACE', "&nbsp;", TokenLine}}.
{SW_NEWLINE} : {token, {'SW_NEWLINE', "\n<br>", TokenLine}}.
{SW_TAB} : {token, {'WS_TAB', "\t&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", TokenLine}}.
{SW_CARRIAGE}		:	{token, {'WS_CARRIAGE', "\r", TokenLine}}.

Erlang code.
-export([reserved_word/1]).

% Tipos de variable
reserved_word('int')-> green;
reserved_word('float')-> green;
reserved_word('double')-> green;
reserved_word('bool')-> green;
reserved_word('char')-> green;
reserved_word('string')-> green;
reserved_word('true')-> green;
reserved_word('false')-> green;

% Variables
reserved_word('void')-> sapphire;
reserved_word('const')-> sapphire;
reserved_word('static')-> sapphire;
reserved_word('struct')-> sapphire;
reserved_word('enum')-> sapphire;
reserved_word('array')-> sapphire;

% Extensión de memoria para variables
reserved_word('short')-> persian_green;
reserved_word('long')-> persian_green;
reserved_word('signed')-> persian_green;
reserved_word('unsigned')-> persian_green;

% Condicionales
reserved_word('if')-> pink;
reserved_word('else')-> pink;
reserved_word('switch')-> pink;
reserved_word('case')-> pink;
reserved_word('continue')-> pink;
reserved_word('break')-> pink;
reserved_word('default')-> pink;

% Ciclos
reserved_word('for')-> purple;
reserved_word('auto')-> purple;
reserved_word('while')-> purple;
reserved_word('do')-> purple;

% Palabras reservadas
reserved_word('include')-> yellow;
reserved_word('define')-> yellow;
reserved_word('main')-> yellow;
reserved_word('goto')-> yellow;
reserved_word('sizeof')-> yellow;
reserved_word('return')-> yellow;
reserved_word('exit')-> yellow;

% IDS/Variables
reserved_word(_)-> darkblue.