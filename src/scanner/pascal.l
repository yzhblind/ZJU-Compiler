%option case-insensitive
%option noyywrap
%option yylineno
%x BLOCK_COMMENT
%{
#include"pascal.y.hpp"
#include<cstdio>
#include<cstring>
int yycolumn = 1;
#define YY_USER_ACTION yylloc.first_line = yylloc.last_line = yylineno; \
	yylloc.first_column = yycolumn; yylloc.last_column = yycolumn + yyleng - 1; \
    yycolumn += yyleng;

%}
LETTER [a-z]
DIGIT [0-9]
%%
    /*匹配单行注释*/
"{".*"}" {}
    /*匹配到多行注释符号"(*"时切换到注释状态, 直到遇见"*)"回到正常匹配状态*/
"(*" {BEGIN BLOCK_COMMENT;}
<BLOCK_COMMENT>"*)" {BEGIN INITIAL;}
<BLOCK_COMMENT>.|"\n" {}

"and" { return KEY_AND; }
"array" {return KEY_ARRAY; }
"begin" {return KEY_BEGIN; }
"case" {return KEY_CASE;}
"const" {return KEY_CONST;}
"div" {return KEY_DIV; }
"do" {return KEY_DO; }
"downto" {return KEY_DOWNTO; }
"else" { return KEY_ELSE; }
"end" { return KEY_END; }
"for" { return KEY_FOR; }
"forward" { return KEY_FORWARD; }
"function" { return KEY_FUNCTION; }
"if" { return KEY_IF; }
"mod" { return KEY_MOD; }
"nil" { return KEY_NIL; }
"not" { return KEY_NOT; }
"of" { return KEY_OF; }
"or" { return KEY_OR; }
"packed" { return KEY_PACKED; }
"procedure" { return KEY_PROCEDURE; }
"program" { return KEY_PROGRAM; }
"record" { return KEY_RECORD; }
"repeat" { return KEY_REPEAT; }
"result" { return KEY_RESULT; }
"then" { return KEY_THEN; }
"to" { return KEY_TO; }
"type" { return KEY_TYPE; }
"until" { return KEY_UNTIL; }
"var" { return KEY_VAR; }
"while" { return KEY_WHILE; }
"with" { return KEY_WITH; }

"+" { return OP_ADD; }
"-" { return OP_SUB; }
"*" { return OP_MUL; }
"/" { return OP_DIV; }
"=" { return OP_EQ; }
"<" { return OP_LT; }
">" { return OP_GT; }
"[" { return OP_L_BCK; }
"]" { return OP_R_BCK; }
"." { return OP_DOT; }
"," { return OP_COMMA; }
":" { return OP_COLON; }
";" { return OP_SEMICOLON; }
"^" { return OP_CARET; }
"(" { return OP_L_PRTS; }
")" { return OP_R_PRTS; }
"<>" { return OP_NOT_EQ; }
"<=" { return OP_LE; }
">=" { return OP_GE; }
":=" { return OP_ASSIGN; }
".." { return OP_DDOT; }
"@" { return OP_AT; }

"read" { return IO_READ; }
"write" { return IO_WRITE; }
"readln" { return IO_READLN; }
"writeln" { return IO_WRITELN; }

"\""[^"]+"\"" { yylval.text=strdup(yytext); return CHARACTER_STRING; }
{LETTER}({LETTER}|{DIGIT})* { yylval.text=strdup(yytext); return IDENTIFIER; }
{DIGIT}+ { yylval.int_val=atoll(yytext); return UNSIGNED_INTEGER; }
{DIGIT}+\.{DIGIT}+ {yylval.real_val=atof(yytext); return UNSIGNED_REAL; }

[ \t\r] {}
[\n] { yycolumn = 1; }
. { return yytext[0]; }

%%

void yyerror(const char *s)
{
    fprintf(stderr, "Error: %s at (Line %d, Column%d)\n", s, yylloc.first_line, yylloc.first_column);
}
