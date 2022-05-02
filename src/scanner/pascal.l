%option case-insensitive
%option noyywrap
%option yylineno
%x BLOCK_COMMENT
%{
#include"pascal.tab.hpp"
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

{LETTER}({LETTER}|{DIGIT})* {}


[ \t\r\n] {}
. { return yytext[0]; }

%%

void void yyerror(const char *s)
{
    fprintf(stderr, "Error: %s at (Line %d, Column%d)\n", s, yylloc.first_line, yylloc.first_column);
}
