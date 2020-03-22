/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     TOKEN_ABBREV = 258,
     TOKEN_AT = 259,
     TOKEN_COMMA = 260,
     TOKEN_COMMENT = 261,
     TOKEN_ENTRY = 262,
     TOKEN_EQUALS = 263,
     TOKEN_FIELD = 264,
     TOKEN_INCLUDE = 265,
     TOKEN_INLINE = 266,
     TOKEN_KEY = 267,
     TOKEN_LBRACE = 268,
     TOKEN_LITERAL = 269,
     TOKEN_NEWLINE = 270,
     TOKEN_PREAMBLE = 271,
     TOKEN_RBRACE = 272,
     TOKEN_SHARP = 273,
     TOKEN_SPACE = 274,
     TOKEN_STRING = 275,
     TOKEN_VALUE = 276,
     TOKEN_UNKNOWN = 277
   };
#endif
/* Tokens.  */
#define TOKEN_ABBREV 258
#define TOKEN_AT 259
#define TOKEN_COMMA 260
#define TOKEN_COMMENT 261
#define TOKEN_ENTRY 262
#define TOKEN_EQUALS 263
#define TOKEN_FIELD 264
#define TOKEN_INCLUDE 265
#define TOKEN_INLINE 266
#define TOKEN_KEY 267
#define TOKEN_LBRACE 268
#define TOKEN_LITERAL 269
#define TOKEN_NEWLINE 270
#define TOKEN_PREAMBLE 271
#define TOKEN_RBRACE 272
#define TOKEN_SHARP 273
#define TOKEN_SPACE 274
#define TOKEN_STRING 275
#define TOKEN_VALUE 276
#define TOKEN_UNKNOWN 277




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif

extern YYLTYPE yylloc;
