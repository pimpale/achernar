#include "parser.h"

#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "token.h"
#include "vector.h"

Parser *createParserLexer(Parser *parser, Lexer *lexer) {
  parser->backing = ParserBackingLexer;
  parser->lex.lexer = lexer;
  parser->lex.loc = 0;
  createVector(&parser->lex.tokVec);
  parser->dl = lexer->dl;
  return parser;
}

Parser *createParserMemory(Parser *parser, DiagnosticLogger *dl, Token *tokens,
                           size_t tokenCount) {
  parser->backing = ParserBackingMemory;
  parser->mem.ptr = malloc(tokenCount * sizeof(Token));
  memcpy(parser->mem.ptr, tokens, tokenCount * sizeof(Token));
  parser->mem.len = tokenCount;
  parser->mem.loc = 0;
  parser->dl = dl;
  return parser;
}

// TODO also have to destroy the nested tokens
Parser *destroyParser(Parser *parser) {
  switch (parser->backing) {
  case ParserBackingMemory: {
    free(parser->mem.ptr);
    break;
  }
  case ParserBackingLexer: {
    destroyVector(&parser->lex.tokVec);
    break;
  }
  }
  return parser;
}

static ResultTokenPtr advanceParser(Parser *p) {
  switch (p->backing) {
  case ParserBackingLexer: {
    if (p->lex.loc < VEC_LEN(&p->lex.tokVec, Token)) {
      // clang-format off
      return (ResultTokenPtr) {
        .val=VEC_GET(&p->lex.tokVec, p->lex.loc++, Token),
        .err=ErrorOk
      };
      // clang-format on
    } else {
      ResultToken ret = lexNextToken(p->lex.lexer);
      if (ret.err == ErrorOk) {
        *VEC_PUSH(&p->lex.tokVec, Token) = ret.val;
        // clang-format off
        return (ResultTokenPtr) {
          .val=VEC_GET(&p->lex.tokVec, p->lex.loc++, Token),
          .err=ErrorOk
        };
        // clang-format on
      } else {
        return (ResultTokenPtr){.err = ret.err};
      }
    }
  }
  case ParserBackingMemory: {
    if (p->mem.loc < p->mem.len) {
      // Increment and return
      // clang-format off
        return (ResultTokenPtr){
          .val = &p->mem.ptr[p->mem.loc++],
          .err = ErrorOk
        };
      // clang-format on
    } else {
      // Issue Eof Error
      return (ResultTokenPtr){.err = ErrorEOF};
    }
  }
  }
}

// Gets current token
static ResultTokenPtr peekParser(Parser *p) {
  switch (p->backing) {
  case ParserBackingLexer: {
    if (p->lex.loc < VEC_LEN(&p->lex.tokVec, Token)) {
      // clang-format off
      return (ResultTokenPtr) {
        .val=VEC_GET(&p->lex.tokVec, p->lex.loc, Token),
        .err=ErrorOk
      };
      // clang-format on
    } else {
      ResultToken ret = lexNextToken(p->lex.lexer);
      if (ret.err == ErrorOk) {
        *VEC_PUSH(&p->lex.tokVec, Token) = ret.val;
        // clang-format off
        return (ResultTokenPtr) {
          .val=VEC_GET(&p->lex.tokVec, p->lex.loc, Token),
          .err=ErrorOk
        };
        // clang-format on
      } else {
        return (ResultTokenPtr){.err = ret.err};
      }
    }
  }
  case ParserBackingMemory: {
    if (p->mem.loc < p->mem.len) {
      // Increment and return
      // clang-format off
        return (ResultTokenPtr){
          .val = &p->mem.ptr[p->mem.loc],
          .err = ErrorOk
        };
      // clang-format on
    } else {
      // Issue Eof Error
      return (ResultTokenPtr){.err = ErrorEOF};
    }
  }
  }
}

static ResultFuncDeclStmnt parseFuncDeclStmnt(Parser *p) {
  // TODO
  return (ResultFuncDeclStmnt){.err = ErrorOk};
}

static ResultStmnt parseStmnt(Parser *p) {
  ResultTokenPtr ret = peekParser(p);
  if (ret.err != ErrorOk) {
    return (ResultStmnt){.err = ret.err};
  }

  Stmnt s;
  switch (ret.val->type) {
  case TokenFunction: {
    ResultFuncDeclStmnt ret = parseFuncDeclStmnt(p);
    if (ret.err != ErrorOk) {
      // TODO probably
      return (ResultStmnt){.err = ret.err};
    }

    s.type = StmntFuncDecl;
    s.value = malloc(sizeof(FuncDeclStmnt));
    memcpy(s.value, &ret.val, sizeof(ret.val));
    // clang-format off
    return (ResultStmnt) {
      .val = s,
      .err = ErrorOk
    };
    // clang-format on
  }
  default: {
    logDiagnostic(p->dl, ErrorUnexpectedToken, ret.val->ln, ret.val->col);
    return (ResultStmnt){.err = ErrorUnexpectedToken};
  }
  }
}

ResultTranslationUnit parseTranslationUnit(Parser *p) {
  Vector data;
  createVector(&data);
  while (true) {
    ResultStmnt ret = parseStmnt(p);
    if (ret.err == ErrorEOF) {
      INTERNAL_ERROR("finished reading file!");
      break;
    } else if (ret.err != ErrorOk) {
      logInternalError(176, __func__, "TODO add error handling system: %s", "yeet");
      break;
    }
    // Only reachable if it is equal to err ok
    *VEC_PUSH(&data, Stmnt) = ret.val;
  }

  size_t length = VEC_LEN(&data, Stmnt);
  Stmnt *statements = releaseVector(&data);

  // clang-format off
  return (ResultTranslationUnit) {
    .val = (TranslationUnit) {
      .statements = statements,
      .length = length
    },
    .err = ErrorOk
  };
  // clang-format on
}
