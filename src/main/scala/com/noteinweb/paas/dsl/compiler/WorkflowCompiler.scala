package com.noteinweb.paas.dsl.compiler

import com.noteinweb.paas.dsl.lexer.WorkflowLexer
import com.noteinweb.paas.dsl.parser.{WorkflowAST, WorkflowParser}

object WorkflowCompiler {
def apply(code: String): Either[WorkflowCompilationError, WorkflowAST] = {
  for {
    tokens <- WorkflowLexer(code).right
    ast <- WorkflowParser(tokens).right
  } yield ast
}
}
