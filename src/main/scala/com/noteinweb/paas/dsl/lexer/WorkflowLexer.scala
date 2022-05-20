package com.noteinweb.paas.dsl.lexer

import com.noteinweb.paas.dsl.compiler.{Location, WorkflowLexerError}

import scala.util.parsing.combinator.RegexParsers

object WorkflowLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "([ \t\r\f]+)|(#.*)".r

  def apply(code: String): Either[WorkflowLexerError, List[WorkflowToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(WorkflowLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }
  def tokens: Parser[List[WorkflowToken]] = {
    phrase(rep1(exit | context |
        cloud | resource | relationalTable | documentTable | bucket | user | vm | vpc | container |
        kubernetesService | appService | lambda | queue | from | typeAttr | image | url
      | readInput | callService | switch | otherwise | colon | arrow |
      equals | comma | literal | identifier | indentation  )) ^^ { rawTokens =>
      processIndentations(rawTokens)
    }
  }

  private def processIndentations(tokens: List[WorkflowToken],
                                  indents: List[Int] = List(0)) : List[WorkflowToken] = {
    tokens.headOption match {
      case Some(INDENTATION(spaces)) if spaces > indents.head =>
        INDENT() :: processIndentations(tokens.tail, spaces :: indents)

      case Some(INDENTATION(spaces)) if spaces < indents.head =>
        val (dropped, kept) = indents.partition( _ > spaces)
        (dropped map (_ => DEDENT())) ::: processIndentations(tokens.tail, kept)

      case Some(INDENTATION(spaces)) if spaces == indents.head =>
        processIndentations(tokens.tail, indents)

      case Some(token) =>
        token :: processIndentations(tokens.tail, indents)

      case None =>
        indents.filter(_ > 0).map(_ => DEDENT())
    }
  }

  def identifier: Parser[IDENTIFIER] = positioned {
    "[a-zA-Z][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
  }

  def literal: Parser[LITERAL] = positioned {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      LITERAL(content)
    }
  }

  def indentation: Parser[INDENTATION] = positioned {
    "\n[ ]*".r ^^ { whitespace =>
      val nSpaces = whitespace.length - 1
      INDENTATION(nSpaces)
    }
  }

  def exit              = positioned { "exit"         ^^ (_ => EXIT()) }
  def readInput         = positioned { "read input"   ^^ (_ => READINPUT()) }
  def callService       = positioned { "call service" ^^ (_ => CALLSERVICE()) }
  def switch            = positioned { "switch"       ^^ (_ => SWITCH()) }
  def otherwise         = positioned { "otherwise"    ^^ (_ => OTHERWISE()) }
  def colon             = positioned { ":"            ^^ (_ => COLON()) }
  def arrow             = positioned { "->"           ^^ (_ => ARROW()) }
  def equals            = positioned { "=="           ^^ (_ => EQUALS()) }
  def comma             = positioned { ","            ^^ (_ => COMMA()) }

  def context           = positioned { "context"      ^^ (_ => CONTEXT()) }
  def cloud             = positioned { "cloud"        ^^ (_ => CLOUD()) }
  def resource          = positioned { "resource"     ^^ (_ => RESOURCE()) }
  def relationalTable   = positioned { "relational table"     ^^ (_ => RELATIONALTABLE()) }
  def documentTable     = positioned { "document table"       ^^ (_ => DOCUMENTTABLE()) }
  def bucket            = positioned { "bucket"       ^^ (_ => BUCKET()) }
  def user              = positioned { "user"         ^^ (_ => USER()) }
  def vm                = positioned { "vm"           ^^ (_ => VM()) }
  def vpc               = positioned { "vpc"          ^^ (_ => VPC()) }
  def container         = positioned { "container"    ^^ (_ => CONTAINER()) }
  def kubernetesService = positioned { "kubernetes service"   ^^ (_ => KUBERNETESSERVICE()) }
  def appService        = positioned { "app service"          ^^ (_ => APPSERVICE()) }
  def lambda            = positioned { "lambda"       ^^ (_ => LAMBDA()) }
  def queue             = positioned { "queue"        ^^ (_ => QUEUE()) }
  def typeAttr          = positioned { "type"         ^^ (_ => TYPE()) }
  def from              = positioned { "from"         ^^ (_ => FROM()) }
  def image             = positioned { "image"        ^^ (_ => IMAGE()) }
  def url               = positioned { "url"            ^^ (_ => URL()) }
}
