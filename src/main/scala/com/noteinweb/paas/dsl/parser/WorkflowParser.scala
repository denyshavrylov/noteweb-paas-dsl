package com.noteinweb.paas.dsl.parser

import com.noteinweb.paas.dsl.compiler.{Location, WorkflowParserError}
import com.noteinweb.paas.dsl.lexer.{APPSERVICE, ARROW, BUCKET, CALLSERVICE, CLOUD, COLON, COMMA, CONTAINER, CONTEXT, DEDENT, DOCUMENTTABLE, EQUALS, EXIT, FROM, IDENTIFIER, IMAGE, INDENT, KUBERNETESSERVICE, LAMBDA, LITERAL, OTHERWISE, QUEUE, READINPUT, RELATIONALTABLE, RESOURCE, SWITCH, TYPE, URL, USER, VM, VPC, WorkflowToken}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object WorkflowParser extends Parsers {
  override type Elem = WorkflowToken

  class WorkflowTokenReader(tokens: Seq[WorkflowToken]) extends Reader[WorkflowToken] {
    override def first : WorkflowToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos : Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[WorkflowToken] = new WorkflowTokenReader(tokens.tail)
  }

  def apply(tokens: Seq[WorkflowToken]): Either[WorkflowParserError, WorkflowAST] = {
    val reader = new WorkflowTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(WorkflowParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def program: Parser[WorkflowAST] = positioned {
    phrase(block)
  }

  def block: Parser[WorkflowAST] = positioned {
    rep1(statement) ^^ { case stmtList => stmtList reduceRight AndThen }
  }

  def statement: Parser[WorkflowAST] = positioned {
    val exit = EXIT() ^^ (_ => Exit)
    val readInput = READINPUT() ~ rep(identifier ~ COMMA()) ~ identifier ^^ {
      case read ~ inputs ~ IDENTIFIER(lastInput) => ReadInput(inputs.map(_._1.str) ++ List(lastInput))
    }
    val callService = CALLSERVICE() ~ literal ^^ {
      case call ~ LITERAL(serviceName) => CallService(serviceName)
    }
    val switch = SWITCH() ~ COLON() ~ INDENT() ~ rep1(ifThen) ~ opt(otherwiseThen) ~ DEDENT() ^^ {
      case _ ~ _ ~ _ ~ ifs ~ otherwise ~ _ => Choice(ifs ++ otherwise)
    }
    val context = CONTEXT() ~ literal ~ INDENT() ~ contextBlock ~ DEDENT() ^^ {
      case _ ~ LITERAL(contextName) ~ _ ~ contextBlock ~ _ => Context(contextName, contextBlock)
    }
    exit | readInput | callService | switch | context
  }

  def contextBlock: Parser[WorkflowAST] = positioned {
    rep(contextStatement) ^^ { case stmtList => stmtList reduceRight AndThen }
  }

  def contextStatement: Parser[WorkflowAST] = positioned {
    val cloud = CLOUD() ~ literal ~ TYPE() ~ literal ^^ {
      case _ ~ LITERAL(cloudName) ~ TYPE() ~ LITERAL(cloudType) => Cloud(cloudName, cloudType, true)
    }
    val resource = RESOURCE() ~ literal ~ TYPE() ~ literal ~ URL() ~ literal ^^ {
      case _ ~ LITERAL(resourceName) ~ TYPE() ~ LITERAL(resourceType) ~ URL() ~ LITERAL(url)
        => Resource(resourceName, resourceType, url)
    }
    val relationalTable = RELATIONALTABLE() ~ literal ~ FROM() ~ literal ^^ {
      case _ ~ LITERAL(tableName) ~ FROM() ~ LITERAL(resourceName) => RelationalTable(tableName, resourceName)
    }
    val documentTable = DOCUMENTTABLE() ~ literal ~ FROM() ~ literal ^^ {
      case _ ~ LITERAL(tableName) ~ FROM() ~ LITERAL(resourceName) => DocumentTable(tableName, resourceName)
    }
    val bucket = BUCKET() ~ literal ^^ {
      case _ ~ LITERAL(bucketName) => Bucket(bucketName)
    }
    val user = USER() ~ literal ^^ {
      case _ ~ LITERAL(bucketName) => User(bucketName)
    }
    val vm = VM() ~ literal ~ TYPE() ~ literal ~ IMAGE() ~ literal ^^ {
      case _ ~ LITERAL(vmName) ~ TYPE() ~ LITERAL(vmType) ~ IMAGE() ~ LITERAL(image) => VmInstance(vmName, vmType, image)
    }
    val vpc = VPC() ~ literal ^^ {
      case _ ~ LITERAL(name) => Vpc(name)
    }
    val container = CONTAINER() ~ literal ~ IMAGE() ~ literal ^^ {
      case _ ~ LITERAL(name) ~ IMAGE() ~ LITERAL(image) => Container(name, image)
    }
    val kubernetesService = KUBERNETESSERVICE() ~ literal ~ IMAGE() ~ literal ^^ {
      case _ ~ LITERAL(name) ~ IMAGE() ~ LITERAL(image) => KubernetesService(name, image)
    }
    val appService = APPSERVICE() ~ literal ~ IMAGE() ~ literal ^^ {
      case _ ~ LITERAL(name) ~ IMAGE() ~ LITERAL(image) => AppService(name, image)
    }
    val lambda = LAMBDA() ~ literal ~ IMAGE() ~ literal ^^ {
      case _ ~ LITERAL(name) ~ IMAGE() ~ LITERAL(image) => Lambda(name, image)
    }
    val queue = QUEUE() ~ literal ~ TYPE() ~ literal ^^ {
      case _ ~ LITERAL(name) ~ TYPE() ~ LITERAL(typeName) => Queue(name, typeName)
    }
    cloud | resource | relationalTable | documentTable | bucket | user | vm | vpc | container | kubernetesService |
      appService | queue | lambda
  }

  def ifThen: Parser[IfThen] = positioned {
    (condition ~ ARROW() ~ INDENT() ~ block ~ DEDENT()) ^^ {
      case cond ~ _ ~ _ ~ block ~ _ => IfThen(cond, block)
    }
  }

  def otherwiseThen: Parser[OtherwiseThen] = positioned {
    (OTHERWISE() ~ ARROW() ~ INDENT() ~ block ~ DEDENT()) ^^ {
      case _ ~ _ ~ _ ~ block ~ _ => OtherwiseThen(block)
    }
  }

  def condition: Parser[Equals] = positioned {
    (identifier ~ EQUALS() ~ literal) ^^ { case IDENTIFIER(id) ~ eq ~ LITERAL(lit) => Equals(id, lit) }
  }

  private def identifier: Parser[IDENTIFIER] = positioned {
    accept("identifier", { case id @ IDENTIFIER(name) => id })
  }

  private def literal: Parser[LITERAL] = positioned {
    accept("string literal", {case lit @ LITERAL(name) => lit })
  }
}
