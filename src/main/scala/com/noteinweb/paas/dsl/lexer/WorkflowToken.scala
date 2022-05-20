package com.noteinweb.paas.dsl.lexer

import scala.util.parsing.input.Positional

sealed trait WorkflowToken extends Positional

case class IDENTIFIER(str: String) extends WorkflowToken
case class LITERAL(str: String) extends WorkflowToken
case class INDENTATION(spaces: Int) extends WorkflowToken
case class EXIT() extends WorkflowToken
case class READINPUT() extends WorkflowToken
case class CALLSERVICE() extends WorkflowToken
case class SWITCH() extends WorkflowToken
case class OTHERWISE() extends WorkflowToken
case class COLON() extends WorkflowToken
case class ARROW() extends WorkflowToken
case class EQUALS() extends WorkflowToken
case class COMMA() extends WorkflowToken
case class INDENT() extends WorkflowToken
case class DEDENT() extends WorkflowToken

case class CONTEXT() extends WorkflowToken
case class CLOUD() extends WorkflowToken
case class RESOURCE() extends WorkflowToken
case class RELATIONALTABLE() extends WorkflowToken
case class DOCUMENTTABLE() extends WorkflowToken
case class BUCKET() extends WorkflowToken
case class USER() extends WorkflowToken
case class VM() extends WorkflowToken
case class VPC() extends WorkflowToken
case class CONTAINER() extends WorkflowToken
case class KUBERNETESSERVICE() extends WorkflowToken
case class APPSERVICE() extends WorkflowToken
case class LAMBDA() extends WorkflowToken
case class QUEUE() extends WorkflowToken
case class TYPE() extends WorkflowToken
case class FROM() extends WorkflowToken
case class IMAGE() extends WorkflowToken
case class URL() extends WorkflowToken