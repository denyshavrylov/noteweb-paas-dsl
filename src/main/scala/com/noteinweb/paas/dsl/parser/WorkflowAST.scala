package com.noteinweb.paas.dsl.parser

import scala.util.parsing.input.Positional

sealed trait WorkflowAST extends Positional
case class AndThen(step1: WorkflowAST, step2: WorkflowAST) extends WorkflowAST
case class ReadInput(inputs: Seq[String]) extends WorkflowAST
case class CallService(serviceName: String) extends WorkflowAST
case class Choice(alternatives: Seq[ConditionThen]) extends WorkflowAST
case object Exit extends WorkflowAST

sealed trait ConditionThen extends Positional { def thenBlock : WorkflowAST }
case class IfThen(predicate: Condition, thenBlock: WorkflowAST) extends ConditionThen
case class OtherwiseThen(thenBlock: WorkflowAST) extends ConditionThen

sealed trait Condition extends Positional
case class Equals(factName: String, factValue: String) extends Condition

case class Context(name: String, resources: WorkflowAST) extends WorkflowAST
case class Cloud(name: String, cloudType: String, isDefault: Boolean) extends WorkflowAST
case class Resource(name: String, resourceType: String, url: String) extends WorkflowAST
case class User(name: String) extends WorkflowAST
case class Bucket(name: String) extends WorkflowAST
case class VmInstance(name: String, instanceType: String, imageUrl: String) extends WorkflowAST
case class Lambda(name: String, image: String) extends WorkflowAST
case class Service(name: String) extends WorkflowAST // BeansTalk
case class Container(name: String, url: String) extends WorkflowAST
case class KubernetesService(name: String, image: String) extends WorkflowAST
case class AppService(name: String, image: String) extends WorkflowAST
case class Vpc(name: String) extends WorkflowAST
case class Subnet(name: String) extends WorkflowAST
case class Queue(name: String, typeName: String) extends WorkflowAST
case class RelationalTable(name: String, resourceName: String) extends WorkflowAST
case class DocumentTable(name: String, resourceName: String) extends WorkflowAST
case class RelationalDatabase(name: String) extends WorkflowAST