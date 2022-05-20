package com.noteinweb.paas.dsl.interpreter

import com.noteinweb.paas.cloud.CloudTypes.{AWS, CloudType}
import com.noteinweb.paas.cloud.Resource
import com.noteinweb.paas.cloud.act.CloudActor
import com.noteinweb.paas.cloud.transform.CloudTransformer
import com.noteinweb.paas.dsl.parser.{AndThen, Bucket, Choice, Context, IfThen, OtherwiseThen, WorkflowAST}
import com.typesafe.scalalogging.Logger
import scala.collection.mutable
import scala.util.parsing.input.Positional

trait ASTInterpreter {
  def run(ast: WorkflowAST): RunResult
}

class ASTInterpreterImpl(transformers: List[CloudTransformer], actors: List[CloudActor]) extends ASTInterpreter {
  protected val logger = Logger(getClass)
  private val transformerMap = mutable.HashMap.empty[CloudType, CloudTransformer]
  private val actorMap = mutable.HashMap.empty[CloudType, CloudActor]

  transformers.foreach(transformer => transformerMap.put(transformer.getCloudType, transformer))
  actors.foreach(actor => actorMap.put(actor.getCloudType, actor))

  def run(ast: WorkflowAST): RunResult = {

    def astWalker(ast: WorkflowAST, resourcesByCloud: mutable.Map[CloudType, mutable.ListBuffer[Resource]]) {
      ast match {
        case _: AndThen => {
          logger.trace("AndThen:")
          astWalker(ast.asInstanceOf[AndThen].step1, resourcesByCloud)
          astWalker(ast.asInstanceOf[AndThen].step2, resourcesByCloud)
        }
        case _: Context => {
          logger.trace("Context:")
          astWalker(ast.asInstanceOf[Context].resources, resourcesByCloud)
        }
        case _: IfThen => {
          logger.trace("IfThen: ", ast.asInstanceOf[IfThen].predicate)
          astWalker(ast.asInstanceOf[IfThen].thenBlock, resourcesByCloud)
        }
        case _: OtherwiseThen => {
          logger.trace("OtherwiseThen: ")
          astWalker(ast.asInstanceOf[OtherwiseThen].thenBlock, resourcesByCloud)
        }
        case _: Choice => {
          logger.trace("Choice: ")
          ast.asInstanceOf[Choice].alternatives.foreach(conditionThen => astWalker(conditionThen.thenBlock, resourcesByCloud))
        }

        case _: Bucket => {
          val bucket = ast.asInstanceOf[Bucket]
          val resource: Resource = Resource(bucket.name, bucket.name, "bucket", Map.empty, List.empty)
          val cloudResources : mutable.ListBuffer[Resource] = resourcesByCloud.getOrElseUpdate(AWS, mutable.ListBuffer[Resource]())
          cloudResources.append(resource)
        }
        case allOthers => logger.trace("some other stmt: ", allOthers)
      }
    }
    val resourcesByCloud: mutable.Map[CloudType, mutable.ListBuffer[Resource]] = mutable.HashMap()
    astWalker(ast, resourcesByCloud)
    val result = resourcesByCloud.keys.flatMap(cloud =>
      getTransformer4ContextCloud(cloud.toString).map(cloudTransformer => {
        logger.trace(s"Running $cloudTransformer .deploy")
        cloudTransformer.deploy(
          resourcesByCloud(cloud).toList)
      })
    )
    RunResult(result.flatten[String])
  }

  def getTransformer4ContextCloud(cloudTypeName: String) : Option[CloudTransformer] = {
    transformerMap.get(AWS)
  }
}

case class RunResult(output: Iterable[String])
case class RunError() extends Positional
