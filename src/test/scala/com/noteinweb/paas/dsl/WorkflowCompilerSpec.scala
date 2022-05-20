package com.noteinweb.paas.dsl

import com.noteinweb.paas.dsl.compiler.{Location, WorkflowCompiler, WorkflowParserError}
import com.noteinweb.paas.dsl.parser._
import org.scalatest.{FlatSpec, Matchers}


class WorkflowCompilerSpec extends FlatSpec with Matchers {

  val validCode =
    """
      |context "NoteWeb"
      |  cloud "NoteWebInAWS" type "AWS"
      |  resource "notesDatabase" type "mysql" url ""
      |  resource "auditDatabase" type "dynamodb" url ""
      |  relational table "notes" from "notesDatabase"
      |  relational table "tags" from "notesDatabase"
      |  document table "audit" from "auditDatabase"
      |  bucket "attachments"
      |  user "NoteWeb"
      |  vm "NoteWebSearch" type "t2.micro.search" image "urn:...::"
      |  vpc "NoteWeb"
      |  container "NoteWebPython" image "urn:...::"
      |  kubernetes service "NoteWebWildfly" image "urn:...::"
      |  app service "NoteWebNode" image "urn:...::"
      |  lambda "DirectAttachmentUpload" image "urn:...::"
      |  queue "audit" type "sqs"
      |  queue "events" type "sns"
      |read input name, country
      |switch:
      |  country == "PT" ->
      |    call service "A"
      |    exit
      |  otherwise ->
      |    call service "B"
      |    switch:
      |      name == "unknown" ->
      |        exit
      |      otherwise ->
      |        call service "C"
      |        exit
    """.stripMargin.trim

  val invalidCode =
    """
      |read input name, country
      |switch:
      |  country == PT ->
      |    call service "A"
      |    exit
      |  otherwise ->
      |    call service "B"
      |    switch:
      |      name == "unknown" ->
      |        exit
      |      otherwise ->
      |        call service "C"
      |        exit
    """.stripMargin.trim

  val successfulAST =
    AndThen(
      Context("NoteWeb",AndThen(
        Cloud("NoteWebInAWS","AWS",true),AndThen(
        Resource("notesDatabase","mysql",""),AndThen(
        Resource("auditDatabase","dynamodb",""),AndThen(
        RelationalTable("notes","notesDatabase"),AndThen(
        RelationalTable("tags","notesDatabase"),AndThen(
        DocumentTable("audit","auditDatabase"),AndThen(
        Bucket("attachments"),AndThen(
        User("NoteWeb"),AndThen(
        VmInstance("NoteWebSearch","t2.micro.search","urn:...::"),AndThen(
        Vpc("NoteWeb"),AndThen(
        Container("NoteWebPython","urn:...::"),AndThen(
        KubernetesService("NoteWebWildfly","urn:...::"),AndThen(
        AppService("NoteWebNode","urn:...::"),AndThen(
        Lambda("DirectAttachmentUpload","urn:...::"),AndThen(
        Queue("audit","sqs"),
        Queue("events","sns"))))))))))))))))),AndThen(
      ReadInput(List("name", "country")),
      Choice(List(
        IfThen(Equals("country","PT"),AndThen(
          CallService("A"),
          Exit)),
        OtherwiseThen(AndThen(
          CallService("B"),
          Choice(List(
            IfThen(Equals("name","unknown"),
            Exit),
          OtherwiseThen(AndThen(
            CallService("C"),
            Exit
          ))
        ))
      ))
    ))))

  val errorMsg = WorkflowParserError(Location(3, 14), "string literal expected")

  "Workflow compiler" should "successfully parse a valid workflow" in {
    val compilationResult = WorkflowCompiler(validCode)
    val co = compilationResult.right.get
    val crType = co.getClass.getName
    val nextStmt = co.asInstanceOf[AndThen]
    println(s"Compilation output type : $crType \n$co\nNext stmt:")
    println("step1: ", nextStmt.step1, "\nstep2: ", nextStmt.step2)
    compilationResult shouldBe Right(successfulAST)
  }


  it should "return an error with an invalid workflow" in {
    WorkflowCompiler(invalidCode) shouldBe Left(errorMsg)
  }
}
