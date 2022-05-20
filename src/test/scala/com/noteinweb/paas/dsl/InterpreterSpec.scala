package com.noteinweb.paas.dsl

import com.noteinweb.paas.cloud.CloudTypes.AWS
import com.noteinweb.paas.cloud.act.CloudActor
import com.noteinweb.paas.cloud.transform.CloudTransformer
import com.noteinweb.paas.dsl.interpreter.ASTInterpreterImpl
import com.noteinweb.paas.dsl.parser.{AndThen, AppService, Bucket, CallService, Choice, Cloud, Container, Context, DocumentTable, Equals, Exit, IfThen, KubernetesService, Lambda, OtherwiseThen, Queue, ReadInput, RelationalTable, Resource, User, VmInstance, Vpc}
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.{any, anyList}
import org.mockito.Mockito.{verify, when}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar.mock

class InterpreterSpec extends FlatSpec with Matchers{
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
  val actors = List(mock[CloudActor])
  val mockAwsCloudTransformer = mock[CloudTransformer]
  when(mockAwsCloudTransformer.getCloudType).thenReturn(AWS)
  when(mockAwsCloudTransformer.deploy(any(classOf[List[com.noteinweb.paas.cloud.Resource]]))).thenReturn(List())
  val transformers = List(mockAwsCloudTransformer)

  new ASTInterpreterImpl(transformers, actors).run(successfulAST)

  val resourceListCaptor = ArgumentCaptor.forClass(classOf[List[com.noteinweb.paas.cloud.Resource]])
  verify(transformers(0)).deploy(resourceListCaptor.capture())
  assert (List(com.noteinweb.paas.cloud.Resource("attachments", "attachments", "bucket", Map.empty, List.empty)) ==
    resourceListCaptor.getValue)
}
