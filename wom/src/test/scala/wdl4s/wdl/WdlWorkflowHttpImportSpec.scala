package wdl4s.wdl

import org.mockserver.integration.ClientAndServer
import org.mockserver.integration.ClientAndServer.startClientAndServer
import org.mockserver.model.HttpRequest.request
import org.mockserver.model.HttpResponse.response
import org.mockserver.socket.PortFactory
import wdl.{ImportResolver, WdlNamespace, WdlNamespaceWithWorkflow}
import org.scalatest.{FlatSpec, Matchers, BeforeAndAfterAll }

class WdlWorkflowHttpImportSpec extends FlatSpec with BeforeAndAfterAll with Matchers  {
  val tinyImport =
    s"""
       |task hello {
       |  command {
       |    echo "Hello World!"
       |  }
       |}
       |
       |workflow wf_hello {
       |  call hello
       |}
       |
     """.stripMargin

  def tinyWorkflow(imp:String) =
    s"""
       | import "$imp" as imp
       |
       | workflow test_import {
       |     call imp.hello
       | }
     """.stripMargin

  val httpResolver: Seq[ImportResolver] = Seq(WdlNamespace.httpResolver)

  var mockServer: ClientAndServer = _

  override def beforeAll() = {
    mockServer = startClientAndServer(PortFactory.findFreePort())

    mockServer.when(
      request().withPath("/hello.wdl")
    ).respond(
      response().withStatusCode(200).withBody(tinyImport)
    )

    mockServer.when(
      request().withPath("/protected.wdl").withHeader("Authorization", "Bearer my-token-value")
    ).
      respond(
      response().withStatusCode(200).withBody(tinyImport)
    )

    mockServer.when(
      request().withPath("/redirect.wdl")
    ).respond(
      response().withStatusCode(301).withHeader("Location","/hello.wdl"))

    mockServer.when(
      request().withPath("/none.wdl")
    ).respond(
      response().withStatusCode(404)
    )

    () // explicitly return unit
  }

  override def afterAll() = {
    mockServer.stop()
    () // explicitly return unit
  }


  "The httpResolver" should "not resolve an invalid URL " in {
    val wf = tinyWorkflow("ht://foobar")
    val ns = WdlNamespaceWithWorkflow.load(wf, httpResolver)
    ns.isFailure shouldBe true
  }

  it should "resolve an http URL" in {
    val wf = tinyWorkflow( "http://localhost:" + mockServer.getPort + "/hello.wdl")
    val ns = WdlNamespaceWithWorkflow.load(wf, httpResolver)
    ns.isFailure shouldBe false
  }

  it should "fail with a 404" in {
    val wf = tinyWorkflow( "http://localhost:" + mockServer.getPort + "/none.wdl")
    val ns = WdlNamespaceWithWorkflow.load(wf, httpResolver)
    ns.isFailure shouldBe true
  }

  it should "follow a redirect" in {
    val wf = tinyWorkflow( "http://localhost:" + mockServer.getPort + "/redirect.wdl")
    val ns = WdlNamespaceWithWorkflow.load(wf, httpResolver)
    ns.isFailure shouldBe false
  }

  it should "be able to supply a bearer token to a protected resource" in {
    val auth = Map("Authorization" -> "Bearer my-token-value")
    val authHttpResolver : Seq[ImportResolver] = Seq(WdlNamespace.httpResolverWithHeaders(auth))

    val wf = tinyWorkflow( "http://localhost:" + mockServer.getPort + "/protected.wdl")
    val ns = WdlNamespaceWithWorkflow.load(wf, authHttpResolver)
    ns.isFailure shouldBe false
  }

}
