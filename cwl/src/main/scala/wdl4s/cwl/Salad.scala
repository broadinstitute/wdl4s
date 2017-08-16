package wdl4s.cwl.other

import ammonite.ops._
import ammonite.ops.ImplicitWd._
import lenthall.validation.ErrorOr.ErrorOr
import wdl4s.cwl.{CwlCodecs, Cwl}
import cats.effect.IO
import cats.effect.IO._


object Salad extends App{

  /**
    *
    * @param in Needs to take a file to get relative file paths.
    * @return
    */
  def cwlTool(filePath: Path): IO[CommandResult] =
    IO((%%('cwltool, "--print-pre", filePath.toString)))

  def saladAndParse(in: Path):IO[CwlFile]  = {

    val relPathForEmbeddedFiles = in relativeTo pwd

    for {
      baseCwlAsJson <- cwlTool(in)
      cwlFile <- SaladDecoder.decodeSingleFileJson(baseCwlAsJson)
      //take all the files and prepend the relative path
      subFiles = cwlFile.embeddedFileNames.map(pwd/relPathForEmbeddedFiles/_)
      //convert all the sub files into cwl objects
      stringPathTosubCwlFiles <- subFiles.traverse(???)

      //modify the original CWL to show the objects instead of just strings
      //lensLookup(string).modify(_ => cwlObject)
    } yield ???
  }

  val x = 'cwl/'src/'test/'resources/"arguments.cwl"

  val y = pwd/x

  println(salad(y))
}
