package models.generator.csv

import java.io.StringWriter

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.{Operation, Resource, Service}
import lib.generator.CodeGenerator
import org.apache.commons.csv.{CSVFormat, CSVPrinter}

class CsvGenerator extends CodeGenerator {

  final val EMPTY = ""

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    Right(generateSourceFiles(form.service))
  }

  def generateSourceFiles(service: Service): Seq[File] = {
    Seq(generateResourcesFile(service.resources))
  }

  def generateResourcesFile(resources: Seq[Resource]): File = {

    def makeRow(resource: Resource, operation: Operation): Seq[String] = {

      val method = operation.method.toString
      val path = resource.path.getOrElse(EMPTY) + operation.path
      val description = operation.description.getOrElse(EMPTY)

      Seq(method, path, description)
    }

    val rows: Seq[Seq[String]] = for {
      resource <- resources
      operation <- resource.operations
    } yield makeRow(resource, operation)


    generateSourceFile("resources.csv", Seq("method", "path", "description"), rows)
  }

  def generateSourceFile(fileName: String, header: Seq[String], rows: Seq[Seq[String]]): File = {

    val stringWriter = new StringWriter
    val csvPrinter = new CSVPrinter(stringWriter, CSVFormat.DEFAULT)

    assert(rows.headOption.forall{row =>
      row.length == header.length
    }, "All lines must have same number of fields as header line")

    csvPrinter.printRecords(header.toArray)
    rows.map(row => csvPrinter.printRecords(row.toArray))
    csvPrinter.flush()

    File(
      name = fileName,
      contents = stringWriter.toString
    )
  }

}

object CsvGenerator extends CsvGenerator