package generator

import com.gilt.apidoc.generator.models.Generator

case class CodeGenTarget(metaData: Generator, status: Status, codeGenerator: Option[CodeGenerator])

