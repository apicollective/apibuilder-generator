package lib.generator

import com.gilt.apidoc.generator.v0.models.Generator

case class CodeGenTarget(metaData: Generator, status: Status, codeGenerator: Option[CodeGenerator])

