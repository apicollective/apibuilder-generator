package lib.generator

import io.apibuilder.generator.v0.models.Generator

case class CodeGenTarget(metaData: Generator, status: Status, codeGenerator: Option[CodeGenerator])

