package models.generator.kotlin.mock

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import generator.ServiceFileNames
import io.apibuilder.spec.v0.models.Service
import lib.Text._
import lib.generator.CodeGenerator

class MockClientGenerator(ssd: Service,
                          userAgent: Option[String])