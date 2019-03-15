package models.operation

import io.apibuilder.spec.v0.models.Operation

case class DependantOperations(referencedOperation: Operation, deleteOperationOpt: Option[Operation])
