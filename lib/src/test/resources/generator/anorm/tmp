import anorm._

package io.flow.common.v0.anorm {

  object Reference {

    def parserByPrefix(prefix: String, separator: String = ".") = parser(
      guid = s"${prefix}${separator}guid"
    )

    def parser(
      guid: String
    ): RowParser[io.flow.common.v0.models.Reference] = {
      SqlParser.get[_root_.java.util.UUID](guid) map {
        case guid => {
          io.flow.common.v0.models.Reference(
            guid = guid
          )
        }
      }
    }
  }

  object Audit {

    def parserByPrefix(prefix: String, separator: String = ".") = parser(
      createdAt = s"${prefix}${separator}created_at",
      createdByPrefix = s"${prefix}${separator}created_by",
      updatedAt = s"${prefix}${separator}updated_at",
      updatedByPrefix = s"${prefix}${separator}updated_by"
    )

    def parser(
      createdAt: String,
      createdByPrefix: String,
      updatedAt: String,
      updatedByPrefix: String
    ): RowParser[io.flow.common.v0.models.Audit] = {
      SqlParser.get[_root_.org.joda.time.DateTime](createdAt) ~
      Reference.parserByPrefix(updatedByPrefix, "_") ~
      SqlParser.get[_root_.org.joda.time.DateTime](updatedAt) ~
      Reference.parserByPrefix(updatedByPrefix, "_") map {
        case createdAt ~ createdBy ~ updatedAt ~ updatedBy => {
          io.flow.common.v0.models.Audit(
            createdAt = createdAt,
            createdBy = createdBy,
            updatedAt = updatedAt,
            updatedBy = updatedBy
          )
        }
      }
    }

  }

}

package com.bryzek.dependency.v0.anorm {

  object Language {

    def parserByPrefix(prefix: String, separator: String = ".") = parser(
      guid = s"${prefix}${separator}guid",
      name = s"${prefix}${separator}name",
      auditPrefix = s"${prefix}${separator}audit"
    )

    def parser(
      guid: String = "guid",
      name: String = "name",
      auditPrefix: String = "audit"
    ): RowParser[com.bryzek.dependency.v0.models.Language] = {
      SqlParser.get[_root_.java.util.UUID](guid) ~
      SqlParser.get[String](name) ~
      io.flow.common.v0.anorm.Audit.parserByPrefix(auditPrefix, "_") map {
        case guid ~ name ~ audit => {
          com.bryzek.dependency.v0.models.Language(
            guid = guid,
            name = com.bryzek.dependency.v0.models.ProgrammingLanguage(name),
            audit = audit
          )
        }
      }
    }
  }

}
