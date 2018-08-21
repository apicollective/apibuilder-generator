Code generator to create POJOs compatible with AWS Lambdas
=====

This generator is intended for use with AWS Lambdas, it will create models that are POJOs,
or "plain old java objects", meaning they do not implement any interfaces and they have
at least one default constructor that takes no arguments and all fields have a public setter
and public getter methods.

DynamoDB Annotation
------
Additionally this generator is designed to interoperate with Amazon's DynamoDB. As such it can
generate the required annotatations for a model to be used as a DynamoDb record. These include
@DynamoDBTable, @DynamoDBDocument, @DynamoDBHashKey, and @DynamoDBRangeKey.

To specify that a particular model corresponds to a DynamoDB Table add the following attribute
to the model's attributes list:

```
{
    "name": "DynamoDBTable",
    "value": {
        "tableName": "[YOUR-TABLE-NAME-HERE]"
    }
}
```

To specify that a particular model is merely a DynamoDBDocument (which should not correspond to a
particular table, but rather should just be treated as a binary or "blob" object belonging to a
table record, include this attribute with an empty JSON value:

```
"attributes": [
{
    "name": "DynamoDBDocument",
    "value": {

    }
}
```

If either of these first two attributes are present for a model all of its field will be annotated as
@DynamoDBAttribute unless otherwise specified to be DynamoDBRangeKey or DynamoDBHashKey arguments.

For those arguments you need to include one of the following attributes in their attribute list:

```
{
    "name": "DynamoDBHashKey",
    "value": {
        "attributeName": "[YOUR-ATTRIBUTE-NAME-HERE]"
    }
}
```

```
{
    "name": "DynamoDBRangeKey",
    "value": {
        "attributeName": "[YOUR-ATTRIBUTE-NAME-HERE]"
    }
}
```