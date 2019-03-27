Postman Collection Format v2.1 Generator
=================

## Overview
Postman Collection Generator translates ApiBuilder service.json to Postman Collection Format v2.1
which can be imported to Postman GUI.  
Every ApiBuilder Resource is mapped to Postman Collection Folder (container for API requests).  
Every ApiBuilder Operation is mapped to Postman Collection Item (API request).  
The key feature is that all required request parameters are filled with ApiBuilder-generated examples.  
This means that - for example - every POST request has its own example JSON payload assigned and it's ready to use.  
Also, sample responses (one for each expected status code) are available. Each one contains its example JSON body.

It is important to know that Postman Collection Generator requires all imported services to be present in InvocationForm.
You need to have all entity definitions at hand in order to generate example JSONs for them.

Having all imported services implies yet another feature of the Generator. It is able to generate the dependant setup calls for the Collection 
(the requests that should be ran before executing the actual requests).  
For example an user group is required to use POST /users endpoint. 
You can instruct the Generator using the right ApiBuilder attributes so the setup steps are generated automatically.

Additionally, Postman Collection Generator uses some ApiBuilder attributes to - for example - generate auth info for the Collection.

## Used attributes
Postman Collection Generator supports several ApiBuilder attributes.  
You can find the most important ones below:

**Postman Basic Auth**  
Needs to be defined on the root level of ApiBuilder specification.
Used to instruct the Generator to attach Basic Auth info to the whole Collection.  
```
{
  "name" : "postman-basic-auth",
  "value" : {
    "username" : "{{MY_TOKEN}}",
    "password" : ""
  }
}
```

**Object Reference**  
Needs to be defined on the field that is a part of ApiBuilder model.
For example it can be attached to "user-group-id" string field in order to reference an endpoint, which creates an user group.
The attribute is used to instruct the Generator to create a setup folder with the parent objects creation.
```
{
  "name": "object-reference",
  "value": {
    "relatedServiceNamespace": "io.user.group.v0",
    "resourceType": "group",
    "operationMethod": "POST",
    "identifierField": "id"
  }
}
```