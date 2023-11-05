# Adi Chat

This is a full fledged chat web application including the web UI, server and database integration.

The server side is built using Erlang/OTP with  [NovaFramework](http://novaframework.org/) (Check out my previous 
[article](https://github.com/sanzor/NovaWebApiTutorial) on how to build web apps with Nova)!

Persistence is done using PostgreSQL

The web UI is built using plain javascrirpt,

**Chat application supports**:
- user subscriptions to multiple channels
- multiple sessions for the same user
- fault tolerance (application is distributed across multiple nodes)
- load balancing
