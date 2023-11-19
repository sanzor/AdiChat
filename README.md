# Nova Websocket Server

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

**Start project (testing)**

Database
- you will need a postgres database installed
- run the sql scripts into your postgres server located at : `./resources/tables.pgsql`
- change the `wsapp.app.src` environment section for key `pg2` as to your requirements: 
 <!-- {pg2,[
    {hostname,"localhost"},
    {port,30762},
    {username,"admin"},
    {password,"test123"},
    {database,"postgresdb"}
    ]}-->

Run 
- run the script in the root of the folder `script.sh` 
- install the live server nuget for vscode 
- go to `ui/html/chat.html` and right click -> `Open with live server`
