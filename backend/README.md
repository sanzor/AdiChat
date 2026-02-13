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

**Start project (testing)**


Prerequisites:
- [vscode live server extensions](https://github.com/ritwickdey/vscode-live-server-plus-plus) to run the frontend server
- PostgreSQL  up and running , running the sql scripts located at `./resources/tables.pgsql` on the target database
- change the `./src/wsapp.app.src`  section for key `pg2` as to your local configuration: 
 `{pg2,[
    {hostname,"localhost"},
    {port,30762},
    {username,"admin"},
    {password,"test123"},
    {database,"postgresdb"}
   ]}`

Run 
- run the script in the root of the folder `script.sh` 
- go to `ui/html/chat.html` and right click -> `Open with live server`
