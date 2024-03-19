<h1>Using Types and the Adapter Pattern</h1>

In my journeys with functional exotic languages like Haskell,F# and Erlang (although dynamic still has types), i didn't quite grok why there was such a hype for defining types for everything starting with inputs coming into your application, your domain models - classes/structures that flow through your systems as well as exiting types , the things that you send over HTTP/GRPC...etc to types that are just aliases towards a primal type.

Types taken together with the adapter pattern safeguard your core functionality (domain model) against changes from both upstream changes (the systems that send you data) and also the systems that consume your data.

Imagine a typical 3 systems scenario , where your system is in the middle and you use the same type for the input , domain model and output 

![Original System](image-2.png)

<h2>Upstream adds new field</h2>
When the upstream system changes and adds new field , your are forced to add it to your database and trail it around uselessly inside your system , and perhaps even in the downstream system

![Upstream Adds new field](image.png)

<h2>Upstream deletes a field </h2>
When the upstream system deletes a field , then you have to change your model from your system and probably initialize it as a null , insert the null in the database and keep it around without purpose.

![Upstream removes field](image-3.png)

- Now on top of these cases, imagine the downstream system also changes , it might request new fields , it might delete fields or rename them and you are in for trouble

<h2>Using adapters </h2>
Separating the types of your input , output and internal system will make a world of difference in the long run and its better to start early and get entrenched with this mindset. That is why the solution is to create the so called adapter layers , or bridges, entities/classes/methods whose task is:
- transform the input of your system to an entity from your domain model
- transform the domain model of your system (the result) to an entity for your consumer/downstream system

Using adapters will keep your domain model intact, and you won't be affected by changes from external systems. You will only change the logic in your adapters
that perform the transformation.

![alt text](image-5.png)


Now i only mentioned systems , but you can also expand it to methods , especially methods that require multiple parameters , parameters that change alot. 








