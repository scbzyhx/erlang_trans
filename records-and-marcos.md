# 5 记录 (Records) 与宏  (Macros)  

一个大规模程序经常有很多个文件组成，这些不同部分之间通过良好定义的接口相互联系。  

## 5.1 将大程序分在多个文件中  

为了演示需要，我们将前面几节中 messager 程序分布到五个文件中：  

- mess\_config.hrl  

     配置所需数据头文件
     
- mess\_interface.hrl  

    客户端与 messager 之间的接口定义
    
- user\_interface.erl

    用户接口函数
    
- mess\_client.erl

    messager 系统客户端的函数
    
- mess\_server.erl

	messager 服务端的函数

除了完成上述工作外，我们使用**记录**重新定义了 shell 、客户端以及服务端的消息格式。此外，我们还引入了下面这些宏：  

```
%%%----FILE mess_config.hrl----

%%% Configure the location of the server node,
-define(server_node, messenger@super).

%%%----END FILE----
```  
```
%%%----FILE mess_interface.hrl----

%%%Message interface between client and server and client shell for
%%% messenger program 

%%%Messages from Client to server received in server/1 function.
-record(logon,{client_pid, username}).
-record(message,{client_pid, to_name, message}).
%%% {'EXIT', ClientPid, Reason}  (client terminated or unreachable.

%%% Messages from Server to Client, received in await_result/0 function 
-record(abort_client,{message}).
%%% Messages are: user_exists_at_other_node, 
%%%               you_are_not_logged_on
-record(server_reply,{message}).
%%% Messages are: logged_on
%%%               receiver_not_found
%%%               sent  (Message has been sent (no guarantee)
%%% Messages from Server to Client received in client/1 function
-record(message_from,{from_name, message}).

%%% Messages from shell to Client received in client/1 function
%%% spawn(mess_client, client, [server_node(), Name])
-record(message_to,{to_name, message}).
%%% logoff

%%%----END FILE----
```
```
%%%----FILE mess_interface.hrl----

%%% Message interface between client and server and client shell for
%%% messenger program 

%%%Messages from Client to server received in server/1 function.
-record(logon,{client_pid, username}).
-record(message,{client_pid, to_name, message}).
%%% {'EXIT', ClientPid, Reason}  (client terminated or unreachable.

%%% Messages from Server to Client, received in await_result/0 function 
-record(abort_client,{message}).
%%% Messages are: user_exists_at_other_node, 
%%%               you_are_not_logged_on
-record(server_reply,{message}).
%%% Messages are: logged_on
%%%               receiver_not_found
%%%               sent  (Message has been sent (no guarantee)
%%% Messages from Server to Client received in client/1 function
-record(message_from,{from_name, message}).

%%% Messages from shell to Client received in client/1 function
%%% spawn(mess_client, client, [server_node(), Name])
-record(message_to,{to_name, message}).
%%% logoff

%%%----END FILE----
```

```
%%%----FILE mess_client.erl----

%%% The client process which runs on each user node

-module(mess_client).
-export([client/2]).
-include("mess_interface.hrl").

client(Server_Node, Name) ->
    {messenger, Server_Node} ! #logon{client_pid=self(), username=Name},
    await_result(),
    client(Server_Node).

client(Server_Node) ->
    receive
        logoff ->
            exit(normal);
        #message_to{to_name=ToName, message=Message} ->
            {messenger, Server_Node} ! 
                #message{client_pid=self(), to_name=ToName, message=Message},
            await_result();
        {message_from, FromName, Message} ->
            io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    client(Server_Node).

%%% wait for a response from the server
await_result() ->
    receive
        #abort_client{message=Why} ->
            io:format("~p~n", [Why]),
            exit(normal);
        #server_reply{message=What} ->
            io:format("~p~n", [What])
    after 5000 ->
            io:format("No response from server~n", []),
            exit(timeout)
    end.

%%%----END FILE---
```

```
%%%----FILE mess_server.erl----

%%% This is the server process of the messenger service

-module(mess_server).
-export([start_server/0, server/0]).
-include("mess_interface.hrl").

server() ->
    process_flag(trap_exit, true),
    server([]).

%%% the user list has the format [{ClientPid1, Name1},{ClientPid22, Name2},...]
server(User_List) ->
    io:format("User list = ~p~n", [User_List]),
    receive
        #logon{client_pid=From, username=Name} ->
            New_User_List = server_logon(From, Name, User_List),
            server(New_User_List);
        {'EXIT', From, _} ->
            New_User_List = server_logoff(From, User_List),
            server(New_User_List);
        #message{client_pid=From, to_name=To, message=Message} ->
            server_transfer(From, To, Message, User_List),
            server(User_List)
    end.

%%% Start the server
start_server() ->
    register(messenger, spawn(?MODULE, server, [])).

%%% Server adds a new user to the user list
server_logon(From, Name, User_List) ->
    %% check if logged on anywhere else
    case lists:keymember(Name, 2, User_List) of
        true ->
            From ! #abort_client{message=user_exists_at_other_node},
            User_List;
        false ->
            From ! #server_reply{message=logged_on},
            link(From),
            [{From, Name} | User_List]        %add user to the list
    end.

%%% Server deletes a user from the user list
server_logoff(From, User_List) ->
    lists:keydelete(From, 1, User_List).

%%% Server transfers a message between user
server_transfer(From, To, Message, User_List) ->
    %% check that the user is logged on and who he is
    case lists:keysearch(From, 1, User_List) of
        false ->
            From ! #abort_client{message=you_are_not_logged_on};
        {value, {_, Name}} ->
            server_transfer(From, Name, To, Message, User_List)
    end.
%%% If the user exists, send the message
server_transfer(From, Name, To, Message, User_List) ->
    %% Find the receiver and send the message
    case lists:keysearch(To, 2, User_List) of
        false ->
            From ! #server_reply{message=receiver_not_found};
        {value, {ToPid, To}} ->
            ToPid ! #message_from{from_name=Name, message=Message}, 
            From !  #server_reply{message=sent} 
    end.

%%%----END FILE---
```  

## 5.2 头文件

如上所示，某些文件的扩展名为 .hrl。这些是在 .erl 文件中会用到的头文件，使用方法如下：  

```
-include("File_Name").
```

例如：  

```
-include("mess_interface.hrl").
``` 

在本例中，上面所有的文件与 messager 系统的其它文件在同一个目录下。  

.hrl 文件中可以包含任何合法的 Erlang 代码，但是通常里面只包含一些记录和宏的定义。  

## 5.3 记录

记录的定义如下：  

```
-record(name_of_record,{field_name1, field_name2, field_name3, ......}).
```  

例如，

```
-record(message_to,{to_name, message}).
```  

这等价于：  

```
{message_to, To_Name, Message}
```  

用一个例子来说明怎样创建一个记录：  

```
#message_to{message="hello", to_name=fred)
```  

上面的代码创建了如下的记录：  

```
{message_to, fred, "hello"}
```  

注意，使用这种方式创建记录时，你不需要考虑给每个部分赋值时的顺序问题。这样做的另外一个优势在于你可以把接口一并定义在头文件中，这样修改接口会变得非常容易。例如，如果你想在记录中添加一个新的域，你只需要在使用该新域的地方进行修改就可以了，而不需要在每个使用记录的地方都进行修改。如果你在创建记录时漏掉了其中的某些域，则这些域会得到一个默认的原子值 undefined。  

使用记录进行模式匹配与创建记录是一样。例如，在 receive 的 case 中：  

```
#message_to{to_name=ToName, message=Message} ->
```  

这与下面的代码是一样的：  

```
{message_to, ToName, Message}
```  

## 5.4 宏  

在 messager 系统添加的另外一种东西是宏。在 mess\_config.hrl 文件中包含如下的定义：  

```
%%% Configure the location of the server node,
-define(server_node, messenger@super).
```  

这个头文件被包括到了 mess\_server.erl 文件中：  

```
-include("mess_config.hrl").
```  

这样，在 mess\_server.erl 中出现的每个 server\_node 都被替换为 messenger@super。  

宏还被用于生成服务端进程：  

```
spawn(?MODULE, server, [])
```  

这是一个标准宏（也就是说，这是一个系统定义的宏而不是用户自定义的宏）。?MODULE  宏总是被替换为当前模块名（也就是在文件开始的部分的 -module 定义的名称）。宏有许多的高级用法，作为参数只是其中之一。  

Messager 系统中的三个 Erlang（.erl）文件被分布编译成三个独立的目标代码文件（.beam）中。当执行过程中引用到这些代码时，Erlang 系统会将它们加载并链接到系统中。在本例中，我们把它们全部放到当前工作目录下（即你执行 "cd" 命令后所在的目录）。我们也可以将这些文件放到其它目录下。  

在这个 messager 例子中，我们没有对发送消息的内容做出任何假设和限制。这些消息可以是任何合法的 Erlang 项。