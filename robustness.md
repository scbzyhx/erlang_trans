# 4 健壮性（Robustness） 

[完整示例](http://www.erlang.org/doc/getting_started/conc_prog.html#ex)中还存在一些问题。当用户所登录的结点崩溃时，用户没有从系统中登出，因此该用户仍然在服务器的 User_List 中，但事实是用户已经不在系统中了。这会导致这用户不能再次登录，因为系统认为它已经在系统中了。  

或者，如果服务器发送消息出现故障了，那么这时候会导致客户端在 await_result 函数中一直等待，那又该怎么处理这个问题呢？  

## 4.1 超时  

在改进 messager 程序之前，让我们一起学习一些基本的原则。回忆一下，当 “ping” 结束的时候，它向 “pong” 发送一个原子值 finished 的消息以通知 “pong” 结束程序。另一种让 “pong” 结束的办法是当 “pong” 有一定时间没有收到来自 “ping” 的消息时则退出程序。我们可在 pong 中添加一个 time-out 来实现它：  

```
-module(tut19).

-export([start_ping/1, start_pong/0,  ping/2, pong/0]).

ping(0, Pong_Node) ->
    io:format("ping finished~n", []);

ping(N, Pong_Node) ->
    {pong, Pong_Node} ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_Node).

pong() ->
    receive
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    after 5000 ->
            io:format("Pong timed out~n", [])
    end.

start_pong() ->
    register(pong, spawn(tut19, pong, [])).

start_ping(Pong_Node) ->
    spawn(tut19, ping, [3, Pong_Node]).
```  

编译上面的代码并将生成的 tut19.beam 文件拷贝到某个目录下，下面是在结点 pong@kosken 上的输出：  

```
true
Pong received ping
Pong received ping
Pong received ping
Pong timed out
```  

在结点 ping@gollum 上的输出结果为：  

```
(ping@gollum)1> tut19:start_ping(pong@kosken).
<0.36.0>
Ping received pong
Ping received pong
Ping received pong
ping finished 
```  

time-out 被设置在：  

```
pong() ->
    receive
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    after 5000 ->
            io:format("Pong timed out~n", [])
    end.
```  

执行 recieve 时，超时定时器 （5000 ms）启动；一旦收到 {ping,Ping\_PID} 消息，则取消该超时定时器。如果没有收到 {ping,Ping\_PID} 消息，那么 5000 毫秒后 time-out 后面的程序就会被执行。after 必须是 recieve 中的最后一个，也就是说，recieve 中其它所有消息的接收处理都优先于超时消息。我们也可以在超时时调用 一个函数为超时返回一个整数值：  

```
after pong_timeout() ->
```  

一般地，除了使用超时来监测分布式 Erlang 系统的各分部外，还有许多更好的办法来实现监测功能。超时适用于监测来自于系统外部的事件，比如说，当你希望在指定时间内收到来自外部系统的消息的时候。举个例子，我们可以用超时来发现用户离开了messager 系统，比如说当用户 10 分钟没有访问系统时，则认为其已离开了系统。  

## 4.2 错误处理  

在讨论监督与错误处理细节之前，让我们先一起来看一下 Erlang 进程的终止过程，或者说 Erlang 的术语 exit。  

进程执行 exit(normal) 结束或者运行完所有的代码而结束被认为是正常（normal）终止。  

进程因为触发运行时错误（例如，除零、错误匹配、调用不存在了函数等）而终止被称之为异常终止。进程执行 exit(Reason) （注意此处的 Reason 是除 normal 以外的值）终止也被称之为异常终止。  

一个 Erlang 进程可以与其它 Erlang 进程建立连接。如果一个进程调用 link(Other_Pid)，那么它就在其自己与 Othre_Pid 进程之间创建了一个双向连接。当一个进程结束时，它会发送信号至所有与有连接的进程。  

这个信号携带着进程的进程标识符以及进程结束的原因信息。  

进程收到进程正常退出的信号时默认情况下是直接忽略它。  

但是，如果进程收到的是异常终止的信号，则默认动作为：  

- 将所有消息投递到接收信号的进程
- 杀死接收信号的进程
- 将相同的错误消息传递给被杀死进程的各个连接。

所以，你可以使用链接的方式把同一事务的所有进程连接起来。如果其中一个进程异常终止，事务中所有进程都会被杀死。正是因为在实际生产过程中，常常创建进程同时与之建立连接的需求，所以存在这样一个内置函数 spawn_link，与 spawn 不同之处在于，它创建一个新进程同时在新进程与创建者之间建立连接。  

下面给出了 ping pong 示例子另外一种实现方法，它通过连接终止 "pong" 进程：  

```
-module(tut20).

-export([start/1,  ping/2, pong/0]).

ping(N, Pong_Pid) ->
    link(Pong_Pid),
    ping1(N, Pong_Pid).

ping1(0, _) ->
    exit(ping);

ping1(N, Pong_Pid) ->
    Pong_Pid ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping1(N - 1, Pong_Pid).

pong() ->
    receive
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start(Ping_Node) ->
    PongPID = spawn(tut20, pong, []),
    spawn(Ping_Node, tut20, ping, [3, PongPID]).
```  

```
(s1@bill)3> tut20:start(s2@kosken).
Pong received ping
<3820.41.0>
Ping received pong
Pong received ping
Ping received pong
Pong received ping
Ping received pong
```  

与前面的代码一样，ping pong 程序的两个进程仍然都是在 start/1 函数中创建的，“ping”进程在单独的结点上建立的。但是这里做了一些小的改动，用到了内置函数 link。“Ping” 结束时调用 exit(ping) ，使得一个终止信号传递给 “pong” 进程，从而导致 “pong” 进程终止。  

也可以修改进程收到异常终止信号时的默认行为，避免进程被杀死。即，把所有的信号都转变为一般的消息添加到信号接收进程的消息队列中，消息的格式为 {'EXIT',FromPID,Reason}。我们可以通过如下的代码来设置：  

```
process_flag(trap_exit, true)
```  

还有其它可以用的进程标志，可参阅 [erlang (3)](http://www.erlang.org/doc/man/erlang.html#process_flag-2)。标准用户程序一般不需要改变进程对于信号的默认处理行为，但是对于 OTP 中的管理程序这个接口还是很有必要的。下面修改了 ping pong 程序来打印输出进程退出时的信息：  

```
-module(tut21).

-export([start/1,  ping/2, pong/0]).

ping(N, Pong_Pid) ->
    link(Pong_Pid), 
    ping1(N, Pong_Pid).

ping1(0, _) ->
    exit(ping);

ping1(N, Pong_Pid) ->
    Pong_Pid ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping1(N - 1, Pong_Pid).

pong() ->
    process_flag(trap_exit, true), 
    pong1().

pong1() ->
    receive
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong1();
        {'EXIT', From, Reason} ->
            io:format("pong exiting, got ~p~n", [{'EXIT', From, Reason}])
    end.

start(Ping_Node) ->
    PongPID = spawn(tut21, pong, []),
    spawn(Ping_Node, tut21, ping, [3, PongPID]).
```  

```
(s1@bill)1> tut21:start(s2@gollum).
<3820.39.0>
Pong received ping
Ping received pong
Pong received ping
Ping received pong
Pong received ping
Ping received pong
pong exiting, got {'EXIT',<3820.39.0>,ping}
```  

## 4.3 增加健壮性后的完整示例  

让我们改进 Messager 程序以增加该程序的健壮性：  

```
%%% Message passing utility.  
%%% User interface:
%%% login(Name)
%%%     One user at a time can log in from each Erlang node in the
%%%     system messenger: and choose a suitable Name. If the Name
%%%     is already logged in at another node or if someone else is
%%%     already logged in at the same node, login will be rejected
%%%     with a suitable error message.
%%% logoff()
%%%     Logs off anybody at that node
%%% message(ToName, Message)
%%%     sends Message to ToName. Error messages if the user of this 
%%%     function is not logged on or if ToName is not logged on at
%%%     any node.
%%%
%%% One node in the network of Erlang nodes runs a server which maintains
%%% data about the logged on users. The server is registered as "messenger"
%%% Each node where there is a user logged on runs a client process registered
%%% as "mess_client" 
%%%
%%% Protocol between the client processes and the server
%%% ----------------------------------------------------
%%% 
%%% To server: {ClientPid, logon, UserName}
%%% Reply {messenger, stop, user_exists_at_other_node} stops the client
%%% Reply {messenger, logged_on} logon was successful
%%%
%%% When the client terminates for some reason
%%% To server: {'EXIT', ClientPid, Reason}
%%%
%%% To server: {ClientPid, message_to, ToName, Message} send a message
%%% Reply: {messenger, stop, you_are_not_logged_on} stops the client
%%% Reply: {messenger, receiver_not_found} no user with this name logged on
%%% Reply: {messenger, sent} Message has been sent (but no guarantee)
%%%
%%% To client: {message_from, Name, Message},
%%%
%%% Protocol between the "commands" and the client
%%% ---------------------------------------------- 
%%%
%%% Started: messenger:client(Server_Node, Name)
%%% To client: logoff
%%% To client: {message_to, ToName, Message}
%%%
%%% Configuration: change the server_node() function to return the
%%% name of the node where the messenger server runs

-module(messenger).
-export([start_server/0, server/0, 
         logon/1, logoff/0, message/2, client/2]).

%%% Change the function below to return the name of the node where the
%%% messenger server runs
server_node() ->
    messenger@super.

%%% This is the server process for the "messenger"
%%% the user list has the format [{ClientPid1, Name1},{ClientPid22, Name2},...]
server() ->
    process_flag(trap_exit, true),
    server([]).

server(User_List) ->
    receive
        {From, logon, Name} ->
            New_User_List = server_logon(From, Name, User_List),
            server(New_User_List);
        {'EXIT', From, _} ->
            New_User_List = server_logoff(From, User_List),
            server(New_User_List);
        {From, message_to, To, Message} ->
            server_transfer(From, To, Message, User_List),
            io:format("list is now: ~p~n", [User_List]),
            server(User_List)
    end.

%%% Start the server
start_server() ->
    register(messenger, spawn(messenger, server, [])).

%%% Server adds a new user to the user list
server_logon(From, Name, User_List) ->
    %% check if logged on anywhere else
    case lists:keymember(Name, 2, User_List) of
        true ->
            From ! {messenger, stop, user_exists_at_other_node},  %reject logon
            User_List;
        false ->
            From ! {messenger, logged_on},
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
            From ! {messenger, stop, you_are_not_logged_on};
        {value, {_, Name}} ->
            server_transfer(From, Name, To, Message, User_List)
    end.

%%% If the user exists, send the message
server_transfer(From, Name, To, Message, User_List) ->
    %% Find the receiver and send the message
    case lists:keysearch(To, 2, User_List) of
        false ->
            From ! {messenger, receiver_not_found};
        {value, {ToPid, To}} ->
            ToPid ! {message_from, Name, Message}, 
            From ! {messenger, sent} 
    end.

%%% User Commands
logon(Name) ->
    case whereis(mess_client) of 
        undefined ->
            register(mess_client, 
                     spawn(messenger, client, [server_node(), Name]));
        _ -> already_logged_on
    end.

logoff() ->
    mess_client ! logoff.

message(ToName, Message) ->
    case whereis(mess_client) of % Test if the client is running
        undefined ->
            not_logged_on;
        _ -> mess_client ! {message_to, ToName, Message},
             ok
end.

%%% The client process which runs on each user node
client(Server_Node, Name) ->
    {messenger, Server_Node} ! {self(), logon, Name},
    await_result(),
    client(Server_Node).

client(Server_Node) ->
    receive
        logoff ->
            exit(normal);
        {message_to, ToName, Message} ->
            {messenger, Server_Node} ! {self(), message_to, ToName, Message},
            await_result();
        {message_from, FromName, Message} ->
            io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    client(Server_Node).

%%% wait for a response from the server
await_result() ->
    receive
        {messenger, stop, Why} -> % Stop the client 
            io:format("~p~n", [Why]),
            exit(normal);
        {messenger, What} ->  % Normal response
            io:format("~p~n", [What])
    after 5000 ->
            io:format("No response from server~n", []),
            exit(timeout)
    end.
```  

主要有如下几处改动:

Messager 服务器捕捉进程退出。如果它收到进程终止信号，{'EXIT',From,Reason}，则说明客户端进程已经终止或者由于下面的原因变得不可达：  

- 用户主动退出登录（取消了 “logoff” 消息）。
- 与客户端连接的网络已经断开。
- 客户进程所处的结点崩溃。
- 客户进程执行了某些非法操作。  

如果收到上面所述的退出信号，服务器调用 server\_logoff 函数将 {From, Name} 元组从 User_Lists 列表中删除。如果服务端所有的结点崩溃了，那么系统将将自动产生进程终止信号，并将其发送给所有的客户端进程：'EXIT',MessengerPID,noconnection} 
，客户端进程收到该消息后会终止自身。  

同样，在 await_result 函数中引入了一个 5 秒钟的定时器。也就是说，如果服务器 5 秒钟之类没有回复客户端，则客户端终止执行。这个只是在服务端与客户端建立连接前的登录阶段需要。  

一个非常有意思的例子是如果客户端在服务端建立连接前终止。需要特别注意，如果一个进程与另一个不存在的进程建立连接，则会收到一个终止信号 {'EXIT',From, noproc}。这就好像连接建立后进程立马就结束了一样。