# 并发编程  

## 3.1 进程  

相比于其它函数式编程语言，Erlang 的优势在于它的并发程序设计与分布式程序设计。并发是指一个程序中同时有多个线程在执行。例如，现代操作系统允许你同时使用文字处理、电子制表软件、邮件终端和打印任务。在任意一个时刻，系统中每个处理单元（CPU）都只有一个线程（任务）在执行，但是可以通过以一定速率交替执行这些线程使得这些它们看上去像是在同时运行一样。Erlang 中创建多线程非常简单，而且很容易就可以实现这些线程之间的通信。Erlang 中，每个执行的线程都称之为一个 process（即进程，注意与操作系统中的进程概念不太一样）。  

（注意：进程被用于没有共享数据的执行线程的场景。而线程（thread）则被用于共享数据的场景下。由于 Erlang 各执行线程之间不共享数据，所以我们一般将其称之为进程。)  

Erlang 的内置函数 spawn 可以用来创建一个新的进程： spawn(Module, Exported_Function, List of Arguments)。假设有如下这样一个模块：  

```
-module(tut14).

-export([start/0, say_something/2]).

say_something(What, 0) ->
    done;
say_something(What, Times) ->
    io:format("~p~n", [What]),
    say_something(What, Times - 1).

start() ->
    spawn(tut14, say_something, [hello, 3]),
    spawn(tut14, say_something, [goodbye, 3]).
```  

```
5> c(tut14).
{ok,tut14}
6> tut14:say_something(hello, 3).
hello
hello
hello
done
```  

如上所示，say\_something 函数根据第二个参数指定的次数将第一个参数的值输出多次。函数 start 启动两个 Erlang 进程，其中一个将 “hello” 输出 3 次，另一个进程将 “goodbye” 输出三次。所有的进程中都调用了 say\_something 函数。不过需要注意的是，要想使用一个函数启动一个进程，这个函数就必须导出此模块，同时必须使用 spawn 启动。  

```
9> tut14:start().
hello
goodbye
<0.63.0>
hello
goodbye
hello
goodbye
```  

请注意，这里并不是先输出 “goodbye” 三次后再输出 “goodbye” 三次。而是，第一个进程先输出一个 "hello"，然后第二个进程再输出一次 "goodbye"。接下来，第一个进程再输出第二个 "hello"。但是奇怪的是 <0.63.0> 到底是哪儿来的呢？在 Erlang 系统中，一个函数的返回值是函数最后一个表达式的值，而 start 函数的第后一个表达式是：  

```
spawn(tut14, say_something, [goodbye, 3]).
```  

spawn 返回的是进程的标识符，简记为 pid。进程标识符是用来唯一标识 Erlang 进程的标记。所以说，<0.63.0> 也就是spawn 返回的一个进程标识符。下面一个例子就可会讲解如何使用进程标识符。  

另外，这个例子中 io:format 输出用的不是 ~w 而变成了 ~p。引用用户手册的说法：“~p 与 ~w 一样都是将数据按标准语法的格式输出，但是当输出的内容需要占用多行时，~p 在分行处可以表现得更加智能。此外，它还会尝试检测出列表中的可输出字符串并将按字符串输出”。  

## 3.2 消息传递  

下面的例子中创建了两个进程，它们相互之间会发送多个消息。  

```
-module(tut15).

-export([start/0, ping/2, pong/0]).

ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("ping finished~n", []);

ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_PID).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start() ->
    Pong_PID = spawn(tut15, pong, []),
    spawn(tut15, ping, [3, Pong_PID]).
```  

```
1> c(tut15).
{ok,tut15}
2> tut15: start().
<0.36.0>
Pong received ping
Ping received pong
Pong received ping
Ping received pong
Pong received ping
Ping received pong
ping finished
Pong finished
```  

start 函数先创建了一个进程，我们称之为 “pong”：  

```
Pong_PID = spawn(tut15, pong, [])
```   

这个进程会执行 tut15:pong 函数。Pong_PID 是 “pong” 进程的进程标识符。接下来，start 函数又创建了另外一个进程 ”ping“：  

```
spawn(tut15,ping,[3,Pong_PID]),
```  

这个进程执行：  

```
tut15:ping(3, Pong_PID)
```  

<0.36.0> 为是 start 函数的返回值。  

”pong“ 进程完成下面的工作：  

```
receive
    finished ->
        io:format("Pong finished~n", []);
    {ping, Ping_PID} ->
        io:format("Pong received ping~n", []),
        Ping_PID ! pong,
        pong()
end.
```   

receive 关键字被进程用来接收从其它进程发送的的消息。它的使用语法如下：  

```
receive
   pattern1 ->
       actions1;
   pattern2 ->
       actions2;
   ....
   patternN
       actionsN
end.
```  

请注意，在 end 前的最后一个 actions 并没有 ";"。  

Erlang 进程之间的消息可以是任何简单的 Erlang 项。比如说，可以是列表、元组、整数、原子、进程标识等等。  

每个进程都有独立的消息接收队列。新接收的消息被放置在接收队列的尾部。当进程执行 receive 时，消息中第一个消息与与 receive 后的第一个模块进行匹配。如果匹配成功，则将该消息从消息队列中删除，并执行该模式后面的代码。  

然而，如果第一个模式匹配失败，则测试第二个匹配。如果第二个匹配成功，则将该消息从消息队列中删除，并执行第二个匹配后的代码。如果第二个匹配也失败，则匹配第三个，依次类推，直到所有模式都匹配结束。如果所有匹配都失败，则将第一个消息留在消息队列中，使用第二个消息重复前面的过程。第二个消息匹配成功时，则执行匹配成功后的程序并将消息从消息队列中取出（将第一个消息与其余的消息继续留在消息队列中）。如果第二个消息也匹配失败，则尝试第三个消息，依次类推，直到尝试完消息队列所有的消息为止。如果所有消息都处理结束（匹配失败或者匹配成功被移除），则进程阻塞，等待新的消息的到来。上面的过程将会一直重复下去。  

Erlang 实现是非常 “聪明” 的，它会尽量减少 receive 的每个消息与模式匹配测试的次数。  

让我们回到 ping pong 示例程序。  

“Pong” 一直等待接收消息。 如果收到原子值  finished，“Pong” 会输出 “Pong finished”，然后结束进程。如果收到如下形式的消息：  

```
{ping, Ping_PID}
```  

则输出 “Pong received ping”，并向进程 “ping” 发送一个原子值消息 pong：  

```
Ping_PID ! pong
```  

请注意这里是如何使用 “!” 操作符发送消息的。 “!” 操作符的语法如下所示：  

```
Pid ! Message
```  

这表示将消息（任何 Erlang 数据）发送到进程标识符为 Pid 的进程的消息队列中。  

将消息 pong 发送给进程 “ping” 后，“pong” 进程再次调用 pong 函数，这会使得再次回到 receive 等待下一个消息的到来。  

下面，让我们一起去看看进程 “ping”，回忆一下它是从下面的地方开始执行的：  

```
tut15:ping(3, Pong_PID)
```  

可以看一下 ping/2 函数，由于第一个参数的值是 3 而不是 0， 所以 ping/2 函数的第二个子句被执行（第一个子句的头为 ping(0,Pong\_PID)，第二个子句的头部为 ping(N,Pong\_PID)，因此 N 为3 ）。  

第二个子句将发送消息给 “pong” 进程：  

```
Pong_PID ! {ping, self()},
```

self() 函数返回当前进程（执行 self() 的进程）的进程标识符，在这儿为 “ping” 进程的进程标识符。（回想一下 “pong” 的代码，这个进程标识符值被存储在变量 Ping\_PID 当中）  

发送完消息后，“Ping” 接下来等待回复消息 “pong”：  

```
receive
    pong ->
        io:format("Ping received pong~n", [])
end,
```  

收到回复消息后，则输出 “Ping received pong”。之后 “ping” 也再次调用 ping 函数：  

```  
ping(N - 1, Pong_PID)
```   

N-1 使得第一个参数逐渐减小到 0。当其值变为 0 后，ping/2 函数的第一个子句会被执行。  

```
ping(0, Pong_PID) ->
    Pong_PID !  finished,
    io:format("ping finished~n", []);
```

此时，原子值 finished 被发送至 “pong” 进程（会导致进程结束），同时将“ping finished” 输出。随后，“Ping” 进程结束。  

## 3.3 注册进程名称  

上面的例子中，因为 “Pong” 在 “ping” 进程开始前已经创建完成，所以才能将 “pong” 进程的进程标识符作为参数传递给进程 “ping”。这也就说，“ping” 进程必须通过某种途径获得 “pong” 进程的进程标识符后才能将消息发送 “pong” 进程。然而，某些情况下，进程需要相互独立地启动，而这些进程之间又要求知道彼此的进程标识符，前面提到的这种方式就不能满足要求了。因此，Erlang 提供了为每个进程提供一个名称绑定的机制，这样进程间通信就可以通过进程名来实现，而不需要知道进程的进程标识符了。为每个进程注册一个名称需要用到内置函数 register：  

```
register(some_atom, Pid)
```  

接下来，让我们一起上面的 ping pong 示例程序。这一次，我们为 “pong” 进程赋予了一名进程名称 pong：  

```
-module(tut16).

-export([start/0, ping/1, pong/0]).

ping(0) ->
    pong ! finished,
    io:format("ping finished~n", []);

ping(N) ->
    pong ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start() ->
    register(pong, spawn(tut16, pong, [])),
    spawn(tut16, ping, [3]).
```

```
2> c(tut16).
{ok, tut16}
3> tut16:start().
<0.38.0>
Pong received ping
Ping received pong
Pong received ping
Ping received pong
Pong received ping
Ping received pong
ping finished
Pong finished
```

start/0 函数如下：  

```
register(pong, spawn(tut16, pong, [])),
```  

创建 “pong” 进程的同时还赋予了它一个名称 pong。在 “ping” 进程中，通过如下的形式发送消息：  

```
pong ! {ping, self()},
```  

ping/2 变成了 ping/1。这是因为不再需要参数 Pong\_PID 了。   

## 3.4 分布式编程  

下面我们进一步对 ping pong 示例程序进行改进。 这一次，我们要让 “ping”、“pong” 进程分别位于不同的计算机上。要想让这个程序工作，你首先的搭建一下分布式的系统环境。分布式 Erlang 系统的实现提供了基本的安全机制，它阻止未授权的外部设备访问本机的 Erlang 系统。同一个系统中的 Erlang 要想相互通信需要设置相同的 magic cookie。设置 magic cookie 最便捷地实现方式就是在你打算运行分布式 Erlang 系统的所有计算机的 home 目录下创建一个 .erlang.cookie 文件：  

- 在 windows 系统中，home 目录为环境变量 $HOME 指定的目录--这个变量的值可能需要你手动设置  
- 在 Linux 或者 UNIX 系统中简单很多，你只需要在执行 cd 命令后所进入的目录下创建一个 .erlang.cookie 文件就可以了。 

.erlang.cookie 文件只有一行内容，这一行包含一个原子值。例如，在 Linux 或 UNIX 系统的 shell 执行如下命令：  

```
$ cd
$ cat > .erlang.cookie
this_is_very_secret
$ chmod 400 .erlang.cookie
```  
 
 使用 chmod 命令让 .erlang.cookie 文件只有文件拥者可以访问。这个是必须设置的。  

当你想要启动 erlang 系统与其它 erlang 系统通信时，你需要给 erlang 系统一个名称，例如：  

```
$erl -sname my_name
```  

在后面你还会看到更加详细的内容。如果你想尝试一下分布式 Erlang 系统，而又只有一台计算机，你可以在同一台计算机上分别启动两个 Erlang 系统，并分别赋予不同的名称即可。运行在每个计算机上的 Erlang 被称为一个 **Erang 结点（Erlang Node）**。  

（注意：erl -sname 要求所有的结点在同一个 IP 域内。如果我们的 Erlang 结点位于不同的 IP 域中，则我们需要使用 -name，而且需要指定所有的 IP 地址。）  

下面这个修改后的 ping pong 示例程序可以分别运行在两个结点之上：  

```
-module(tut17).

-export([start_ping/1, start_pong/0,  ping/2, pong/0]).

ping(0, Pong_Node) ->
    {pong, Pong_Node} ! finished,
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
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start_pong() ->
    register(pong, spawn(tut17, pong, [])).

start_ping(Pong_Node) ->
    spawn(tut17, ping, [3, Pong_Node]).
``` 

我们假设这两台计算分别称之为 gollum 与 kosken。在 kosken 上启动结点 ping。在 gollum 上启动结点 pong。  

在 kosken 系统上（Linux/Unix 系统）：  

```
kosken> erl -sname ping
Erlang (BEAM) emulator version 5.2.3.7 [hipe] [threads:0]

Eshell V5.2.3.7  (abort with ^G)
(ping@kosken)1>
```  

在 gollum 上：  

```
gollum> erl -sname pong
Erlang (BEAM) emulator version 5.2.3.7 [hipe] [threads:0]

Eshell V5.2.3.7  (abort with ^G)
(pong@gollum)1>
```  

下面，在 gollum 上启动 "pong" 进程：  

```
(pong@gollum)1> tut17:start_pong().
true
```  

然后在 kosken 上启动 “ping” 进程（从上面的代码中可以看出，start\_ping 的函数的其中一个参数为 “pong” 进程所在结点的名称）：  

```
(ping@kosken)1> tut17:start_ping(pong@gollum).
<0.37.0>
Ping received pong
Ping received pong 
Ping received pong
ping finished
```  

如上所示，ping pong 程序已经开始运行了。在 “pong” 的这一端：  

```
(pong@gollum)2>
Pong received ping                 
Pong received ping                 
Pong received ping                 
Pong finished                      
(pong@gollum)2>
```  

再看一下 tut17 的代码，你可以看到 pong 函数根本就没有发生任何改变，无论 “ping” 进程运行在哪个结点下，下面这一行代码都可以正确的工作：  

```
{ping, Ping_PID} ->
    io:format("Pong received ping~n", []),
    Ping_PID ! pong,
```  

因此，Erlang 的进程标识符中包含了程序运行在哪个结点上的位置信息。所以，如果你知道了进程的进程标识符，无论进程是运行在本地结点上还是其它结点上面，"!" 操作符都可以将消息发送到该进程。  

要想通过进程注册的名称向其它结点上的进程发送消息，这时候就有一些不同之处了：  

```
{pong, Pong_Node} ! {ping, self()},
```  

这个时候，我们就不能再只用 registered\_name 作为参数了，而需要使用元组 {registered\_name,node\_name} 作为注册进程的名称参数。  

在之前的代码中了，“ping”、“pong” 进程是在两个独立的 Erlang 结点上通过 shell 启动的。 spawn 也可以在其它结点（非本地结点）启动新的进程。  

下面这段示例代码也是一个 ping pong 程序，但是这一次 “ping” 是在异地结点上启动的：  

```
-module(tut18).

-export([start/1,  ping/2, pong/0]).

ping(0, Pong_Node) ->
    {pong, Pong_Node} ! finished,
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
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start(Ping_Node) ->
    register(pong, spawn(tut18, pong, [])),
    spawn(Ping_Node, tut18, ping, [3, node()]).
```  

假设在 Erlang 系统 ping 结点（注意不是进程 “ping”）已经在 kosken 中启动（译注：可以理解 Erlang 结点已经启动），则在 gollum 会有如下的输出：  

```
<3934.39.0>
Pong received ping
Ping received pong
Pong received ping
Ping received pong
Pong received ping
Ping received pong
Pong finished
ping finished
```  

注意所有的内容都输出到了 gollum 结点上。这是因为 I/O 系统发现进程是由其它结点启动的时候，会自将输出内容输出到启动进程所在的结点。  

## 3.5 完整示例  

接下来这个示例是一个简单的消息传递者（messager）示例。Messager 是一个允许用登录到不同的结点并向彼此发送消息的应用程序。  

开始之前，请注意以下几点：  

- 这个示例只演示了消息传递的逻辑---没有提供用户友好的界面（虽然这在 Erlang 是可以做到的）。
- 这类的问题使用 OTP 的工具可以非常方便的实现，还能同时提供线上更新的方法等。（参考 [OTP 设计原则](http://www.erlang.org/doc/design_principles/des_princ.html#otp%20design%20principles)）
- 这个示例程序并不完整，它没有考虑到结点离开等情况。这个问题在后面的版本会得到修复。  

Messager 允许 “客户端” 连接到集中的服务器并表明其身份。也就是说，用户并不需要知道另外一个用户所在 Erlang 结点的名称就可以发送消息。  

messager.erl 文件内容如下：  

```
%%% Message passing utility.  
%%% User interface:
%%% logon(Name)
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
%%% To server: {ClientPid, logoff}
%%% Reply: {messenger, logged_off}
%%%
%%% To server: {ClientPid, logoff}
%%% Reply: no reply
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
-export([start_server/0, server/1, logon/1, logoff/0, message/2, client/2]).

%%% Change the function below to return the name of the node where the
%%% messenger server runs
server_node() ->
    messenger@bill.

%%% This is the server process for the "messenger"
%%% the user list has the format [{ClientPid1, Name1},{ClientPid22, Name2},...]
server(User_List) ->
    receive
        {From, logon, Name} ->
            New_User_List = server_logon(From, Name, User_List),
            server(New_User_List);
        {From, logoff} ->
            New_User_List = server_logoff(From, User_List),
            server(New_User_List);
        {From, message_to, To, Message} ->
            server_transfer(From, To, Message, User_List),
            io:format("list is now: ~p~n", [User_List]),
            server(User_List)
    end.

%%% Start the server
start_server() ->
    register(messenger, spawn(messenger, server, [[]])).


%%% Server adds a new user to the user list
server_logon(From, Name, User_List) ->
    %% check if logged on anywhere else
    case lists:keymember(Name, 2, User_List) of
        true ->
            From ! {messenger, stop, user_exists_at_other_node},  %reject logon
            User_List;
        false ->
            From ! {messenger, logged_on},
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
        {value, {From, Name}} ->
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


%%% The client process which runs on each server node
client(Server_Node, Name) ->
    {messenger, Server_Node} ! {self(), logon, Name},
    await_result(),
    client(Server_Node).

client(Server_Node) ->
    receive
        logoff ->
            {messenger, Server_Node} ! {self(), logoff},
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
    end.
```  

在使用本示例程序之前，你需要：  

- 配置 server_node() 函数。
- 将编译后的代码（messager.beam）拷贝到每一个你启动了 Erlang 的计算机上。

这接下来的例子中，我们在四台不同的计算上启动了 Erlang 结点。如果你的网络没有那么多的计算机，你也可以在同一台计算机上启动多个结点。  

启动的四个结点分别为：messager@super，c1@bilo，c2@kosken，c3@gollum。  

首先在 meesager@super 上启动服务器程序：  

```
(messenger@super)1> messenger:start_server().
true
```  

接下来用 peter 是在 c1@bibo 登录：  

```
(c1@bilbo)1> messenger:logon(peter).
true
logged_on
```  

然后 James 在 c2@kosken 上登录：  

```
(c2@kosken)1> messenger:logon(james).
true
logged_on
```  

最后，用 Fred 在 c3@gollum 上登录：  

```
(c3@gollum)1> messenger:logon(fred).
true
logged_on
```  

现在，Peter 就可以向 Fred 发送消息了：  

```
(c1@bilbo)2> messenger:message(fred, "hello").
ok
sent
``` 

Fred 收到消息后，回复一个消息给 Peter 然后登出：  

```
Message from peter: "hello"
(c3@gollum)2> messenger:message(peter, "go away, I'm busy").
ok
sent
(c3@gollum)3> messenger:logoff().
logoff
```  

随后，James 再向 Fred 发送消息时，则出现下面的情况：  

```
(c2@kosken)2> messenger:message(fred, "peter doesn't like you").
ok
receiver_not_found
``` 
 
因为 Fred 已经离开，所以发送消息失败。  

让我们先来看看这个例子引入的一些新的概念。  

这里有两个版本的 server_transfer 函数：其中一个有四个参数（server\_transfer/4）另外一个有五个参数（server\_transfer/5）。Erlang 将它们看作两个完全不一样的函数。  

请注意这里是如何让 server\_transfer 函数通过 server(User_List) 调用其自身的，这里形成了一个循环。 Erlang 编译器非常的聪明，它会将上面的代码优化为一个循环而不是一个非法的递规函数调用。但是它只能是在函数调用后面没有别的代码的情况下才能工作（注：即尾递规）。  

示例中用到了lists 模块中的函数。lists 模块是一个非常有用的模块，推荐你通过用户手册仔细研究一下（erl -man lists）。   

lists:keymemeber(Key,Position,Lists) 函数遍历列表中的元组，查看每个元组的指定位置 （Position）处的数据并判断元组该位置是否与 Key 相等。元组中的第一个元素的位置为 1，依次类推。如果发现某个元组的 Position 位置处的元素与 Key 相同，则返回 true，否则返回 false。  

```
3> lists:keymember(a, 2, [{x,y,z},{b,b,b},{b,a,c},{q,r,s}]).
true
4> lists:keymember(p, 2, [{x,y,z},{b,b,b},{b,a,c},{q,r,s}]).
false
```  

lists:keydelete 与 lists:keymember 非常相似，只不过它将删除列表中找到的第一个元组（如果存在），并返回剩余的列表：  

```
5> lists:keydelete(a, 2, [{x,y,z},{b,b,b},{b,a,c},{q,r,s}]).
[{x,y,z},{b,b,b},{q,r,s}]
```  

lists:keysearch 与 lists:keymember 类似，但是它将返回 {value,Tuple_Found} 或者原子值 false。  

lists 模块中还有许多非常有用的函数。  

Erlang 进程（概念上地）会一直运行直到它执行 receive 命令，而此时消息队列中又没有它想接收的消息为止。  
这儿，“概念上地” 是因为 Erlang 系统活跃的进程实际上是共享 CPU 处理时间的。  

当进程无事可做时，即一个函数调用 return 返回而没有调用另外一个函数时，进程就结束。另外一种终止进程的方式是调用 exit/1 函数。exit/1 函数的参数是有特殊含义的，我们稍后会讨论到。在这个例子中使用 exit(normal) 结束进程，它与程序因没有再调用函数而终止的效果是一样的。  

内置函数 whereis(RegisteredName) 用于检查是否已有一个进程注册了进程名称 RegisteredName。如果已经存在，则返回进程 的进程标识符。如果不存在，则返回原子值 undefined。  

到这儿，你应该已经可以看懂 messager 模块的大部分代码了。让我们来深入研究将一个消息从一个用户发送到另外一个的详细过程。  

当第一个用户调用 “sends” 发送消息时：  

```
messenger:message(fred, "hello")
```  

首先检查用户自身是否在系统中运行（是否可以查找到 mess_client 进程）：

```
whereis(mess_client) 
```  

如果用户存在则将消息发送给 mess_client：  

```
mess_client ! {message_to, fred, "hello"}
```  

客户端通过下面的代码将消息发送到服务器：  

```
{messenger, messenger@super} ! {self(), message_to, fred, "hello"},
```  

然后等待服务器的回复。
服务器收到消息后将调用：  

```
{messenger, messenger@super} ! {self(), message_to, fred, "hello"},
```  

接下来，用下面的代码检查进程标识符 From 是否在 User_Lists 列表中：  

```
lists:keysearch(From, 1, User_List)
```  

如果 keysearch 返回原子值 false，则出现的某种错误，服务将返回如下消息：  

```
From ! {messenger, stop, you_are_not_logged_on}
```  

client 收到这个消息后，则执行 exit(normal) 然后终止程序。如果 keysearch 返回的是 {value,{From,Nmae}} ，则可以确定该用户已经登录，并其名字（peter）存储在变量 Name 中。  

接下来调用：  

```
server_transfer(From, peter, fred, "hello", User_List)
```  

注意这里是函数 server\_transfer/5，与其它的 server\_transfer/4 不是同一个函数。还会再次调用 keysearch 函数用于在 User_List 中查找与 fred 对应的进程标识符：  

```
lists:keysearch(fred, 2, User_List)
```  

这一次用到了参数 2，这表示是元组中的第二个元素。如果返回的是原子值 false，则说明 fred 已经登出，服务器将向发送消息的进程发送如下消息：  

```
From ! {messenger, receiver_not_found};
```  

client 就会收到该消息。  

如果 keysearch 返回值为：   

```
{value, {ToPid, fred}}
```  

则会将下面的消息发送给 fred 客户端：  

```
ToPid ! {message_from, peter, "hello"}, 
```  

而如下的消息会发送给 peter 的客户端：  

```
From ! {messenger, sent} 
```  

Fred 客户端收到消息后将其输出：  

```
{message_from, peter, "hello"} ->
    io:format("Message from ~p: ~p~n", [peter, "hello"])
```  

peter 客户端在 await_result 函数中收到回复的消息。