# 并发编程  

## 3.1 进程  

相比于其它函数式编程语言，Erlang 的优势在于它的并发编程与分布式式编程。并发是指一个程序同时有多个线程在执行。例如，现代操作系统允许你同时使用文字处理、电子制表软件、邮件终端和打印任务。在任意一个时刻，系统中每个处理单元 (CPU) 都只有一个线程 (任务) 在执行，但是可以通过以一定速率交替执行这些线程使得这些它们看上去像是在同时运行一样。Erlang 中创建多线程非常简单，而且很容易就可以实现这些线程之间的通信。Erlang 中，每个执行的线程都称之为一个 process (即进程，注意与操作系统中的进程概念不太一样)。  

(注意：进程 (process) 被用于没有共享数据的执行线程的场景。而线程 (thread) 则被用于会共享数据的情况下。由于 Erlang 执行线程之间不共享数据，所以我们一般将其称之为进程。)  

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

如上所示，say_something 函数根据第二个参数指定的次数将第一个参数的值输出多次。函数 start 启动两个 Erlang 进程，其中一个将 “hello” 输出 3 次，另一个进程将 “goodbye” 输出三次。所有的进程中都调用了 say_something 函数。不过需要注意的是，要想使用一个函数启动一个进程，这个函数就必须导出此模块，同时必须使用 spawn 启动。  

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

请注意，这里并不是先输出 “goodbye” 三次后再输出 “goodbye” 三次。而是，第一个进程先输出一个 "hello"，然后第二个进程再输出一次 "goodbye"。接下来，第一个进程再输出第二个 "hello"。但是奇怪的是 <0.63.0> 到底是哪儿来的呢？一个函数的返回值是函数最后一个表达式的值，而 start 函数的第后一个表达式是：  

```
spawn(tut14, say_something, [goodbye, 3]).
```  

spawn 返回的是进程的标识符，简记为 pid。进程标识符是用来唯一标识 Erlang 进程的标记。所以说，<0.63.0> 也就是spawn 返回的一个进程标识符。下面一个例了就可会讲解如何使用进程标识符。  

另外，这个例子中 io:format 输出用的不是 ~w 而变成了 ~p。引用用户手册的说法：“~p 与 ~w 一样都是将数据按标准语法的格式输出，但是当输出的内容需要超出一行时，~p 在分行处可以表现得更加智能。此外，它还会尝试检测出列表中的可输出字符串并将按字符串输出”。  

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

这个进程会执行 tut15.pong 函数。Pong_PID 是 “pong” 进程的进程标识符。接下来，start 函数又创建了另外一个进程 ”ping“：  

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

请注意，在 end 之间并没能 ";"。  

Erlang 进程之间的消息可以是任何简单的 Erlang 项。比如说，可以是列表、元组、整数、原子、进程标识等等。  

每个进程都有独立的消息接收队列。新接收的消息被放置在接收队列的尾部。当进程执行 receive 时，消息中第一个消息与与 receive 后的第一个模块进行匹配。如果匹配成功，则将该消息从消息队列中删除，并执行该模式后面的代码。  

然而，如果第一个模式匹配失败，则测试第二个匹配。如果第二个匹配成功，则将该消息从消息队列中删除，并执行第二个匹配后的代码。如果第二个匹配也失败，则匹配第三个，依次类推，直到所有模式都匹配结束。如果所有匹配都失败，则将第一个消息留在消息队列中，使用第二个消息重复前面的过程。第二个消息匹配成功时，则执行匹配成功后的程序并将消息从消息队列中取出（将第一个消息与其余的消息继续留在消息队列中）。如果第二个消息也匹配失败，则尝试第三个消息，依次类推，直到消息队列最后一个消息。如果所有消息都处理结束（匹配失败或者匹配成功被移除），则进程阻塞，等待新的消息的到来。上面的过程将会一直重复下去。  

Erlang 实现是非常 “聪明” 的，它会尽量减少 receive 的每个消息与模式匹配测试的次数。  

让我们回到 ping pong 示例程序。  

“Pong” 一直等待接收消息。 如果收到原子值  finished，“Pong” 会输出 “Pong finished”，然后结束。如果收到如下形式的消息：  

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

这表示将消息（任何 Erlang 数据）发送到进程标识符为 Pid 的进程。  

将消息 pong 发送给进程 “ping” 后，“pong” 进程再次调用 pong 函数，这会使得再次回到 receive 等待下一个消息的到来。  

下面，让我们一起去看看进程 “ping”，回忆一下它是从下面的地方开始执行的：  

```
tut15:ping(3, Pong_PID)
```  

可以看一下 ping/2 函数，由于第一个参数的值是 3 而不是 0， 所以 ping/2 函数的第二个子句被执行（第一个子句的头为 ping(0,Pong_PID)，第二个子句的头部为 ping(N,Pong_PID)，因此 N 为3 ）。  

第二个子句将发送消息给 “pong” 进程：  

```
Pong_PID ! {ping, self()},
```

self() 函数返回当前进程（执行 self() 的进程）的进程标识符，在这儿为 “ping” 进程的进程标识符。（回想一下 “pong” 的代码，这个进程标识符值被存储在变量 Ping_PID 当中）  

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

N-1 使得第一参数逐渐减小到 0。当其值变为 0 后，ping/2 函数的第一个子句会被执行。  

```
ping(0, Pong_PID) ->
    Pong_PID !  finished,
    io:format("ping finished~n", []);
```

此时，原子值 finished 被发送至 “pong” 进程（会导致进程结束），同时将“ping finished” 被输出。随后，“Ping” 进程结束。  

## 3.3 注册进程名称  

上面的例子中，因为 “Pong” 在 “ping” 进程开始前已经创建完成，所以才能将 “pong” 进程的进程标识符作为参数传递给进程 “ping”。这也就说，“ping” 进程必须通过某种途径获得 “pong” 进程的进程标识符后才能将消息发送 “pong” 进程。然而，某些情况下，进程需要相互独立的启动，而这些进程之间又要求知道彼此的进程标识符，前面提到的这种方式就不能满足要求了。因此，Erlang 提供了为每个进程提供一个名称的机制，这样进程间通信就可以通过进程名来实现，而不需要知道进程的进程标识符了。为每个进程注册一个名称需要用到内置函数 register：  

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

ping/2 变成了 ping/1。这是因为不再需要参数 Pong_PID 了。  