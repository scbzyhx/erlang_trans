#2 串行编程
##2.1 Erlang Shell

绝大多数操作系统都有命令解释器或者外壳 (shell)，Unix 与 Linux 系统中有很多的 shell, windows 上也有命令提示。 Erlang 自己的 shell 中可以直接编写 Erlang 代码，并被执行输出执行后的效果 (可以参考 STDLIB 中 [shell](http://www.erlang.org/doc/man/shell.html) 手册)。

在 Linux 或 Unix 操作系统中先启动一个 shell 或者命令解释器，再输入 erl 命令即可启动 erlang 的 shell。启动 Erlang 的 shell 的 shell 后，你可以看到如下的输出效果:  

```
% erl
Erlang R15B (erts-5.9.1) [source] [smp:8:8] [rq:8] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9.1  (abort with ^G)
1>
```  

在 shell 中输入 "2+5." 后，再输入回车符。请注意，输入字符 "." 与回车符的目的是通知 shell 你已经完成代码输入。

```
1> 2 + 5.
7
2>
```  

 如上所示,Erlang 给所有可以输入的行标了编号 (例如，>1，>2),上面的例子的意思就是 2+5 结果为 7。如果你在 shell 中输入错误的内容，则可以使用回退键将其删除，这一点与绝大多数 shell 是一样的。在 shell 下有许多编辑命令 (参阅 ERTS 用户指南中的 [tty - A command line interface](http://www.erlang.org/doc/apps/erts/tty.html) 文档)。  

（请注意，下面的这些示例中所给出的 shell 行号很多都是乱序的。这是因为这篇教程中的示例都是是使用的单独的测试过程）。  

下面是一个更加复杂的计算：  

```
2> (42 + 77) * 66 / 3.
2618.0
```  

请注意其中括号的使用，乘法操作符 “*” 与除法操作符 “/” 与一般算术运算中的含义与用法完全相同。(参见 [表达式](http://www.erlang.org/doc/reference_manual/expressions.html))。  

输入 Ctrl 与 C 键可以停止 Erlang 系统与交互式命令行 (shell)。  

下面给出的输出结果：  

```
BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
       (v)ersion (k)ill (D)b-tables (d)istribution
a
%
```  

输入 “a” 可以结束 Erlang 系统。  

关闭 Erlang 系统的另一种途径则是通过输入 halt() :  

```
3> halt().
%
```  

##2.2 模块与函数  

如果一种编程语言只能通过 shell 来运行代码，那么这种语言一个没有太大的用处。这里有一小段 Erlang 程序。使用合适的文本编辑器将其输入到文件 tut.erl 中。文件名称必须为 tut.erl 不能改变，并而需要将其放置于你启动 erl 命令时时所在的目录下。如果恰巧你的编辑器有 Erlang 模式的话，那么编辑器会帮助你优雅地组织和格式化你的代码 (参考 [Emacs 的 Erlang 模式](http://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html)),不过即使你没有这样的编辑器你也可以很好的管理你自己的代码。下面是待输入的代码：  

```
-module(tut).
-export([double/1]).

double(X) ->
    2 * X.
```  

很容易就可以看出来这个程序将数值翻倍。开始两行的代码稍后会解释。让我们来编译一下这段代码。我们可以在 Erlang shell 下来完成这个任务，其中 c 代表编译 (compile) 的意思：  

```
3> c(tut).
{ok,tut}
```  

｛ok,tut｝表示编译成功。如果返回 “error” 则表示输入的代码中存在错误。其它相关的错误信息可以帮助你弄清楚错误的位置和原因，然后修改它并重新编译。  

下面运行这个程序：  

```
4> tut:double(10).
20
```  

和预期结果一样，10 翻倍后就是 20。  

让我们先回到代码开始的这两行。存储在每个文件中的 Erlang 程序都包含一个 Erlang 模块。模块中代码的第一个行就是该模块的名字 (参见 [模块](http://www.erlang.org/doc/reference_manual/modules.html))  

```
-module(tut).
```  

因此，这个模块名为 tut。请注意该行代码结束后的句号 “.”。存储模块的文件必须与模块同名，并且以 “.erl” 作为扩展。在这个例子中，文件名为 tut.erl。如果使用到另外一个模块中的一个函数，可以使用如下的语法 module_name:function_name(arguments)。因此，下面的代码的含义也就是调用模块 tut 中的函数 double,并且输入的实参为 10。  

```
4> tut:double(10).
```  

第二行声明了 tut 模块中包含一个 double 函数，此函数接受一个参数 (在本例中为 x):  

```
-export([double/1]).
```  

第二行同时也行声明了这个函数可以在 tut 模块外被其它模块调用。后面会详细说明这一点。请再次注意这一行末尾的句号。  

接下来给出一个更加复杂的例子，计算一个数的阶乘。比如，4 的阶乘即为 4*3*2*1，等于 24。  

在 tut1.erl 文件中输入如下的一段代码：  

```
-module(tut1).
-export([fac/1]).

fac(1) ->
    1;
fac(N) ->
    N * fac(N - 1).
```  

所以在模块 tut1 中存在一个函数 fac，此函数接入一个输入参数 N。  

第一部分说明的是 1 的阶乘即为 1：  

```
fac(1) ->
    1;
```  

需要注意的是，这一部分是以分号结束的，这也就表示后面还有 fac 函数的更多内容。  

第二部分表示 N 的阶乘为 N 乘以 N-1 的阶乘：  

```
fac(N) ->
    N * fac(N - 1).
```  

与前面不同，这部分是以句号结尾的。这也就是说，后面没有这个函数更多的内容了。  

编译这个文件：  

```
5> c(tut1).
{ok,tut1}
```  

下面计算 4 的阶乘：  

```
6> tut1:fac(4).
24
```  

调用 tut1 模块中的 fac 函数，传入的参数为 4。  

函数也可以有多个参数，让我们扩展 tut1 模块，实现一个函数完成两个数相乘：  

```
-module(tut1).
-export([fac/1, mult/2]).

fac(1) ->
    1;
fac(N) ->
    N * fac(N - 1).

mult(X, Y) ->
    X * Y.
```  

需要注意，扩展模块时，需要修改 -export 这一行。在这一行中添加另外一个接受两个参数的函数 mult。  

编译：  

```
7> c(tut1).
{ok,tut1}
``` 

使用一下新函数：  

```
8> tut1:mult(3,4).
12
```  

在这个例子中，数字是整数值，代码中函数的参数 N、X 与 Y 被称之为变量。变量必须以大写字母开始 (参考[变量](http://www.erlang.org/doc/reference_manual/expressions.html))。Number、ShoeSize 和 Age 都是变量。  

##2.3 原子类型  

原子类型是 Erlang 语言中另一种数据类型。所有原子类型都以小写字母开头 (参见 [原子类型](http://www.erlang.org/doc/reference_manual/data_types.html))。例如，charles，centimeter，inch 等。原子类型就是名字而已，没有其它含义。它们与变量不同，变量拥有值，而原子类型没有。  

将下面的这段程序输入到文件 tut2.erl 中。这段程序完成英寸与厘米之间的相互转换：  

```
-module(tut2).
-export([convert/2]).

convert(M, inch) ->
    M / 2.54;

convert(N, centimeter) ->
    N * 2.54.
```  

编译：  

```
9> c(tut2).
{ok,tut2}
```  

测试：  

```
10> tut2:convert(3, inch).
1.1811023622047243
11> tut2:convert(7, centimeter).
17.78
```  

注意，到目前为止我们都没有介绍小数 (符点数) 的相关内容。希望你暂时先了解一下。  

让我们看一下，如果输入的参数既不是 centimeter 也不是 inch 时会发生什么情况：  

```
12> tut2:convert(3, miles).
** exception error: no function clause matching tut2:convert(3,miles) (tut2.erl, line 4)
```  

convert 函数的两部分被称之为函数的两个子句。正如你所看到的那样，miles 并不内子句的一部分。Erlang 系统找不到匹配的子句，所以返回了错误消息 function_clause。shell 负责被错误信息友好地输出，同时错误元组会被存储到 shell 的历史列表中，可以使用 v/1 命令将该列表输出：  

```
13> v(12).
{'EXIT',{function_clause,[{tut2,convert,
                                [3,miles],
                                [{file,"tut2.erl"},{line,4}]},
                          {erl_eval,do_apply,5,[{file,"erl_eval.erl"},{line,482}]},
                          {shell,exprs,7,[{file,"shell.erl"},{line,666}]},
                          {shell,eval_exprs,7,[{file,"shell.erl"},{line,621}]},
                          {shell,eval_loop,3,[{file,"shell.erl"},{line,606}]}]}}
```  

## 元组  

前面的 tut2 的程序不是一好的编程风格。例如：  

```
tut2.convert(3,inch)  
```  

这是意味着 3 本身已经是英寸表示了呢？还是指将 3 厘米转换成英寸呢？ Erlang 提供了将事件成组并用更易理解的方式表示的机制。它就是元组。一个元组由花括号括起来的。  

所以，{inch,3} 指的就是 3 英寸，而 {centimeter, 5} 指的就是 5 厘米。接下来，我们将重写厘米与英寸之间的转换程序。将下面的代码输入到文件 tut3.erl 文件中：  

```
-module(tut3).
-export([convert_length/1]).

convert_length({centimeter, X}) ->
    {inch, X / 2.54};
convert_length({inch, Y}) ->
    {centimeter, Y * 2.54}.
```  

编译并测试：  

```
14> c(tut3).
{ok,tut3}
15> tut3:convert_length({inch, 5}).
{centimeter,12.7}
16> tut3:convert_length(tut3:convert_length({inch, 5})).
{inch,5.0}
```  

请注意，第 16 行代码将 5 英寸转换成厘米后，再转换为就英寸，所以它得到原来的值。这也表明，一个函数实参可以是另一个函数的返回结果。仔细看一下，第 16 行的代码是怎么工作的。将 {inch,5} 参数传递给函数后，convert\_length 函数的首语句的头首先被匹配，也就是 convert_length({inch,5})  被匹配。也可以看作，{centimeter, X} 没有与 {inch,5} 匹配成功 ("->" 前面的内容即被称之为头部)。第一个匹配失败后，程序会尝试第二个语句，即 convert\_length({inch,5})。 第二个语句匹配成功，所以 Y 值也就为 5。

元组中可以有更多的元素，而不仅仅是上面描述的两部分。事实上，你可以在元组中，使用任意多的部分，只要每个部分都是合法的 Erlang 的项。例如，表示世界上不同城市的温度值：  

```
{moscow, {c, -10}}
{cape_town, {f, 70}}
{paris, {f, 28}}
```  

这些元组中每个都有固定数目的项。元组中的每个项都被称之为一个元素。在元组  {moscow,{c,-10}} 中，第一个元素为 moscow 而第二个元素为 {c,-10}。其中，c 表示摄氏度，f 表示华氏度。
