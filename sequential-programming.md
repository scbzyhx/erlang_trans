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

##2.5 列表  

虽然元组可以将数据组成一组，但是我们也需要表示数据列表。 Erlang 中的列表由方括号括起来表示。例如，世界上不同城市的温度列表就可以表示为：  

```
[{moscow, {c, -10}}, {cape_town, {f, 70}}, {stockholm, {c, -4}},
 {paris, {f, 28}}, {london, {f, 36}}]
```  

请注意，这个列表太长而不能放在一行中，但是这并没有什么关系。Erlang 允许在 “合理的地方” 换行，但是并不允许在一些 “不合理的方”，比如原子类型、整数、或者其它数据类型的中间。  

可以使用 “|” 查看部分列表。可以在下面的的例子来解释这种用法：  

```
17> [First |TheRest] = [1,2,3,4,5].
[1,2,3,4,5]
18> First.
1
19> TheRest.
[2,3,4,5]
```  

可以用 | 将列表中的第一个元素与列表中其它元素分离开。 First 值为 1 ， TheRest 的值为 [2,3,4,5]。  

下一个例子：  

```
20> [E1, E2 | R] = [1,2,3,4,5,6,7].
[1,2,3,4,5,6,7]
21> E1.
1
22> E2.
2
23> R.
[3,4,5,6,7]
```  

这个例子中，我们用 | 取得了列表中的前两个元素。如果你要取得的元素的数量超过了列表中元素的总数，将返回错误。请注意列表中特殊情况，空列表（没有元素）[]：  

```
24> [A, B | C] = [1, 2].
[1,2]
25> A.
1
26> B.
2
27> C.
[]
```  

在前面的例子中，我们用的是新的变量名而没有重复使用已有的变量名: First，TheRest，E1，R，A，B 或者 C。这是因为：在上下文环境下一个变量只能被赋值一次。稍后会介绍更多的内容。  

下面的例子中演示了如何获得一个列表的长度。将下面的代码保存在文件 tut4.erl 中：  

```
-module(tut4).

-export([list_length/1]).

list_length([]) ->
    0;    
list_length([First | Rest]) ->
    1 + list_length(Rest).
```  

编译并运行：  

```
28> c(tut4).
{ok,tut4}
29> tut4:list_length([1,2,3,4,5,6,7]).
7
```  

代码含义如下：  

```
list_length([]) ->
    0;
```  

空列表的长度显然为 0。  

```
list_length([First | Rest]) ->
    1 + list_length(Rest).
```  

一个列表中包含第一个元素 First 与剩余元素 Rest, 所以列表长度为 Rest 列表的长度加上 1。  

（高级内容：这并不是尾递归，还有更好地实现该函数的方法。）  

一般地，Erlang 中元组类型承担其它语言中记录或者结构体类型的功能。列表是一个可变长容器，与其它语言中的链表功能相同。  

Erlang 中没有字符串类型。因为，在 Erlang 中字符串可以用 Unicode 字符的列表表示。这也隐含的说明了列表 [97,98,99] 等价于字符串 “abc”。 Erlang 的 shell 是非常“聪明"的，它可以猜测出来列表所表示的内容，而将其按最合适的方式输出，例如：  

```
30> [97,98,99]
"abc"
```  

##2.6 映射 (Map)  

映射是从键到值的关联。这种关联方式是由 “#{” 与 “}” 括起来。创建一个字符串 "key" 到值 42 的映射的方法如下：  

```
1>#{ "key"=>42}.
  #{"key" => 42}
```  
 
让我们直接通过示例来看一些有意思的特性。  

下面的例子展示了使用映射来关联颜色与 alpha 通道，从而计算 alpha 混合(译注：一种让 3D 物件产生透明感的技术)的方法。将下面的代码输入到 color.erl 文件中：  

```  
-module(color).

-export([new/4, blend/2]).

-define(is_channel(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)).

new(R,G,B,A) when ?is_channel(R), ?is_channel(G),
                  ?is_channel(B), ?is_channel(A) ->
    #{red => R, green => G, blue => B, alpha => A}.

blend(Src,Dst) ->
    blend(Src,Dst,alpha(Src,Dst)).

blend(Src,Dst,Alpha) when Alpha > 0.0 ->
    Dst#{
        red   := red(Src,Dst) / Alpha,
        green := green(Src,Dst) / Alpha,
        blue  := blue(Src,Dst) / Alpha,
        alpha := Alpha
    };
blend(_,Dst,_) ->
    Dst#{
        red   := 0.0,
        green := 0.0,
        blue  := 0.0,
        alpha := 0.0
    }.

alpha(#{alpha := SA}, #{alpha := DA}) ->
    SA + DA*(1.0 - SA).

red(#{red := SV, alpha := SA}, #{red := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).
green(#{green := SV, alpha := SA}, #{green := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).
blue(#{blue := SV, alpha := SA}, #{blue := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).
```  

编译测试：  

```
1> c(color).
{ok,color}
2> C1 = color:new(0.3,0.4,0.5,1.0).
 #{alpha => 1.0,blue => 0.5,green => 0.4,red => 0.3}
3> C2 = color:new(1.0,0.8,0.1,0.3).
 #{alpha => 0.3,blue => 0.1,green => 0.8,red => 1.0}
4> color:blend(C1,C2).
 #{alpha => 1.0,blue => 0.5,green => 0.4,red => 0.3}
5> color:blend(C2,C1).
 #{alpha => 1.0,blue => 0.38,green => 0.52,red => 0.51}
```  

关于上面的例子的解释如下：  

```
-define(is_channel(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)).
```  

首先，上面的例子中定义了一个宏 is_channel，这个宏用的作用主要是方便检查。大多数情况下，使用宏目的都是为了方便使用或者简化语法。更多关于宏的内容可以参考[预处理](http://www.erlang.org/doc/reference_manual/macros.html)。  

```
new(R,G,B,A) when ?is_channel(R), ?is_channel(G),
                  ?is_channel(B), ?is_channel(A) ->
    #{red => R, green => G, blue => B, alpha => A}.
```  

函数 new/4 创建了一个新的映射，此映射将 red，green，blue 以及 alpha 这些健与初始值关联起来。其中，is_channel 保证了只有 0.0 与 1.0 之间的浮点数是合法数值 (包括 0.0 与 1.0)。注意，在创建新映射的时候只能使用 => 运行符。  

使用由 new/4 函数生成的任何颜色作为参数调用函数 blend/2，就可以得到该颜色的 alpha 混合结果。显然，这个结果是由两个映射来决定的。  

blend/2 函数所在的第一件是就是计算 alpha 通道：  

```
alpha(#{alpha := SA}, #{alpha := DA}) ->
    SA + DA*(1.0 - SA).
```  

使用 := 操作符取得键 alpha 相关联的值作为参数的值。映射中的其它键被直接忽略。因为只需要键 alpha 与其值，所以也只有它才会被检查。  

对于函数 red/2，blue/2 和 green/2 也是一样的：  

```
red(#{red := SV, alpha := SA}, #{red := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).
```  

唯一不同的是，每个映射参数中都有两个键会被检查，而其它键会被忽略。  

最后，让我们回到 blend/3 返回的颜色：  

```
blend(Src,Dst,Alpha) when Alpha > 0.0 ->
    Dst#{
        red   := red(Src,Dst) / Alpha,
        green := green(Src,Dst) / Alpha,
        blue  := blue(Src,Dst) / Alpha,
        alpha := Alpha
    };
```  

Dst 映射会被更新为一个新的通道值。更新已存在的映射键值对可以用 := 操作符。

##2.7 标准模块与使用手册  

Erlang 有大量的标准模块可供使用。例如，IO 模块中包含大量处理格式化输入与输出的函数。如果你需要查看标准模块的详细信息，可以在操作系统的 shell 或者命令行 (即开始 erl 的地方) 使用 erl -man 命令来查看。示例如下：  

```
% erl -man io
ERLANG MODULE DEFINITION                                    io(3)

MODULE
     io - Standard I/O Server Interface Functions

DESCRIPTION
     This module provides an  interface  to  standard  Erlang  IO
     servers. The output functions all return ok if they are suc-
     ...
```  

如果在系统上执行命令不成功，你也可以使用 Erlang/OTP 的在线文档。 在线文件也支持以 PDF 格式下载。在线文档位置在 [www.erlang.se (commercial Erlang)](www.erlang.se) 或 [www.erlang.org (open source)](www.erlang.org)。例如，Erlang/OTP R9B 文档位于：  

```
http://www.erlang.org/doc/r9b/doc/index.html
```  

##2.8 输出至终端  

使用例子来说明如何格式化输出到终端再好不过了，因此下面就用一个简单的示例程序来说明如何使用 io:format 函数。与其它导出的函数一样，你可以在 shell 中测试 io:format 函数：  

```
31> io:format("hello world~n", []).
hello world
ok
32> io:format("this outputs one Erlang term: ~w~n", [hello]).
this outputs one Erlang term: hello
ok
33> io:format("this outputs two Erlang terms: ~w~w~n", [hello, world]).
this outputs two Erlang terms: helloworld
ok
34> io:format("this outputs two Erlang terms: ~w ~w~n", [hello, world]).
this outputs two Erlang terms: hello world
ok
```  

format/2 (2 表示两个参数) 接收两个列表作为参数。一般情况下，第一个参数是一个字符串 (前面已说明，字符串也是列表)。除了 ~w 会按顺序被替换为第二个列表中的的项以外，第一个参数会被直接输出。每个 ~n 都会引起输出换行。如果正常输出， io:formate/2 函数会反正个原子值 ok。与其它 Erlang 函数一样，如果发生错误会直接导致函数崩溃。这并Erlang 中的错误，而是经过深思熟虑后的一种策略。稍后会看到，Erlang 有着非常完善的错误处理机制来处理这些错误。如果要练习，想让 io:format 崩溃并不是什么难事儿。不过，请注意，io:format 函数崩溃并不是说 Erlang shell 本身崩溃了。  

##2.9 完整示例

接下来，我们会用一个更加完整的例子来巩固前面学到的内容。假设你有一个世界上各个城市的温度值的列表。其中，一部分是以摄氏度表示，另一部分是华氏度表示的。首先，我们先将所有的温度都转换为用摄氏度表示，再将数据输出。  

```
%% This module is in file tut5.erl

-module(tut5).
-export([format_temps/1]).

%% Only this function is exported
format_temps([])->                        % No output for an empty list
    ok;
format_temps([City | Rest]) ->
    print_temp(convert_to_celsius(City)),
    format_temps(Rest).

convert_to_celsius({Name, {c, Temp}}) ->  % No conversion needed
    {Name, {c, Temp}};
convert_to_celsius({Name, {f, Temp}}) ->  % Do the conversion
    {Name, {c, (Temp - 32) * 5 / 9}}.

print_temp({Name, {c, Temp}}) ->
    io:format("~-15w ~w c~n", [Name, Temp]).
```  
 
```  
35> c(tut5).
{ok,tut5}
36> tut5:format_temps([{moscow, {c, -10}}, {cape_town, {f, 70}},
{stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
moscow          -10 c
cape_town       21.11111111111111 c
stockholm       -4 c
paris           -2.2222222222222223 c
london          2.2222222222222223 c
ok
```  

在分析这段程序前，请先注意代码中加入了一部分的注释。注释都是以 % 开始并一直到行末的。另外，-export([format_temps/1]) 只导出了函数 format_temp/1，其它函数都是局部函数。也就是说，这些函数在 tut5 外部是不见的。  

在 shell 测试程序时，输出被分割到了两行中，这是因为输入太长，在一行中放不下。  

第一次调用 format_temps 函数时，City 被赋予值 {moscow,{c,-10}}, Rest 表示剩余的列表。所以调用函数 print_temp(convert_to_celsius({moscow,{c,-10}}))。  

这里，convert_to_celsius({moscow,{c,-10}}) 调用的结果作为另一个函数 print_temp 的参数。当以这样嵌套的方式调用函数时，它们会从内到外计算。也就是说，先计算 convert_to_celsius({moscow,{c,-10}}) 得到以摄氏度表示的值 {moscow,{c,-10}}。接下来，convert_to_celsius 与前面例子中的 convert_length 函数类似。  

print_temp 函数调用 io:format 函数，~-15w 表示以宽度值 15 输出随后的项 (参见STDLIB 的 [IO](http://www.erlang.org/doc/man/io.html#fwrite-1) 手册。)

接下来，用列表剩余的元素作参数调用 format_temps(Rest)。这与其它语言中循环构造很类似 (是的，虽然这是的规递的形式，但是我们并不需要担心)。再调用 format_temps 函数时，City 的值为 {cape_town,{f,70}}，然后同样的处理过程再重复一次。上面的过程一直重复到列表为空为止，因为此时列表为空，会匹配 format_temps([]) 语句。此语句会简单的返回原子值 ok，最后程序结束。  

##2.10 匹配、Guards 与变量的作用域  

返回温度值列表最大值或最小值是非常有用的。在扩展程序实现该功能之前，让我们先看一下寻找列表中的最大值的方法：  

```
-module(tut6).
-export([list_max/1]).

list_max([Head|Rest]) ->
   list_max(Rest, Head).

list_max([], Res) ->
    Res;
list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->
    list_max(Rest, Head);
list_max([Head|Rest], Result_so_far)  ->
    list_max(Rest, Result_so_far).
37> c(tut6).
{ok,tut6}
38> tut6:list_max([1,2,3,4,5,7,4,3,2,1]).
7
```  


首先注意这里有两个名称安全相同的函数。但是，由于它们接受不同数目的参数，所以在 Erlang 中它们被当作安全不相同的函数。在你需要使用它们的时候，你使用名称/参数数量就可以，这里名称就是函数的名称，参数数量是指函数的参数的个数。这个例子中为 list_max/1 与 list_max/2。  

在本例中，遍历列表的中元素时“携带”一个值，即 Result_so_far。 list\_max/1 函数把列表中的第一个元素当作最大值元素，然后使用剩余的元素作参数调用函数 list\_max/2。在上面的例子中为 list\_max([2，3，4，5，6，7，4，3，2，1]，1)。如果你使用空列表或者非列表类型的数据作为实参调用 list\_max/1，则会产生一个错误。注意，Erlang 的哲学是不要在这类错误产生的地方处理它，而是在别的地方来处理。稍后会讨论更多的内容。  

在 list\_max/2 中，当 Head > Result\_so\_far 时，则使用 Head 代替 Result\_so\_far 继续调用函数。 when 用在函数的 -> 前时是一个特别的的词，它表示只有测试条件为真时才会用到函数的这一部分。这种类型的测试被称这为 guard。如果 guard 为假 (即 guard 测试失败)，则跳过此部分而使用函数的下一部分。这个例子中，如果 Head 不大于 Result\_so\_far 则必小于或等于。所以在函数的下一部分中不需要 guard 测试。  

可以用在 guard 中的操作符还包括：  
-  < 小于  
-  > 大于  
-  == 等于  
-  >= 大于或等于  
-  =< 小于或等于  
-  /= 不等于  
(详见 [Guard Sequences](http://www.erlang.org/doc/reference_manual/expressions.html))  

要将上面找最大值的程序修改为查找最小值元素非常容易，只需要将 > 变成 < 就可以了。(但是，最好将函数名同时也修改为 list\_min)  

前面我们提到过，每个变量在其作用域内只能被赋值一次。从上面的例子中也可以看到，Result\_so\_far 却被赋值多次。这是因为，每次调用一次 list\_max/2 函数都会创建一个新的作用域。在每个不同的作用域中，Result\_so\_far 都被当作完全不同的变量。  

另外，我们可以使用匹配操作符 = 创建一个变量并给这个变量赋值。因此，M = 5 创建了一个变量 M，并给其赋值为 5。如果在相同的作用域中，你再写 M = 6, 则会导致错误。可以在 shell 中尝试一下：  

```
39> M = 5.
5
40> M = 6.
** exception error: no match of right hand side value 6
41> M = M + 1.
** exception error: no match of right hand side value 6
42> N = M + 1.
6
```  

除了创建新变量化，匹配操作符另一个用处就是将 Erlang 项分开。  

```
43> {X, Y} = {paris, {f, 28}}.
{paris,{f,28}}
44> X.
paris
45> Y.
{f,28}
```  

如上，X 值为 paris,而 Y 的值为 {f,28}。  

如果同样用 X 和 Y 再使用一次，则会产生一个错误：  

```
46> {X, Y} = {london, {f, 36}}.
** exception error: no match of right hand side value {london,{f,36}}
```  

变量用来提高程序的可读性。例如，在 list\_max/2 函数中，你可以这样写：  

```
list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->
    New_result_far = Head,
    list_max(Rest, New_result_far);
```  

这样写，可以让程序更加清晰。  

##2.11 更多关于列表的内容  

| 操作符可以用于取列表中的首元素:  

```
47> [M1|T1] = [paris, london, rome].
[paris,london,rome]
48> M1.
paris
49> T1.
[london,rome]
```  

同时，| 操作符也可以用于在列表首部添加元素:  

```
50> L1 = [madrid | T1].
[madrid,london,rome]
51> L1.
[madrid,london,rome]
```  

使用 | 操作符操作列表的例子如下 - 翻转列表中的元素:  

```  
-module(tut8).

-export([reverse/1]).

reverse(List) ->
    reverse(List, []).

reverse([Head | Rest], Reversed_List) ->
    reverse(Rest, [Head | Reversed_List]);
reverse([], Reversed_List) ->
    Reversed_List.
```  

```
52> c(tut8).
{ok,tut8}
53> tut8:reverse([1,2,3]).
[3,2,1]
```  

思考一下，Reversed\_List 是如何被创建的。初始时，其为 []。随后，待翻转的列表的首元素被取出来再添加到 Reversed\_List 列表中,如下所示：  

```
reverse([1|2,3], []) =>
    reverse([2,3], [1|[]])

reverse([2|3], [1]) =>
    reverse([3], [2|[1])

reverse([3|[]], [2,1]) =>
    reverse([], [3|[2,1]])

reverse([], [3,2,1]) =>
    [3,2,1]
```  

lists 模块中包括许多操作列表的函数，例如，列表翻转。所以，在自己动手写操作列表的函数之前是可以先检查是否在模块中已经有了 (参考 STDLIB 中 [lists(3)](http://www.erlang.org/doc/man/lists.html) 手册)。  

下面让我们回到城市与温度的话题上，但是这一次我们会使用更加结构化的方法。首先，我们将整个列表中的温度都使用摄氏度表示：  

```
-module(tut7).
-export([format_temps/1]).

format_temps(List_of_cities) ->
    convert_list_to_c(List_of_cities).

convert_list_to_c([{Name, {f, F}} | Rest]) ->
    Converted_City = {Name, {c, (F -32)* 5 / 9}},
    [Converted_City | convert_list_to_c(Rest)];

convert_list_to_c([City | Rest]) ->
    [City | convert_list_to_c(Rest)];

convert_list_to_c([]) ->
```  

测试一下上面的函数：  

```
54> c(tut7).
{ok, tut7}.
55> tut7:format_temps([{moscow, {c, -10}}, {cape_town, {f, 70}},
{stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
[{moscow,{c,-10}},
 {cape_town,{c,21.11111111111111}},
 {stockholm,{c,-4}},
 {paris,{c,-2.2222222222222223}},
 {london,{c,2.2222222222222223}}]
``` 

含义如下：  

```
format_temps(List_of_cities) ->
    convert_list_to_c(List_of_cities).
```  

format_temps/1 调用 convert\_list\_to\_c/1。covert\_list\_to\_c/1 移除 List\_of\_cities 的首元素，并将其转换为摄氏单位表示 (如果需要)。| 操作符用来将被转换后的元素转换后的剩余列表中：  

```
[Converted_City | convert_list_to_c(Rest)];
```  

或者：  

```
[City | convert_list_to_c(Rest)];
```  

这个过程一直重复到列表结束。当列表为空时，则执行：  

```
convert_list_to_c([]) ->
    [].
```  

当列表被转换后，用新增的输出函数将其输出：  

```
-module(tut7).
-export([format_temps/1]).

format_temps(List_of_cities) ->
    Converted_List = convert_list_to_c(List_of_cities),
    print_temp(Converted_List).

convert_list_to_c([{Name, {f, F}} | Rest]) ->
    Converted_City = {Name, {c, (F -32)* 5 / 9}},
    [Converted_City | convert_list_to_c(Rest)];

convert_list_to_c([City | Rest]) ->
    [City | convert_list_to_c(Rest)];

convert_list_to_c([]) ->
    [].

print_temp([{Name, {c, Temp}} | Rest]) ->
    io:format("~-15w ~w c~n", [Name, Temp]),
    print_temp(Rest);
print_temp([]) ->
    ok.
```  

```
56> c(tut7).
{ok,tut7}
57> tut7:format_temps([{moscow, {c, -10}}, {cape_town, {f, 70}},
{stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
moscow          -10 c
cape_town       21.11111111111111 c
stockholm       -4 c
paris           -2.2222222222222223 c
london          2.2222222222222223 c
ok

```  

接下来，添加一个函数用于搜索拥有最高温度与最低温度值的城市。下面的方法并不是最高效的方式，因为它遍历了四次列表。但是首先应当保证程序的清晰性和正确性，然后才是提高程序的效率：  

```
-module(tut7).
-export([format_temps/1]).

format_temps(List_of_cities) ->
    Converted_List = convert_list_to_c(List_of_cities),
    print_temp(Converted_List),
    {Max_city, Min_city} = find_max_and_min(Converted_List),
    print_max_and_min(Max_city, Min_city).

convert_list_to_c([{Name, {f, Temp}} | Rest]) ->
    Converted_City = {Name, {c, (Temp -32)* 5 / 9}},
    [Converted_City | convert_list_to_c(Rest)];

convert_list_to_c([City | Rest]) ->
    [City | convert_list_to_c(Rest)];

convert_list_to_c([]) ->
    [].

print_temp([{Name, {c, Temp}} | Rest]) ->
    io:format("~-15w ~w c~n", [Name, Temp]),
    print_temp(Rest);
print_temp([]) ->
    ok.

find_max_and_min([City | Rest]) ->
    find_max_and_min(Rest, City, City).

find_max_and_min([{Name, {c, Temp}} | Rest], 
         {Max_Name, {c, Max_Temp}}, 
         {Min_Name, {c, Min_Temp}}) ->
    if 
        Temp > Max_Temp ->
            Max_City = {Name, {c, Temp}};           % Change
        true -> 
            Max_City = {Max_Name, {c, Max_Temp}} % Unchanged
    end,
    if
         Temp < Min_Temp ->
            Min_City = {Name, {c, Temp}};           % Change
        true -> 
            Min_City = {Min_Name, {c, Min_Temp}} % Unchanged
    end,
    find_max_and_min(Rest, Max_City, Min_City);

find_max_and_min([], Max_City, Min_City) ->
    {Max_City, Min_City}.

print_max_and_min({Max_name, {c, Max_temp}}, {Min_name, {c, Min_temp}}) ->
    io:format("Max temperature was ~w c in ~w~n", [Max_temp, Max_name]),
    io:format("Min temperature was ~w c in ~w~n", [Min_temp, Min_name]).
```  

```
58> c(tut7).
{ok, tut7}
59> tut7:format_temps([{moscow, {c, -10}}, {cape_town, {f, 70}},
{stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
moscow          -10 c
cape_town       21.11111111111111 c
stockholm       -4 c
paris           -2.2222222222222223 c
london          2.2222222222222223 c
Max temperature was 21.11111111111111 c in cape_town
Min temperature was -10 c in moscow
ok  
```  

##2.12 If 与 case  

上面的 find\_max\_and\_min 函数可以找到温度的最大值与最小值。这儿介绍一个新的结构 if。If 的工作方式如下：  

```
if
    Condition 1 ->
        Action 1;
    Condition 2 ->
        Action 2;
    Condition 3 ->
        Action 3;
    Condition 4 ->
        Action 4
end
```  

注意，在 end 之前没有 “;”。条件 (Condidtion) 的工作方式与 guard 一样，即测试并返回成功或者失败。Erlang 从最上面的条件开始测试一直到找到一个条件真。随后，执行该条件后的动作，且忽略其它在 end 前的条件与动作。如果所有条件都测试失败，则会产生运行时错误。一个测试恒为真的条件就是 true。它常用作 if 的最后一个条件，即当所有条件都测试失败时，则执行 true 后面的动作。  

下面这个例子说明了 if 的工作方式：  

```
-module(tut9).
-export([test_if/2]).

test_if(A, B) ->
    if 
        A == 5 ->
            io:format("A == 5~n", []),
            a_equals_5;
        B == 6 ->
            io:format("B == 6~n", []),
            b_equals_6;
        A == 2, B == 3 ->                      %That is A equals 2 and B equals 3
            io:format("A == 2, B == 3~n", []),
            a_equals_2_b_equals_3;
        A == 1 ; B == 7 ->                     %That is A equals 1 or B equals 7
            io:format("A == 1 ; B == 7~n", []),
            a_equals_1_or_b_equals_7
    end.
```  

测试该程序：  

```
60> c(tut9).
{ok,tut9}
61> tut9:test_if(5,33).
A == 5
a_equals_5
62> tut9:test_if(33,6).
B == 6
b_equals_6
63> tut9:test_if(2, 3).
A == 2, B == 3
a_equals_2_b_equals_3
64> tut9:test_if(1, 33).
A == 1 ; B == 7
a_equals_1_or_b_equals_7
65> tut9:test_if(33, 7).
A == 1 ; B == 7
a_equals_1_or_b_equals_7
66> tut9:test_if(33, 33).
** exception error: no true branch found when evaluating an if expression
     in function  tut9:test_if/2 (tut9.erl, line 5)
```

注意，tut9:test_if(33,33) 使得所有测试条件都失败，这将导致产生一个 if_clause 运行时错误。参考 [Guard 序列](http://www.erlang.org/doc/reference_manual/expressions.html) 可以得到更多 guard 测试的内容。  

Erlang 中还有一种 case 结构。回想一下前面的 convert_length 函数：  

```
convert_length({centimeter, X}) ->
    {inch, X / 2.54};
convert_length({inch, Y}) ->
    {centimeter, Y * 2.54}.
```  

该函数也可以用 case 实现，如下所示：  

```
-module(tut10).
-export([convert_length/1]).

convert_length(Length) ->
    case Length of
        {centimeter, X} ->
            {inch, X / 2.54};
        {inch, Y} ->
            {centimeter, Y * 2.54}
    end.
```  

无论是 case 还是 if 都有返回值。这也就是说，上面的例子中，case 语句要么返回 {inch,X/2.54} 要么返回 {centimeter,Y*2.54}。case 语句也可以用 guard 子句来实现。下面的例子可以帮助你分清二者。这个例子中，输入年份得到指定某月的天数。年份必须是已知的，因为闰年的二月有 29 天。  

```
-module(tut11).
-export([month_length/2]).

month_length(Year, Month) ->
    %% All years divisible by 400 are leap
    %% Years divisible by 100 are not leap (except the 400 rule above)
    %% Years divisible by 4 are leap (except the 100 rule above)
    Leap = if
        trunc(Year / 400) * 400 == Year ->
            leap;
        trunc(Year / 100) * 100 == Year ->
            not_leap;
        trunc(Year / 4) * 4 == Year ->
            leap;
        true ->
            not_leap
    end,  
    case Month of
        sep -> 30;
        apr -> 30;
        jun -> 30;
        nov -> 30;
        feb when Leap == leap -> 29;
        feb -> 28;
        jan -> 31;
        mar -> 31;
        may -> 31;
        jul -> 31;
        aug -> 31;
        oct -> 31;
        dec -> 31
    end
```  

```
70> c(tut11).
{ok,tut11}
71> tut11:month_length(2004, feb).
29
72> tut11:month_length(2003, feb).
28
73> tut11:month_length(1947, aug).
31
```  
