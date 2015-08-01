#1 引言

这部分是用于帮助你快速使用 Erlang 的教程。这一节中所讲述的都是正确的，但是只是部分正确。例如，本节中只讲述了 Erlang 的一些简单语法形式，而没有包括所有复杂难懂的语法形式。此外，极大简化后的部分都使用 \*manual\* 标识出来了。这就也是说，这个主题的大部分内容都可以在这本 Erlang 的书中和 [Erlang Reference Manual](http://www.erlang.org/doc/reference_manual/introduction.html#erlang%20ref%20manual) 中找到。

##1.1 适用群体

我们假设本文的读者已经掌握了如下的知识：

+ 计算机的基本概论
+ 计算机编程的基本知识

##1.2 不包含的主题

本节中不包括如下的这些主题内容：

+ 引用
+ 局部错误处理 (catch/throw)
+ 单向链接 (monitor)
+ 二进制数据处理 (binaries/bit syntax)
+ 列表
+ 程序如何与外界难住，以及如何与其它语言写的软件进行通信；这部分内容在 [Interoperability Turorial](http://www.erlang.org/doc/tutorial/introduction.html#interoperability%20tutorial)。
+ Erlang 库 (例如，文件处理)
+ OTP 与 Mnesia 数据库
+ Erlang 语法的哈希表
+ 线上修改代码