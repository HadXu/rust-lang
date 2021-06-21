# A language write by Rust

一种Rust语言编写的迷你解释器。涵盖了当代语言大部分功能。下载体验一下吧
```shell
git clone https://github.com/HadXu/lang.git
cd lang
cargo run --bin main
```

## 主要内容

一张图来该概况就是
![](https://hadxu-blog-1251872278.cos.ap-shanghai.myqcloud.com/imgs/interpreter.png)

首先对源代码进行分析，对每一个字符进行分析，得到其**token**， 这个过程称之为```lexer analysis```。当得到每一个token之后，对其进行抽象语法树构造，这个过程称之为```parser```，得到抽象语法树之后就是计算。如果是直接计算**evaluation**，那么就称之为解释器，如果是转换成二进制文件，那就是编译器，一般来说编译器运行比解释器快，因为大量的时间被消耗在编译时间上，编译期间不仅仅生成二进制代码，更对二进制代码进行优化，对于编译器的内容下面有空再写，感觉编译器比解释器要困难很多。

得益于Rust语言强大的match匹配模式，编写解释器代码量并不是很多。总共写了2407行代码，11个文件。

## 语法一览

1. Let
```
let x = 4;
```

2. expression
```
let y = 3 * (3 * 3) + 10
```

3. bool
```
let x = 1 != 1
```

4. if else
```
if (1 > 2) { 10 } else { 20 }
```

5. return
```
return 2 * 5
```

6. function
```
let identity = fn(x) { x; }; identity(5)
```

7. closure
```
let newAdder = fn(x) {
  fn(y) { x + y };
}
let addTwo = newAdder(2);
addTwo(2);
```

8. builtin function
```
len("hello world")
```

9. array
```
let myArray = [1, 2, 3]; 
myArray[2]
```

10. hash
```
let two = "two";
{
  "one": 10 - 9,
  two: 1 + 1,
  "thr" + "ee": 6 / 2,
  4: 4,
  true: 5,
  false: 6
}
```

11. hash get
```
let key = "foo"; 
{"foo": 5}[key]
```
已经实现了现代语言中大部分的语法。

## 实现流程
1. token

定义了基本的Token类型，包括```let if ```等等关键字

2. lexer

将源代码转换成各种**Token**。

3. parser

将转换之后的token按照优先级构建抽象语法树，优先级高的在树底层，优先级低的在高层。

4. evaluator

对抽象语法树进行计算，利用现代的语言将值计算出来，整理成该语言的对象然后打印。

5. wasm(todo)
得益于rust的wasm，能够在浏览器直接使用。

# 参考
- [rs-monkey-lang](https://github.com/wadackel/rs-monkey-lang)
- [writing an INTERPRETER in go](https://archive-1251872278.cos.ap-shanghai.myqcloud.com/pdfs/thorsten-ball-writing-an-interpreter-in-go-2017pdf_compress.pdf)