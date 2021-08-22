# Documentation tests

This program implements documentation tests. It does so by looking through its
own comments, looking if those comments have any embedded code, and then running
that code through a new interpreter created at runtime.

You can recreate output file by running the command in this folder:

```shell
tontuna main.tnt --output output.txt
```

* [Read the source](./main.tnt)
* [Take a look at the output](./output.txt)
