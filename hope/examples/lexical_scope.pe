let a = "global";
{
    let showA = fn() {
        print(a);
    };

    print(a) /* prints "global" */;
    showA() /* prints "global" */;
    let a = "local";
    showA() /* prints "global" */;
    print(a) /* prints "local" */;
};