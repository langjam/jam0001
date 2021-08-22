export type Ref = symbol & { __isref: true }

export interface IBackend {
    callfn(funcref: Ref): Ref;
    lookupfn(name: string): Ref;
    declarefn(name: string, body: () => void): Ref;
    declarethread(name: string, body: () => void): Ref;
    nilliteral(): Ref;
    strliteral(s: string): Ref;
    numliteral(s: number): Ref;
    boolliteral(v: boolean): Ref;
    atlogsend(s: Ref): void;
    declarevar(name: string, value: Ref): Ref;
    declareloop(prebody:() => Ref, body: () => void): void;
    pubwork(work: Ref): Ref;
    rfc(work: Ref): Ref;
    if(cond: Ref, then: () => void, else_: () => void): void;
    equal(left: Ref, right: Ref): Ref;
    add(left: Ref, right: Ref): Ref;
    symbol(s: string): Ref;
    setvar(v: Ref, newval: Ref): void;
    seework(thr: string): Ref;
    comment(work: Ref, comment: Ref): Ref;
    interpolate(items: (string | Ref)[]): Ref;
    break(): void;
}