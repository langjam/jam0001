import { IBackend, Ref } from './IBackend'

const refdb: Map<symbol, string> = new Map()
const gensym = (
    idbase => () =>
        (sym => (refdb.set(sym, `val${idbase++}`), <Ref>sym))(Symbol('value'))
)(0)

refdb.set(Symbol.for('nil'), 'nil')

export class LogBackend implements IBackend {
    interpolate(val: (string | Ref)[]): Ref {
        const sym = gensym()
        console.log(
            `    ${refdb.get(sym)} <- concat ${val
                .filter(e => e)
                .map(e => (typeof e == 'string' ? `\`${e}\`` : refdb.get(e)))
                .join(' ')}`
        )
        return sym
    }
    seework(thr: string): Ref {
        const sym = gensym()
        console.log(`    ${refdb.get(sym)} <- seework ${thr}`)
        return sym
    }
    comment(work: Ref, comment: Ref): Ref {
        const sym = gensym()
        console.log(`    ${refdb.get(sym)} <- comment ${refdb.get(work)}, ${refdb.get(comment)}`)
        return sym
    }
    rfc(work: Ref): Ref {
        const sym = gensym()
        console.log(`    ${refdb.get(sym)} <- rfc ${refdb.get(work)}`)
        return sym
    }
    if(cond: Ref, then: () => void, else_: () => void): void {
        console.log(`.if ${refdb.get(cond)}`)
        then()
        console.log(`.else`)
        else_()
        console.log(`.endif`)
    }
    equal(left: Ref, right: Ref): Ref {
        const sym = gensym()
        console.log(
            `    ${refdb.get(sym)} <- ${refdb.get(left)} eq ${refdb.get(right)}`
        )
        return sym
    }
    add(left: Ref, right: Ref): Ref {
        const sym = gensym()
        console.log(
            `    ${refdb.get(sym)} <- ${refdb.get(left)} plus ${refdb.get(right)}`
        )
        return sym
    }
    symbol(s: string): Ref {
        const sym = gensym()
        console.log(`    ${refdb.get(sym)} <- :${s}`)
        return sym
    }
    setvar(v: Ref, newval: Ref): void {
        console.log(`    ${refdb.get(v)} <- ${refdb.get(newval)}`)
    }
    pubwork(work: Ref): Ref {
        const sym = gensym()
        console.log(`    ${refdb.get(sym)} <- publish work ${refdb.get(work)}`)
        return sym
    }
    numliteral(s: number): Ref {
        const sym = gensym()
        console.log(`    ${refdb.get(sym)} <- ${s}`)
        return sym
    }
    boolliteral(s: boolean): Ref {
        const sym = gensym()
        console.log(`    ${refdb.get(sym)} <- ${s}`)
        return sym
    }
    declareloop(prebody: () => Ref, body: () => void): Ref {
        const sym = gensym()
        console.log(`.loop:`)
        console.log(`.body if ${refdb.get(prebody())}`)
        body()
        console.log(`.end:`)
        return sym
    }
    declarevar(name: string, value: Ref): Ref {
        const sym = gensym()
        console.log(`    ${name} <- ${refdb.get(value)}`)
        refdb.set(sym, name)
        return sym
    }
    strliteral(s: string): Ref {
        const sym = gensym()
        console.log(`    ${refdb.get(sym)} <- ${s}`)
        return sym
    }
    atlogsend(s: Ref): Ref {
        console.log(`    log ${refdb.get(s)}`)
        return <Ref>Symbol.for('nil')
    }
    nilliteral(): Ref {
        return <Ref>Symbol.for('nil')
    }
    declarethread(name: string, generator: () => void): Ref {
        const sym = gensym()

        console.log(`thread ${name}:`)
        generator()
        refdb.set(sym, `thread_${name}`)
        return sym
    }
    callfn(funcref: Ref): Ref {
        throw new Error('Method not implemented.')
    }
    lookupfn(name: string): Ref {
        throw new Error('Method not implemented.')
    }
    declarefn(name: string, generator: () => void): Ref {
        const sym = gensym()

        console.log(`${name}:`)
        generator()
        refdb.set(sym, `func_${name}`)
        return sym
    }
}
