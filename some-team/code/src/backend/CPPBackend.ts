import { execSync } from 'child_process'
import { unlinkSync, writeFileSync } from 'fs'
import { IBackend, Ref } from './IBackend'

const refdb: Map<symbol, string> = new Map()
const gensym = (
    idbase => () =>
        (sym => (refdb.set(sym, `val${idbase++}`), <Ref>sym))(Symbol('value'))
)(0)

refdb.set(Symbol.for('nil'), 'slapi::nil')

const outputs: Record<string, string[]> = {
    beforeatoms: [],
    atoms: [],
    afteratoms: [],
    atoms2str_after: [],
    code: [],
    main: [],
    aftermain: []
}
const emit = new Proxy<Record<keyof typeof outputs, (s: string) => void>>(<any>{}, {
    get(_v, name) {
        return (str: string) => {
            outputs[<string>name].push(str)
        }
    }
})
emit.beforeatoms('#include <stdlib.h>')
emit.beforeatoms('#include <string>')
emit.beforeatoms('namespace atom { enum atom { __dummy')
emit.afteratoms('}; }')
emit.afteratoms('static std::string atom2str(atom::atom aa) {')
emit.atoms2str_after('    abort();')
emit.atoms2str_after('}')
emit.atoms2str_after('#define HAS_ATOMS')
emit.atoms2str_after('#include <libsl.hpp>')
emit.main('int main() {')
emit.aftermain('    slapi::main_thread_go_bye();')
emit.aftermain('}')
const atoms = new Set<string>(['__dummy'])
function atom(s: string): string {
    if (atoms.has(s)) return 'atom::' + s
    emit.atoms(',' + s)
    emit.afteratoms(`    if (aa == atom::${s}) return ":${s}";`)
    atoms.add(s)
    return 'atom::' + s
}

export class CPPBackend implements IBackend {
    break(): void {
        emit.code('break;');
    }
    interpolate(val: (string | Ref)[]): Ref {
        const sym = gensym()
        emit.code(
            `    slref ${refdb.get(sym)} = slapi::from_str(${val
                .filter(e => e)
                .map(e => (typeof e == 'string' ? `std::string{${JSON.stringify(e)}}` : `slapi::format(${refdb.get(e)})`))
                .join(' + ')});`
        )
        return sym
    }
    seework(thr: string): Ref {
        const sym = gensym()
        emit.code(`    slref ${refdb.get(sym)} = slapi::seework(${atom(thr)});`)
        return sym
    }
    comment(work: Ref, comment: Ref): Ref {
        const sym = gensym()
        emit.code(`    slref ${refdb.get(sym)} = slapi::comment(${refdb.get(work)}, ${refdb.get(comment)});`)
        return sym
    }
    rfc(work: Ref): Ref {
        const sym = gensym()
        emit.code(`    slref ${refdb.get(sym)} = slapi::rfc(${refdb.get(work)});`)
        return sym
    }
    if(cond: Ref, then: () => void, else_: () => void): void {
        emit.code(`    if (slapi::to_bool(${refdb.get(cond)})) {`)
        then()
        emit.code(`} else {`)
        else_()
        emit.code(`}`)
    }
    equal(left: Ref, right: Ref): Ref {
        const sym = gensym()
        emit.code(
            `    slref ${refdb.get(sym)} = slapi::eq(${refdb.get(left)}, ${refdb.get(right)});`
        )
        return sym
    }
    add(left: Ref, right: Ref): Ref {
        const sym = gensym()
        emit.code(
            `    slref ${refdb.get(sym)} = slapi::add(${refdb.get(left)}, ${refdb.get(right)});`
        )
        return sym
    }
    symbol(s: string): Ref {
        const sym = gensym()
        emit.code(`    slref ${refdb.get(sym)} = slapi::from_symbol(${atom(s)});`)
        return sym
    }
    setvar(v: Ref, newval: Ref): void {
        emit.code(`    ${refdb.get(v)} = ${refdb.get(newval)};`)
    }
    pubwork(work: Ref): Ref {
        const sym = gensym()
        emit.code(`    slref ${refdb.get(sym)} = slapi::pubwork(${refdb.get(work)});`)
        return sym
    }
    numliteral(s: number): Ref {
        const sym = gensym()
        emit.code(`    slref ${refdb.get(sym)} = slapi::from_number(${s});`)
        return sym
    }
    boolliteral(s: boolean): Ref {
        const sym = gensym()
        emit.code(`    slref ${refdb.get(sym)} = slapi::from_bool(${s});`)
        return sym
    }
    declareloop(prebody: () => Ref, body: () => void): Ref {
        const sym = gensym()
        emit.code('while (true) {')
        emit.code('{')
        emit.code(`if (!slapi::to_bool(${refdb.get(prebody())})) break;`)
        emit.code('}')
        emit.code('{')
        body()
        emit.code('}')
        emit.code('}')
        return sym
    }
    declarevar(name: string, value: Ref): Ref {
        const sym = gensym()
        emit.code(`    slref fn_${name} = ${refdb.get(value)};`)
        refdb.set(sym, 'fn_' + name)
        return sym
    }
    strliteral(s: string): Ref {
        const sym = gensym()
        emit.code(`    slref ${refdb.get(sym)} = slapi::from_str(${JSON.stringify(s)});`)
        return sym
    }
    atlogsend(s: Ref): Ref {
        emit.code(`    slapi::log(${refdb.get(s)});`)
        return <Ref>Symbol.for('nil')
    }
    nilliteral(): Ref {
        return <Ref>Symbol.for('nil')
    }
    declarethread(name: string, generator: () => void): Ref {
        const sym = gensym()
        emit.code(`slref t${refdb.get(sym)}() {`)
        emit.code(`slapi::set_thread_name(${atom(name)});`)
        generator()
        emit.code(`return slapi::nil; }`)
        emit.main(`    new_thread_from_fn(t${refdb.get(sym)});`)
        return sym
    }
    callfn(funcref: Ref): Ref {
        throw new Error('Method not implemented.')
    }
    lookupfn(name: string): Ref {
        throw new Error('Method not implemented.')
    }
    declarefn(name: string, generator: () => void): Ref {
        throw new Error('Method not implemented.')
    }
}

export function cppemit() {
    const o: string[] = []
    for (let k of Object.values(outputs)) {
        for (let v of k) o.push(v)
    }
    writeFileSync('.tmp.cpp', o.join('\n'))
    execSync('clang++ .tmp.cpp -Isrc -std=c++2a -o ' + process.argv[3])
    // unlinkSync('.tmp.cpp')
}