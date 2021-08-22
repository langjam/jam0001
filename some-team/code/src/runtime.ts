export type Term = string | number | Map<string, Term> | Term[] | null

class Thread {
    constructor(
        public iter: Iterator<string | [string, Term]>,
        public istack: Iterator<string | [string, Term]>[],
        public predicate: Term
    ) {}
}

const threads: Thread[] = []
const aliases: Map<string, number> = new Map()

export default {
    registerthread(
        tname: string,
        tfn: () => Iterator<string | [string, Term]>
    ) {
        threads.push(new Thread(tfn(), [], null))
        aliases.set(tname, threads.length - 1)
    },
    strlit(s: string): Term {
        return s
    },
    atlog_send(t: Term) {
        console.log(t)
    },
    nil: null,
    loop(
        cond: () => Iterator<string | [string, Term]>,
        body: () => Iterator<string | [string, Term]>
    ) {
        return {
            cond,
            body,
        }
    },
}
function runThreads() {
    while (threads.length) {
        for (const t of threads) {
            const r = t.iter.next()
            if (r.done) {
                if (t.istack.length) {
                    t.iter = t.istack.pop()
                    t.predicate = r.value
                    continue
                }
                threads.splice(threads.indexOf(t), 1)
                if (r.value[1] != null)
                    throw new Error('invalid return: ' + r.value[1])
                continue
            }
            if (typeof r.value == 'string') {
                if (r.value == 'just-yield') continue
                throw new Error('todo threxit: ' + r.value)
            } else {
                if (r.value[0] == 'ret') {
                    if (t.istack.length) {
                        t.iter = t.istack.pop()
                        t.predicate = r.value[1]
                        continue
                    }
                    console.log('tkill (overret)')
                    threads.splice(threads.indexOf(t), 1)
                    if (r.value[1] != null)
                        throw new Error('invalid return: ' + r.value[1])
                    continue
                }
                if (r.value[0] == 'loop_inflate') {
                    // r.value[1]
                    t.istack.push(t.iter)
                    console.log('enter')
                    const rv = r.value[1]
                    t.iter = (function* () {
                        while (true) {
                            {
                                let elem = rv.cond()
                                while (true) {
                                    const v = elem.next()
                                    if (v.done && v.value) return null
                                    if (v.done) break
                                    yield v
                                }
                            }
                            {
                                let elem = rv.body()
                                while (true) {
                                    const v = elem.next()
                                    if (v.done) break
                                    yield v
                                }
                            }
                        }
                    })()
                    continue
                }
                throw new Error('todo threxit: ' + r.value[0])
            }
        }
    }
}
process.nextTick(runThreads)