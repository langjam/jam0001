import _rt from './src/runtime.ts'
_rt.registerthread('some_thread', function* () {
    let v_i = 0
    yield [
        'loop_inflate',
        _rt.loop(
            function* () {
                return true
            },
            function* () {
                const t0 = _rt.pubwork(v_i)
                yield 'just-yield'
                let v_work = t0
                yield [
                    'loop_inflate',
                    _rt.loop(
                        function* () {
                            return true
                        },
                        function* () {
                            let __t_t1 = _rt.rfc(v_work)
                            while (!__t_t1.has) yield ['rfc', __t_t1]
                            const t1 = __t_t1.value
                            let v_comment = t1
                            const t2 = _rt.sym('main_ack')
                            yield 'just-yield'
                            const t3 = _rt.equal(v_comment, v_comment)
                            yield 'just-yield'
                            _rt.if(
                                t3,
                                function* () {},
                                function* () {}
                            )
                            const t4 = _rt.interpolate(
                                ['str', 'Comment: '],
                                ['str', ''],
                                ['sym', 'v_comment']
                            )
                            yield 'just-yield'
                            _rt.atlog_send(t4)
                            yield 'just-yield'
                        }
                    ),
                ]
                const t5 = _rt.add(v_i, v_i)
                yield 'just-yield'
                v_i = t5
                yield 'just-yield'
            }
        ),
    ]
    yield ['ret', _rt.nil]
})
_rt.registerthread('main', function* () {
    yield [
        'loop_inflate',
        _rt.loop(
            function* () {
                return true
            },
            function* () {
                let __t_t6 = _rt.seework('some_thread')
                while (!__t_t6.has) yield ['rfc', __t_t6]
                const t6 = __t_t6.value
                let v_work = t6
                const t7 = _rt.sym('main_ack')
                yield 'just-yield'
                const t8 = _rt.comment(v_work, t7)
                yield 'just-yield'
                const t9 = _rt.interpolate(
                    ['str', "some_thread's work: "],
                    ['str', ''],
                    ['sym', 'v_work']
                )
                yield 'just-yield'
                _rt.atlog_send(t9)
                yield 'just-yield'
            }
        ),
    ]
    yield ['ret', _rt.nil]
})
