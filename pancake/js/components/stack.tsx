import { StateInfo } from "../interpreter";

interface StackProps {
    state: StateInfo
}

export function Stack(props: StackProps) {
    return (
        <div style={{ textAlign: 'center'}}>
            <h1>Stack</h1>
            <div className='stack'>
                {
                    // TODO replace dummy with `props.state.currentStack`
                    dummyStack.map((value)=> {
                        return <div>{valueString(value)}</div>
                    })
                }
            </div>
        </div>
    )
}


function valueString(value: any) {
    if (typeof value === 'string') {
        return `"${value}"`;
    }
    if (typeof value === 'number') {
        return value.toString();
    }
    if (typeof value === 'object' && value.type === 'func') {
        return "<Function>";
    }
    if (typeof value === 'object' && value.type === 'name') {
        return `<Name: "${value.name}">`;
    }
    if (Array.isArray(value)) {
        const elements = value.map(valueString);
        return `[${elements.join(', ')}]`;
    }
}

const dummyStack: any[] = [
    'foobar',
    1,
    { type: 'func' },
    { type: 'name', name: 'map' },
    3,
    ['a', 2, 'c']
]