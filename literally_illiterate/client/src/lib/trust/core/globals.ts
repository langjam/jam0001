import 'reflect-metadata';

type StructTo<T> = Record<string, T>

type Globals = {
	impl: {
		fmt: StructTo<(x: unknown) => string>
		into: StructTo<StructTo<(x: unknown) => unknown>>
	}
}

export const GLOBALS: Globals = { 
	impl: { 
		fmt: {},
		into: {},
	},
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any, @typescript-eslint/ban-types
export type Constructor = { new (...args: any[]): object }
export type ConstructorOf<T> = new (...args: any[]) => T;

// https://stackoverflow.com/questions/40922531/how-to-check-if-a-javascript-function-is-a-constructor
const isConstructor = (x: unknown): x is Constructor => {
	try {
		// eslint-disable-next-line @typescript-eslint/ban-ts-comment
		// @ts-ignore
		new x()
	} catch (err) {
		if (err.message.indexOf('is not a constructor') >= 0) {
			return false
		}
	}
	return true
}

export class ImplStructNotFoundError extends Error {
	// eslint-disable-next-line @typescript-eslint/ban-types
	constructor (input: Constructor | Object, implName: string) {
		const constructor = isConstructor(input) ? input : input.constructor

		super(`${implName}: You forgot to add a @struct('${constructor.name}') to your class ${constructor.name}.`
			+ ' Remember, this should be the last decorator on the class.')
	}
}

// ///////////////////////https://stackoverflow.com/a/62471619/5770245
// TODO: To try: can't I just save this to the Globals?
// Globals.structs = Record where Mangled name points to given name?
// If that is possible, remove reflect-metadata
const nameKey = Symbol('name')

/**
 * To preserve class name though mangling.
 * @example
 * @struct('Customer')
 * class Customer {}
 * @param className
 * 
 * assertEq(getStructName(Customer), 'Customer')
 */
export function struct(className: string): ClassDecorator {
	return (Reflect as any).metadata(nameKey, className);
}

/**
 * @example
 * const type = Customer;
 * getName(type); // 'Customer'
 * @param type
 */
// eslint-disable-next-line @typescript-eslint/ban-types
export function getStructName(type: Function | Object): string {
	if (isConstructor(type)) 
		return (Reflect as any).getMetadata(nameKey, type);
	else
		return (Reflect as any).getMetadata(nameKey, type.constructor);
}
// ///////////////////////
