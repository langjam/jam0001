import { cubicOut } from 'svelte/easing';

// modified from: https://svelte.dev/repl/0ace7a508bd843b798ae599940a91783?version=3.16.7
export const clickOutside = (node: HTMLElement, handler: () => any): any => {
	const handleClick = event => {
			if (node && !node.contains(event.target) && !event.defaultPrevented) {
				handler()
			}
	}
	
	document.addEventListener('click', handleClick, true)

	return {
		destroy() {
			document.removeEventListener('click', handleClick, true);
		}
	}
}

// whatever this isn't an action. im tired
export function planeFly(node, { duration, y, x, out=false }) {
	return {
		duration,
		css: t => {
			const eased = 1 - cubicOut(t)
			const deg = eased * (out ? 35 : -35)

			return `
				transform: rotate(${deg}deg) translateX(${(eased) * x}px) translateY(${(eased) * y}px);
			`
		}
	};
}