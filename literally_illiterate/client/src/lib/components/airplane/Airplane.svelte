<script lang='ts'>
	import type { Readable } from 'svelte/store'
	import type { ProgramState } from '$lib/lang'
	import { onMount } from 'svelte';

	export let program: Readable<ProgramState>

	let plane: HTMLDivElement
	 
	onMount(() => {
		const magnitude = 45
		const slowdown = 1000

		const animate = () => {
			if (!plane) return

			const t = Date.now() / slowdown

			// figure 8
			plane.style.left = (Math.cos(t)) * magnitude + 'px'
			plane.style.top = (Math.sin(2 * t) / 2) * magnitude + 'px'
		}
		
		setInterval(() => {
			requestAnimationFrame(animate)
		}, 150)
	})
</script>

<div class='wrapper'>
	<div bind:this={plane} class='container'>
		<img src='/seats.png' alt='seats'>		
		{#each Object.entries($program.world.citizens) as [name, state]}
			<img src={`${name}-${state.type}.png`} alt={`${name}-${state.type}`}>
		{/each}
		<img src='/plane.png' alt='seats'>
	</div>
</div>

<style>
	.wrapper {
		position: fixed;
		left: 50vw;
		transform: translateX(-50%);
		z-index: var(--z-index);
		width: 750px;
		height: 250px;
	}

	.container, img {
		position: absolute;
		top: 0;
		left: 0;
		width: 100%;
		height: 100%;
	}

	.container {
		width: 300%;
		height: 300%;
		transform: translate(-15%, -51%);
		transition: left 250ms ease-in, top 250ms ease-out;
	}
</style>