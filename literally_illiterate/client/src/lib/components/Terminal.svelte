<script lang='ts'>
	import { createEventDispatcher } from 'svelte'
	import { writable } from 'svelte/store'
	import type { Readable } from 'svelte/store'
	import type { ProgramState } from '$lib/lang'
	import { clickOutside } from '$lib/actions'

	export let program: Readable<ProgramState>

	const dispatch = createEventDispatcher()
	const focused = writable(true)	

	let terminalInput: string = ''
	let historyOffset = 0
	let controlKeyPressed: boolean = false

	let inputElement: HTMLTextAreaElement

	focused.subscribe($focused => {
		if ($focused && inputElement) {
			inputElement.focus()
		}
	})

	const onInput = ({ resetHistory=false }) => (_?) => {
		if (resetHistory) historyOffset = 0
	}

	const submitInput = () => {
		dispatch('input', terminalInput)
		terminalInput = ''
	}

	const onTerminalClick = () => {
		$focused = true
	}

	const onTerminalKeyDown = (e: KeyboardEvent) => {
		switch (e.key) {
			case 'Control':
				controlKeyPressed = true
		}
	}

	const onTerminalKeyUp = (e: KeyboardEvent) => {
		const onArrowUpDown = () => {
			if (historyOffset > 0) {
				terminalInput = $program.history[$program.history.length - historyOffset].input
				onInput({ resetHistory: false })()
			} else {
				terminalInput = ''
			}
		}

		switch (e.key) {
			case 'Control': {
				controlKeyPressed = false
				break
			}				
			case 'ArrowUp': {
				historyOffset = Math.min($program.history.length, historyOffset + 1)
				onArrowUpDown()
				break;
			}
			case 'ArrowDown': {
				historyOffset = Math.max(0, historyOffset - 1)
				onArrowUpDown()
				break;
			}
			case 'Enter': {
				if (controlKeyPressed) {
					submitInput()
					break;
				}
			}
		}		
	}

	const onClickOutside = () => {
		$focused = false
	}
</script>

<div 
	class='terminal' 
	on:click={onTerminalClick} 
	on:keydown={onTerminalKeyDown}
	on:keyup={onTerminalKeyUp}
	on:dblclick={submitInput}
	use:clickOutside={onClickOutside}
>
	{#each $program.history as { input, output } }
		<p>{input}</p>
		<output>{output}</output>
	{/each}

	{#if $program.working}
		<span>...</span>
	{:else}
		<div class='terminal-input'>
			<span class='cursor'>$</span>
			<div>
				<!-- TODO grow rows with input
				https://css-tricks.com/the-cleanest-trick-for-autogrowing-textareas/
				-->
				<textarea 
					bind:this={inputElement} 
					data-gramm_editor="false"
					bind:value={terminalInput} 
					on:input={onInput({ resetHistory: true })}
					autofocus
					rows="3"
					cols="50"
				></textarea>
			</div>
		</div>
	{/if}	
</div>

<style>
	.terminal {
		overflow-y: scroll;		
		display: flex;
		flex-direction: column;
		justify-content: flex-end;
		width: 100%;
		height: 100%;		
	}

	p::before, .cursor {
		content: '$ ';
		color: var(--gray);
	}

	.terminal-input {
		display: flex;
		width: 100%;
	}

	output {
		white-space: pre-line;
	}

	.cursor { 
		margin-right: var(--s-2);
	}

	textarea {
		background: var(--darkerGray);
		color: var(--white);
		outline: 0;
		border: 0;
		width: 100%;
	}
</style>