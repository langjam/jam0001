<script lang='ts'>
	import { Spacer } from '@ollopa/cedar'
	import { createEventDispatcher } from 'svelte'

	const dispatch = createEventDispatcher()
	const maxPages = 8

	let inputElement: HTMLInputElement
	let terminalInput: string
	let controlKeyPressed = false
	let error = null
	let page = 0

	const onTerminalKeyDown = (e: KeyboardEvent) => {
		switch (e.key) {
			case 'Control':
				controlKeyPressed = true
		}
	}

	const onTerminalKeyUp = (e: KeyboardEvent) => {
		switch (e.key) {
			case 'ArrowRight': {
				terminalInput = 'n'
				handleTerminalInput()
				break
			}
			case 'ArrowLeft': {
				terminalInput = 'b'
				handleTerminalInput()
				break
			}
			case 'Control': {
				controlKeyPressed = false
				break
			}				
			case 'Enter': {
				if (controlKeyPressed) {
					handleTerminalInput()
					break;
				}
			}
		}		
	}

	const handleTerminalInput = () => {
		error = null
		terminalInput = terminalInput.trim()

		switch (terminalInput) {
			case 'done':
			case 'skip':
			case 'exit': {
				dispatch('done')
				break
			}
			case 'b':
			case 'back': {
				page = Math.max(0, page - 1)
				break
			}				
			case 'n':
			case 'next': {
				page = Math.min(maxPages, page + 1)
				break
			}	
			default: {
				error = `unrecognized input: ${terminalInput}`
			}		
		}

		terminalInput = ''
		inputElement.focus()
	}
</script>

<div 
	class='container' 
	on:keydown={onTerminalKeyDown}
	on:keyup={onTerminalKeyUp}
	on:dblclick={handleTerminalInput}
>
	<div class="top">
		{#if page == 0}
			<h2>Literally Illiterate</h2>
			<p>a language where comments are first class citizens</p>
			<p>type <strong>next</strong> or <strong>n</strong> to continue to next page</p>
			<p>type <strong>back</strong> or <strong>b</strong> to go back</p>
			<p>type <strong>skip</strong> to bypass the quick intro</p>
			<p>press <strong>control+enter</strong> or <strong>double click</strong> the screen to submit</p>
		{:else if page == 1}
			<h2>First-Class Comments</h2>
			<p>a language with 'first-class comments' can mean many things</p>
			<p>i chose to interpret it quite literally</p>
			<p>in <strong>literally illiterate</strong>, you send comments to citizens sitting in first class of an airplane</p>
			<p>a comment is not a programming comment (like <strong>//</strong>), but rather a remark</p>
			<p>a valid comment would be, <strong>you dress well</strong></p>
		{:else if page == 2}
			<h2>Responding to Comments</h2>
			<p>a citizen <i>likes</i> your comment if it fits with their personality</p>
			<p><strong>kanye</strong>, for example, <i>likes</i> his ego being fed (compliments)</p>
			<p>if they like your comment, they will evaluate code that you give them</p>
		{:else if page == 3}
			<h2>Syntax</h2>
			<p>to send someone a comment with some code, follow this syntax</p>
			<p><strong><lit>(name)</lit>, <lit>(comment)</lit> "<lit>(code)</lit>"</strong></p>
			<p>if a citizen accepts your comment, they will evaluate your code</p>
			<p>to tell <strong>kanye</strong> he is beautiful, write</p>
			<p><strong>kanye, you are beautiful "3"</strong></p>
			<strong>3</strong> <span class='small'>{'< evaluation'}</span>
		{:else if page == 4}
			<h2>Citizens</h2>
			<p>Citizens have likes...</p>
			<span class='break'>
				- kanye likes compliments
				- linus likes to read hackernews headlines
				- socrates likes questions
				- tina likes a joke with a punchline
			</span>
			<p>... but also, each citizen <i>owns</i> a facet of the language</p>			
		{:else if page == 5}
			<h2>Ownership</h2>		
			<p>not only do they have to like your comment...</p>
			<p>
				...you have to make sure your <strong>code</strong>'s
				type is owned by them
			</p>
			<Spacer />
			<span class='break'>
				- kanye is literal <lit>3</lit>, <lit>'text'</lit>, <lit>true</lit>, <lit>false</lit>
				- linus operates <lit>+</lit>, <lit>-</lit>, <lit>/</lit>, <lit>*</lit>
				- tina compares <lit>and</lit>, <lit>or</lit>, <lit>{'<'}</lit>, <lit>{'>'}</lit>, <lit>{'<='}</lit>, <lit>{'>='}</lit>
				- socrates decides <lit>if else</lit>
			</span>	
		{:else if page == 6}
			<h2>Chaining</h2>
			<p>to do math or an if/else, you must chain</p>
			<p>chaining is too much to fit on this little screen</p>
			<p>so refer to <a target='_blank' href="https://github.com/kierangilliam/first-class-comments#chaining">this</a></p>
		{:else if page == 7}
			<h2>Events</h2>
			<p>oh yeah, and sometimes the citizens go to sleep</p>
			<p>when they are sleeping, they won't evaluate your code</p>
		{:else if page == 8}
			<h2>Stuck?</h2>
			<p>see the <a target='_blank' href='https://github.com/kierangilliam/first-class-comments'>repo</a></p>
			<p>type <strong>man</strong> for manual</p>
		{/if}
	</div>

	<div class='bottom'>
		{#if page == 0}
			<p><a target='_blank' href='https://github.com/langjam/langjam'>a langjam001 submission</a></p>
			<p>(best on desktop)</p>
		{:else if page == 2}
			<p>pssst, you can also arrow right/left to go forward/backward</p>
			<p>i just wanted you to get used to pressing control+enter</p>
		{:else if page == 3}
			<p>(once you exit the tutorial, you'll be able to send comments)</p>
		{:else if page == maxPages}
			<p>done. type <strong>exit</strong></p>
			<p>comeback any time by typing <strong>help</strong></p>
		{/if}

		{#if error}
			<div class='error'>{error}</div>
		{/if}

		<Spacer />
	
		<div>
			<span>$</span>
			<input autofocus type='text' bind:this={inputElement} bind:value={terminalInput}>
		</div>
	</div>
</div>

<style>
	.container {
		display: flex;
		flex-direction: column;
		justify-content: space-between;
		width: 100%;
		height: 100%;		
	}

	.break {
		white-space: pre-line;
	}

	h2 {
		text-transform: uppercase;
		letter-spacing: .25ch;
		word-spacing: 1ch;
	}

	.top p::before {
		content: '$ ';
		color: var(--gray);
	}
	
	.bottom p {
		font-size: var(--textSmall);
	}

	.small {
		color: var(--gray);
		font-size: .86rem;	
	}

	strong {
		color: var(--gold);
	}

	lit {
		font-weight: 400;
		color: var(--lightGray);
		border-radius: 2px;
		background: var(--darkGray);
	}

	input {
		width: 90%;
		color: var(--white);
		background: var(--darkerGray);
		color: var(--white);
		outline: 0;
		border: 0;
	}

	.error {
		color: var(--red);
	}

	a {
		color: var(--lightBlue)
	}
</style>