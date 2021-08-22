
<div class='container'>
	<img src='/computer.png' alt='computer'>
	<div class='screen'>
		<slot />
	</div>
</div>

<style>
	.container {
		position: fixed;
		bottom: -13%;
		left: 50vw;
		z-index: var(--z-index);
		transform: translateX(-50%);
	}

	.screen {
		position: absolute;
		top: 12.2%;
		left: 16%;
		width: 65%;
		height: 57%;
		mix-blend-mode: hard-light;
		transform: skew(2deg, 0deg);

		color: var(--white);
		font-family: var(--monoFont);
		font-size: var(--h4);
		padding: var(--s-4);
		padding-left: var(--s-6);
		padding-bottom: var(--s-6);
	}

	img {
		width: 900px;
		height: 800px;
	}

	@media only screen and (max-width: 600px) {
		.container {
			bottom: -22%;
		}
		img {
			height: 600px;	
			width: 600px;	
		}	
		.screen {
			overflow: scroll;
			font-size: 1rem;
		}
	}

	/* modified from https://blog.carbonfive.com/vintage-terminal-effect-in-css3/ */
	.screen:before {
      content : '';
      position : absolute;
      width : 110%;
	  top: 0;
	  left: calc(-1 * var(--s-6));
	  height: 60px;
      background : #fff;
	  --strength: .6;
	  --strength2: .58;
      background: 
	  	linear-gradient(
			  to bottom, rgba(255,0,0,0) 0%,
			  rgba(255,250,250, var(--strength)) 30%,
			  rgba(255,255,255, var(--strength2)) 71%,
			  rgba(255,0,0,0) 100%
		);
      opacity : .1;
      animation: vline 7.25s linear infinite;
    }

	@keyframes vline {
		0%   { 
			top: -60px;
			opacity: 0;
		}
		5% { opacity: .1; }
		95% { opacity: .1; }
		100% { 
			top: 90%;
			opacity: 0;
		}
	}	
</style>