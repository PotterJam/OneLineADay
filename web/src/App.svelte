<script>
	const lipsumLines = ['Lorem ipsum dolor sit amet',
		'consectetur adipiscing elit.',
		'Nullam suscipit velit turpis.',
		'Morbi fermentum tempus lacus, nec venenatis arcu malesuada idnec venenatis arcu malesuada idnec.',
		'Vestibulum interdum vestibulum urna ac elementum.',
		'Interdum et malesuada fames ac ante ipsum primis in faucibus.',
		'Fusce nec laoreet elit, quis maximus elit.',
		'Vivamus mattis nulla ac ipsum varius, at mollis justo vestibulum.',
		'Suspendisse sed nulla ut dolor vehicula vestibulum in non ante.',
		'Etiam sagittis sagittis lacinia.',
		'Ut non lectus quis odio dictum commodo.',
		'Quisque vel mauris semper, pellentesque augue non, viverra augue.',
		'Nunc et leo quis metus ornare viverra. Aliquam sem ex, tincidunt nec metus sit amet, placerat pellentesque ipsum.',
	];
</script>

<main>
	<h1>What will you write?</h1>
	<input id="input-line" type="text" placeholder="There's no going back."/>
	<div class="live-lines-container">
		{#each lipsumLines as line}
			<span class="live-line">{line}</span>
		{/each}
	</div>
</main>

<style>
	main {
		text-align: center;
		display: flex;
		flex-direction: column;
		align-items: center;
		height: 100%;
	}

	h1 {
		color: #141010;
		font-size: 4em;
	}

	#input-line {
		width: 500px;
		height: 70px;
		font-size: x-large;
		border: 10px solid;
		border-image-slice: 1;
		border-width: 5px;
		border-image-source: linear-gradient(to left, #743ad5, #d53a9d);
	}

	.live-lines-container {
		flex: 1;
		max-width: 800px;
		background-color: rgb(246, 251, 253);
		margin: 2em 0 0 0;
		padding: 1em;
		border-radius: 30px 30px 0 0;
		display:flex;
		align-items: flex-start;
		align-content: flex-start;
		flex-wrap: wrap;
		flex-direction: column;
	}

	.live-line {
		max-width: 700px;
		border-radius: 0.5em;
		padding: 0.5em 0.8em;
		margin: 0.6em;
		background: rgb(209,209,209);
		background: linear-gradient(81deg, rgba(209,209,209,1) 0%, rgba(240,240,240,1) 100%);
		white-space: nowrap;
		overflow: hidden;
		text-overflow: ellipsis;
		animation: bounceIn 2s;
	}

	@keyframes bounceIn {
		0% {
			transform: scale(0.1);
			opacity: 0;
		}
		60% {
			transform: scale(1.2);
			opacity: 1;
		}
		100% {
			transform: scale(1);
		}
	}
</style>