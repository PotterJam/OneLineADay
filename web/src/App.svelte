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
	<div class="gradient-div">
		<div class="live-lines-container">
			{#each lipsumLines as line}
				<span class="live-line">{line}</span>
			{/each}
		</div>
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

	.gradient-div {
		background-color: rgb(57, 57, 58);
		display: flex;
		margin-top: 2em;
		flex: 1;
		width: 100%;
		justify-content: center;
	}

	.live-lines-container {
		flex: 1;
		max-width: 800px;
		background-color: rgb(246, 251, 253);
		margin: 3.5em 0 0 0;
		padding: 1em;
		border-radius: 30px 30px 0 0;
		display:flex;
		align-items: flex-start;
		align-content: flex-start;
		flex-wrap: wrap;
	}

	.live-line {
		max-width: 700px;
		background-color: #ffffff;
		border-radius: 0 0.5em 0.5em 0.5em;
		padding: 0.5em 0.8em;
		margin: 0.6em;
		border: 3px solid #585bd6;
		box-shadow: 0 0 0 1px #4143a7;
		white-space: nowrap;
		overflow: hidden;
		text-overflow: ellipsis;
	}
</style>