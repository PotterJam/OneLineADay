<script>
    import { getContext } from 'svelte';
    import Login from './Login.svelte';

    const { open } = getContext('simple-modal');

    const loginModalOptions = {
        closeButton: false,
        styleWindow: { width: '800px', maxWidth: '100%' },
    };

    const openLoginModal = () => {
        open(Login, {}, loginModalOptions);
    };
</script>

<nav>
	<button class="loginBut" on:click={openLoginModal}>Login</button>
</nav>

<style>
	nav {
		height: 60px;
		background-color: rgb(53, 53, 53);
		display: flex;
		justify-content: flex-end;
		align-items: center;
	}

	.loginBut {
		background-color: rgb(80, 80, 80);
		border-radius: 0.4em;
		padding: 0.5em;
		margin: 1em;
		border: 0;
		color: white;
	}

	.loginBut:active {
		background-color: rgb(48, 48, 48);
	}
</style>