<script lang="ts">
  import { Router, Route, link } from "svelte-routing";
  import Home from './routes/Home.svelte';
  import Login from './routes/Login.svelte';
  import { updateLoginStatus, loggedIn, logout } from './Auth';

  export let url: string = "";

  updateLoginStatus();

  const processLogout = async () => {
    await logout();
  };
</script>

<Router url="{url}">
  <nav>
    <a href="/" use:link>Home</a>
    {#if $loggedIn}
      <!-- svelte-ignore a11y-missing-attribute -->
      <a href="login" use:link on:click={processLogout}>Logout</a>
    {:else}
      <a href="login" use:link>Login</a>
    {/if}
  </nav>

  <routes>
    <Route path="login"><Login /></Route>
    <Route path="/"><Home /></Route>
  </routes>
</Router>

<style>
  routes {
    height: 100%;
  }

  nav {
    height: 60px;
    background-color: rgb(53, 53, 53);
    display: flex;
    justify-content: flex-end;
    align-items: center;
  }

  a:link, a:visited {
    border: solid 2px rgb(80, 80, 80);
    background-color: rgb(80, 80, 80);
    border-radius: 0.4em;
    padding: 0.45em;
    margin: 1em;
    color: white;
    text-align: center;
    -webkit-transition-duration: 0.4s;
    transition-duration: 0.4s;
    text-decoration: none;
    font-size: 16px;
    cursor: pointer;
    display: inline-block;
  }

  a:hover, a:active {
    background-color: rgb(48, 48, 48);
    border: solid 2px rgb(139, 139, 139);
    text-decoration: none;
  }
</style>