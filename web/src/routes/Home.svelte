<script lang="ts">
  import { scale } from "svelte/transition";
  import { flip } from "svelte/animate";
  import { loggedIn } from "../Auth"
  import { createLine, getLines, Line } from "../services/lines";
  import { onMount } from "svelte";

  let liveLines: Line[] = [];

  onMount(async () => {
    liveLines = await getLines();
  });

  let line = '';

  const submitLine = async () => {
    const newLine = await createLine(line);
    liveLines = [newLine, ...liveLines];
  };
</script>

<main>
  <h1>What will you write?</h1>
  <input bind:value={line} id="input-line" type="text" placeholder="There's no going back." />
  {#if $loggedIn}
    <button on:click={submitLine}>Submit</button>
  {/if}
  <div class="live-lines-content">
    <div class="live-lines-container">
      {#each liveLines as line, i (line.id)}
        <span
          animate:flip={{ duration: 300 }}
          out:scale={{ duration: 250 }}
          in:scale={{ duration: 250 }}
          class="live-line">{line.body}</span
        >
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
    font-size: x-large;
  }

  .live-lines-content {
    flex: 1;
    display: flex;
    overflow: auto;
    width: 100%;
    justify-content: center;
  }

  .live-lines-container {
    flex: 1;
    max-width: 700px;
    margin: 2em 0 0 0;
    padding: 1em;
    border-radius: 30px 30px 0 0;
    display: flex;
    align-items: flex-start;
    align-content: flex-start;
    flex-wrap: nowrap;
    flex-direction: column;
    height: max-content;
  }

  .live-lines-container:after {
    content: "";
    position: absolute;
    z-index: 1;
    bottom: 0;
    left: 0;
    pointer-events: none;
    background-image: linear-gradient(
      to bottom,
      rgba(255, 255, 255, 0),
      rgba(246, 251, 253, 1) 90%
    );
    width: 100%;
    height: 4em;
  }

  .live-line {
    max-width: 700px;
    border-radius: 0.5em;
    padding: 0.5em 0.8em;
    margin: 0.6em;
    background: rgb(209, 209, 209);
    background: linear-gradient(
      81deg,
      rgba(209, 209, 209, 1) 0%,
      rgba(240, 240, 240, 1) 100%
    );
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
</style>
