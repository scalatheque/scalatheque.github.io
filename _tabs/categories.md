---
icon: fas fa-stream
order: 1
---
<style>
details {
  border: 1px solid #aaa;
  border-radius: 4px;
  padding: 0.5em 0.5em 0;
}

summary {
  font-weight: bold;
  margin: -0.5em -0.5em 0;
  padding: 0.5em;
}

details[open] {
  padding: 0.5em;
}

details[open] summary {
  border-bottom: 1px solid #aaa;
  margin-bottom: 0.5em;
}
</style>
<!-- 
// https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
-->

<details id="category1"> 
  <summary>Toggle me!</summary>
  Peek a boo!
</details>
<details id="category2"> 
  <summary>Toggle me too!</summary>
  <details id="category21"> 
    <summary>Toggle me 3!</summary>
    Peek a boo!
  </details>
</details>
