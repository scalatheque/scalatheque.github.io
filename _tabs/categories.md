---
icon: fas fa-stream
order: 1
---
<style>
details {
  border: 1px solid;
  border-radius: 4px;
  padding: 0;
  margin-bottom: 1rem;
  border-color: #cccccc;
}

details > details {
  margin-inline-start: 1rem;
}

summary {
  padding: 0.5rem 1rem;
  border-radius: 3rem;
  border-bottom: 0;

  &.hide-border-bottom {
    border-bottom-left-radius: 1;
    border-bottom-right-radius: 1;
  }

  border-radius: 50%;
  text-align: left;
  color: #6c757d !important;
}

details[open] {
  padding: 0.5em;
}

details[open] summary {
  margin-bottom: 0;
}
</style>
<!-- 
// https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
-->

<details id="syntax"> 
  <summary>Syntax</summary>
  <ul>
    <li>Anonymous functions</li>
    <li>Traits</li>
  </ul>
</details>
<details id="collections"> 
  <summary>Collections</summary>
  <details id="immutable-collections"> 
    <summary>Immutable collections</summary>
    <ul>
      <li>List</li>
      <li>Map</li>
      <li>Set</li>
    </ul>
  </details>
<details id="mutable-collections"> 
    <summary>Mutable collections</summary>
    <ul>
      <li>List</li>
      <li>Map</li>
      <li>Set</li>
    </ul>
  </details>
</details>
