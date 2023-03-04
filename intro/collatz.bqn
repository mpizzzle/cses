#! /usr/bin/env bqn

Print ← •Out 2‿¯2 ↓´˜ •Fmt
input ← •BQN ⊑ •args

Collatz ← ⌽ {
  n ← (2⊸| ⊑ ÷⟜2 ⋈ 1+3⊸×) ⊑𝕩
  n (⊣ =⟜1 ◶ 𝕊‿⊢ ∾) 𝕩
}

Print Collatz input
