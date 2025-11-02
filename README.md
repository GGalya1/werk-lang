# werk-lang: A Turing-Complete Educational Language
An educational project focused on designing and implementing a custom programming language, written entirely in **Racket** and based on the **PLAI-Typed** dialect.

---

## Project Goal: Turing Completeness

The main objective is to write a **Turing-Complete** programming language.

By "_Turing-Complete_," we mean the language must theoretically be capable of performing _any_ computation that a Universal Turing Machine can. In this project this is achieved through the presence of these core computational features:

1.  **State and Sequence:** The ability to store and mutate data (variables/memory) and execute a sequence of operations.
2.  **Conditional Branching:** The mechanism to perform different actions based on conditions (e.g., `if-then-else` logic).
3.  **Iteration/Recursion:** The capacity for unbounded repetition, typically implemented via **recursion** or **loops**, enabling the possibility of an infinite loop.
4.  **Unbounded Memory:** The theoretical capability to access an unlimited amount of memory (ignoring the practical limitations of physical hardware).

---

## Language Pipeline Overview

The computational pipeline of the `werk-lang` interpreter follows these steps:

1.  **Parsing (S-Expressions):** Racket’s parser reads the input text, breaks it down into **S-expressions**, and transforms them into the initial **Sugar Surface Language** (SSL) abstraction.
2.  **Desugaring:** The SSL representation undergoes **desugaring** (or "_de-harification_"), where all complex, syntactic sugar constructs are translated into a minimal set of core expressions.
3.  **Core Grammar Representation:** The resulting minimal language conforms to the tightly defined **Core Grammar** (Kernel) of the language.
4.  **Interpretation:** The expression in the Core Grammar is **evaluated** by the interpreter to produce a **final resulting value** (e.g., an integer, a boolean, etc.).