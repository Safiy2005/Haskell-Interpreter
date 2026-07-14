# RDF Query Language (RQL)

A lightweight domain-specific language (DSL) for querying and transforming RDF graphs, developed in Haskell for the University of Southampton COMP2322 coursework.

The project implements a complete interpreter including lexical analysis, parsing, semantic evaluation, graph transformations, aggregation and Turtle parsing.

> **Note:** This repository contains coursework completed as part of a group project. The [My Contributions](#my-contributions) section below identifies the parts I personally implemented.

## Features

- Custom query language inspired by SPARQL
- Alex-generated lexer
- Happy-generated LALR parser
- Pure functional evaluation pipeline
- Pattern matching over RDF triples
- Boolean filtering (`and`, `or`, `not`)
- Graph difference using `exists`
- Aggregation: `max`, `min`, `count`
- Graph construction using templates
- Canonical N-Triples output
- Support for Turtle features including:
  - Prefix declarations
  - Base URIs
  - Predicate lists
  - Object lists

## Example Query

```
from people

match
?person <http://example.org/ont/hasAge> ?age

where
?age >= 21

construct
?person <http://example.org/ont/hasAge> ?age
```

## Architecture

```
Lexer.x
   │
   ▼
Alex Lexer
   │
   ▼
Parser.y
   │
Happy Parser
   │
   ▼
Abstract Syntax Tree
   │
   ▼
Evaluation Engine
   │
   ▼
RDF Graph Output
```

## Technologies

- Haskell
- Stack
- Alex
- Happy

## Building

```
stack build
```

## Running

```
stack exec -- comp2322-coursework-exe <program-file>
```

Example:

```
stack exec -- comp2322-coursework-exe examples/query.rql
```

## Example Capabilities

- RDF graph union
- Graph difference
- Pattern matching
- Graph joins
- Anti-joins
- Aggregation
- RDF graph rewriting
- Predicate normalisation
- Type derivation

## Project Structure

| File | Description |
|------|--------------|
| `Lexer.x` | Alex lexer specification |
| `Parser.y` | Happy grammar |
| `Syntax.hs` | AST definitions |
| `Eval.hs` | Query evaluation |
| `Render.hs` | Output rendering |
| `Turtle.hs` | Turtle parser |
| `Main.hs` | CLI entry point |

## Learning Outcomes

This project strengthened my understanding of:

- Compiler construction
- Lexical analysis
- Parser generation
- Functional programming
- Domain-specific language design
- RDF and semantic web technologies
- Graph query processing

## My Contributions

This project was developed as part of a three-person university group project. My primary contributions included:

- Designed and implemented the core RQL interpreter in Haskell.
- Implemented the query evaluation pipeline, including pattern matching, joins, filtering, graph construction, and aggregation.
- Developed the Alex lexer and Happy parser, defining the language's syntax and grammar.
- Implemented support for boolean expressions (`and`, `or`, `not`) and the `exists` predicate for graph difference and anti-joins.
- Implemented RDF graph loading, canonical N-Triples output, and duplicate elimination.
- Wrote the majority of the project documentation, including the language manual.
- Contributed to testing, debugging, and language design decisions.

## Notes

This project was developed as part of the COMP2322 Programming Language Design module at the University of Southampton.
