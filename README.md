# Sless
Implementation of the Sless project for the Comparative Programming Languages course.

## Implemented Features
I implemented the base requirements (DSL Traits, no-space printing, pretty printing, variables and lint tool-style checks), comments, mixins and nested style rules. I have also made two extra tests to showcase that it is possible to use comments with nested style rules and lint tool-style checks in nested sless. These can be found in "Pretty print nested with comments" in CommentTest and "Nested aggregate margins" in LessLintTest respectively.

## Project Details
Every class ending on "Class" is used to build the AST. The different case classes found in the Selector file (e.g. ClassName) exist for the same purpose. Selector, Value and Property also contain traits with that the DSL object extends to allow building the AST. On top of that the DSL object directly extends the Compilable trait to compile/pretty print the AST.
