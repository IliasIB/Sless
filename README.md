# Sless
Implementation of the Sless project for the Comparative Programming Languages course.

## Implemented Features
I implemented the base requirements (DSL Traits, no-space printing, pretty printing, variables and lint tool-style checks), comments, mixins and nested style rules. 

## Project Details
Every case class ending on "AST" in the ASTTypes file is used to build the AST. All the traits ending on handler extend the corresponding DSL traits and are responsible for building the AST with the *AST case classes. The only exception are the CompilableHandler and the NestedCompilableHandler which are classes so they can be extended with the other *Handler traits.

## Extensibility

Overall most changes would be contained to the CompilableHandler file as it's responsible for starting the building of the AST and the compilation of it. Suppose for example that we want to add a new selector.

Adding new selector types would require adding a new case class that extends from selector and add an extra line that handles this case class in the compile/pretty method for selector case classes. For the nested variant an extra line is also required in rectifyParent and containsParent for a total 5 extra lines of code.

Take now a bigger addition like support at-rules. We would need to declare that a css file can contain rules and at-rules, which requires an extra case class in ASTTypes and a common trait they extend from. Furthermore the pretty/compile method for CssAST needs to be modified to support both types of rules. Beyond that we would just need to write extra code for the at-rules themselves but can reuse the code for normal rules and declarations (even nested ones).

## Extra

I made four extra tests to showcase that it is possible to use comments with nested style rules and lint tool-style checks in nested sless. These can be found in LessNestingLintCommentTest.

## Feedback on the Project

I made this project as soon as I could and I noticed that it was updated on Toledo quite a lot. This was rather frustrating as I always had to check for new changes. This was especially the case for the Nesting feature as one of the requirements for it was just changed. The project itself though was a breath of fresh air. It's been a long time that I was able to just program beyond the usual mathmatical programming. Doing it in Scala was also very fun, since I normally don't use anonymous functions that much. I quite like them now however!
