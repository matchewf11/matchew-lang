```orca
add x y = + x y;
sub x y = - x y;
```

Grammer:

```
ident: [^\s-]+
declaration: (<ident>)+ = <expr>;
```

AST:

```haskell
data Program = Program [Statements]

data Statement
  = StatementExpression Expression
  | StatementDeclaration

data Expression
  = Infix Expression OP Expression
  | ...
```
