#lang brag

grammar: section+
section: DOUBLE-QUOTED /"⟨" @item+ /"⟩"
item: section | rule
rule: SYMBOL /"→" alternative+ /";"
alternative: /"|" @expansion [comment]
comment: /"," double-bracket
expansion: @string | or
string: s-expression+
@s-expression: primary | postfix
postfix: primary @postfix-operator
@primary: symbol | double-bracket | list | quoted | optional | group
symbol: SYMBOL
double-bracket: DOUBLE-BRACKET
quoted: /"'" SYMBOL
postfix-operator: "…" | "…+"
list: /"(" @expansion /")"
optional: /"[" @expansion /"]"
or: primary (/"∨" primary)+
group: /"{" @expansion /"}"