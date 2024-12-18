%%
bool "BOOL"
int "INT"
real "REAL"
true "TRUE"
false "FALSE"
\^ "^"
\| "|"
type "TYPE"
\; ";"
, ","
\. "."
= "="
const "CONST"
: ":"
node "NODE"
function "FUNCTION"
returns "RETURNS"
var "VAR"
when "WHEN"
let "LET"
tel "TEL"
assert "ASSERT"
step "STEP"
not "NOT"
- "-"
pre "PRE"
current "CURRENT"
fby "FBY"
-> "->"
and "AND"
or "OR"
xor "XOR"
=\> "=>"
\<\> "<>"
\< "<"
\<= "<="
\> ">"
\>= ">="
div "DIV"
mod "MOD"
\+ "+"
/ "/"
\* "*"
if "IF"
then "THEN"
else "ELSE"
with "WITH"
# "#"
nor "NOR"
\( "("
\) ")"
\[ "["
\] "]"
merge "MERGE"
[_a-zA-Z][_a-zA-Z0-9]* "label"
[0-9][0-9]* "num"
[[:space:]]+ ;
\(\*([^\)]*([^\*]\))?)*\*\) ;
\\\*([^\\]*([^\*]\\)?)*\*\\ ;
--.* ;