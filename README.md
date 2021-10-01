# North
## Examples
```forth
,doc "Here goes all libraries to import from the system" .doc
{
"io" 1 0 0 library
}
import

,doc "Here goes all libraries to be used unqualafied" .doc
{
io
}
include

,doc "Functions ending with ' use the top element to determine how many elements to take as input" .doc
,doc "Strings put a sequence of character points on the stack followed by the length of the string" .doc
,macro hello
    io "Hello, World!" print'
.macro
```
