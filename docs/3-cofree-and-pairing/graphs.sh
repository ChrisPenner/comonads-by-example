#!/bin/bash

# Installing graphviz on mac with image support: https://github.com/parrt/dtreeviz

# orange="#ed553c"
focused="#f3b134"
neutral="#B7D7D8"
bgfocused="#0b94a1"
selected="#48ab6c"

header=$(cat <<EOF
digraph {
    bgcolor=transparent;
    newrank=true;
EOF
)

footer="}"

graph(){
    cat <(echo "$header") - <(echo "$footer") | dot -Tpng -Gdpi=800 > ./images/"$1".png
}

graph tree-demo-1 <<EOF
a[label="a" penwidth="3" style="filled" fillcolor="$focused"]
{ rank=same
    b[label="b" penwidth="3" style=filled fillcolor="$selected"]
    d[label="d" penwidth="3" style=filled fillcolor="$selected"]
}
{
    rank=same
    c[label="c" penwidth="3" style=filled fillcolor="$selected"]
}
a-> b
a-> d
b -> c
EOF

