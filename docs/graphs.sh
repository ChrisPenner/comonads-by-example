#!/bin/bash

# orange="#ed553c"
focused="#f3b134"
neutral="#0b94a1"
selected="#48ab6c"

header=$(cat <<EOF
digraph {
    bgcolor=transparent;
    newrank=true;
EOF
)

footer="}"

graph(){
    cat <(echo "$header") - <(echo "$footer") | dot -Tpng -Gdpi=300 > ./images/"$1".png
}

graph tree <<EOF
a[label="" style="filled" fillcolor="$focused"]
b[label="" style=filled fillcolor="$selected"]
c[label="" style=filled fillcolor="$selected"]
a -> b
a -> c
EOF

graph list <<EOF
rankdir=LR;
bgcolor=transparent;
a[label="" style="filled" fillcolor="$neutral"]
b[label="" style="filled" fillcolor="$focused"]
c[label="" style="filled" fillcolor="$selected"]
d[label="" style="filled" fillcolor="$selected"]
a -> b
b -> c
c -> d
EOF

graph zipper <<EOF
rankdir=TB;
a[label="" style=filled fillcolor="$selected"]
b[label="" style=filled fillcolor="$selected"]
c[label="" style="filled" fillcolor="$focused"]
d[label="" style=filled fillcolor="$selected"]
e[label="" style=filled fillcolor="$selected"]
b -> a [constraint=false]
c -> b [constraint=false]
c -> d [constraint=false]
d -> e [constraint=false]
EOF


graph zipper-duplicate <<EOF
rankdir=LR;
concentrate=true;

subgraph cluster_0 {
    rank=same;
    color="$selected";
    a1[label="a" style=filled fillcolor="$selected"]
    b1[label="b" style="filled" fillcolor="$selected"]
    c1[label="c" style=filled fillcolor="$focused"]
    b1 -> a1 [constraint=false]
    c1 -> b1 [constraint=false]

}

subgraph cluster_1 {
    rank=same;
    color="$focused";
    a2[label="a" style=filled fillcolor="$selected"]
    b2[label="b" style="filled" fillcolor="$focused"]
    c2[label="c" style=filled fillcolor="$selected"]
    b2 -> a2 [constraint=false]
    b2 -> c2 [constraint=false]
}

subgraph cluster_2 {
    rank=same;
    color="$selected";
    a3[label="a" style=filled fillcolor="$focused"]
    b3[label="b" style="filled" fillcolor="$selected"]
    c3[label="c" style=filled fillcolor="$selected"]
    a3 -> b3 [constraint=false]
    b3 -> c3 [constraint=false]
}
b1 -> b2 [style=invis]
b2 -> b3 [style=invis]
EOF
