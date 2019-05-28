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

graph dep-analysis/quiver <<EOF
    color="$selected"
    penwidth="7"
    bgcolor="$bgfocused"
    label=""
    style=rounded;

    a[label="quiver" penwidth="3" style="filled" fillcolor="$selected"]
    b[label="arrow" penwidth="3" style=filled fillcolor="$selected"]
    c[label="bow" penwidth="3" style=filled fillcolor="$selected"]
    d[label="sticks" penwidth="3" style=filled fillcolor="$selected"]
    e[label="feather" penwidth="3" style=filled fillcolor="$selected"]
    f[label="stone" penwidth="3" style=filled fillcolor="$selected"]
    g[label="string" penwidth="3" style=filled fillcolor="$selected"]
    h[label="wool" penwidth="3" style=filled fillcolor="$selected"]
    i[label="wood" penwidth="3" style=filled fillcolor="$selected"]
    a -> b
    a -> c
    b -> d
    b -> e
    b -> f
    d -> i
    c -> d
    c -> g
    g -> h
EOF

graph dep-analysis/trace-quiver <<EOF
    color="$selected"
    penwidth="7"
    bgcolor="$bgfocused"
    label="trace [\"quiver\"] recipes"
    labelloc="t"
    style=rounded;

    a[label="quiver" penwidth="3" style="filled" fillcolor="$focused"]
    b[label="arrow" penwidth="3" style=filled fillcolor="$selected"]
    c[label="bow" penwidth="3" style=filled fillcolor="$selected"]
    d[label="sticks" penwidth="3" style=filled fillcolor="$neutral"]
    e[label="feather" penwidth="3" style=filled fillcolor="$neutral"]
    f[label="stone" penwidth="3" style=filled fillcolor="$neutral"]
    g[label="string" penwidth="3" style=filled fillcolor="$neutral"]
    h[label="wool" penwidth="3" style=filled fillcolor="$neutral"]
    i[label="wood" penwidth="3" style=filled fillcolor="$neutral"]
    a -> b
    a -> c
    b -> d
    b -> e
    b -> f
    d -> i
    c -> d
    c -> g
    g -> h
EOF

graph dep-analysis/traces-quiver <<EOF
    color="$selected"
    penwidth="7"
    bgcolor="$bgfocused"
    label="trace [\"quiver\"] $ recipes =>> traces id"
    labelloc="t"
    style=rounded;

    a[label="quiver" penwidth="3" style="filled" fillcolor="$focused"]
    b[label="arrow" penwidth="3" style=filled fillcolor="$selected"]
    c[label="bow" penwidth="3" style=filled fillcolor="$selected"]
    d[label="sticks" penwidth="3" style=filled fillcolor="$selected"]
    e[label="feather" penwidth="3" style=filled fillcolor="$selected"]
    f[label="stone" penwidth="3" style=filled fillcolor="$selected"]
    g[label="string" penwidth="3" style=filled fillcolor="$selected"]
    h[label="wool" penwidth="3" style=filled fillcolor="$neutral"]
    i[label="wood" penwidth="3" style=filled fillcolor="$neutral"]
    a -> b
    a -> c
    b -> d
    b -> e
    b -> f
    d -> i
    c -> d
    c -> g
    g -> h
EOF

graph dep-analysis/torches <<EOF
subgraph cluster_a {
    color="$selected"
    penwidth="7"
    bgcolor="$bgfocused"
    label=""
    style=rounded;

    a[label="torches" penwidth="3" style="filled" fillcolor="$focused"]
    b[label="sticks" penwidth="3" style=filled fillcolor="$selected"]
    c[label="wood" penwidth="3" style=filled fillcolor="$selected"]
    d[label="coal" penwidth="3" style=filled fillcolor="$selected"]
    a -> b
    a -> d
    b -> c
}
EOF
