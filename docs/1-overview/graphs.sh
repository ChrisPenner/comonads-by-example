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

graph list <<EOF
rankdir=LR;
bgcolor=transparent;
a[label="" penwidth="3" style="filled" fillcolor="$focused"]
b[label="" penwidth="3" style="filled" fillcolor="$selected"]
c[label="" penwidth="3" style="filled" fillcolor="$selected"]
a -> b
b -> c
EOF

graph zipper <<EOF
rankdir=TB;
a[label="" penwidth="3" style=filled fillcolor="$selected"]
b[label="" penwidth="3" style=filled fillcolor="$selected"]
c[label="" penwidth="3" style="filled" fillcolor="$focused"]
d[label="" penwidth="3" style=filled fillcolor="$selected"]
e[label="" penwidth="3" style=filled fillcolor="$selected"]
b -> a [constraint=false]
c -> b [constraint=false]
c -> d [constraint=false]
d -> e [constraint=false]
EOF

graph skill-tree-demo-1 <<EOF
a[label="Magic\n2/5" penwidth="3" style="filled" fillcolor="$focused"]
b[label="Fireball\n1/3" penwidth="3" style=filled fillcolor="$selected"]
e[label="Flamewall\n0/1" penwidth="3" style=filled fillcolor="$selected"]
c[label="Levitation\n1/2" penwidth="3" style=filled fillcolor="$selected"]
a -> b
a -> c
b -> e
EOF


graph skill-tree-demo-2 <<EOF
subgraph cluster_a {
    color="$selected"
    penwidth="7"
    bgcolor="$bgfocused"
    label="7 Total"
    style=rounded;

    a[label="Magic\n2/5 -> 3" penwidth="3" style="filled" fillcolor="$focused"]
    b[label="Fireball\n1/3 -> 2" penwidth="3" style=filled fillcolor="$selected"]
    e[label="Flamewall\n0/1 -> 1" penwidth="3" style=filled fillcolor="$selected"]
    c[label="Levitation\n1/2 -> 1" penwidth="3" style=filled fillcolor="$selected"]
    a -> b
    a -> c
    b -> e
}
EOF


graph skill-tree-demo-3 <<EOF
a[label="Magic (7)\n2/5\n" penwidth="3" style="filled" fillcolor="$neutral"]

subgraph cluster_a {
    color="$neutral"
    penwidth="7"
    bgcolor="$bgfocused"
    label="3 Total"
    style=rounded;

    b[label="Fireball\n1/3" penwidth="3" style=filled fillcolor="$focused"]
    e[label="Flamewall\n0/1" penwidth="3" style=filled fillcolor="$selected"]

}

c[label="Levitation\n1/2" penwidth="3" style=filled fillcolor="$neutral"]

a -> b
a -> c
b -> e
EOF

graph skill-tree-demo-4 <<EOF
a[label="Magic (7)\n2/5\n" penwidth="3" style="filled" fillcolor="$neutral"]
b[label="Fireball (3)\n1/3" penwidth="3" style=filled fillcolor="$neutral"]

subgraph cluster_a {
    color="$selected"
    penwidth="7"
    bgcolor="$bgfocused"
    label="1 Total"
    style=rounded;

    e[label="Flamewall\n0/1" penwidth="3" style=filled fillcolor="$focused"]
}

c[label="Levitation\n1/2" penwidth="3" style=filled fillcolor="$neutral"]

a -> b
a -> c
b -> e
EOF

graph skill-tree-demo-5 <<EOF
a[label="Magic (7)\n2/5\n" penwidth="3" style="filled" fillcolor="$focused"]
b[label="Fireball (3)\n1/3" penwidth="3" style=filled fillcolor="$selected"]
e[label="Flamewall (1) \n0/1" penwidth="3" style=filled fillcolor="$selected"]
c[label="Levitation (1)\n1/2" penwidth="3" style=filled fillcolor="$selected"]

a -> b
a -> c
b -> e
EOF


# graph list-demo-1 <<EOF
# rank=same;
# a[label="a" style="filled" fillcolor="$focused"]
# b[label="b" style=filled fillcolor="$selected"]
# c[label="c" style=filled fillcolor="$selected"]
# a-> b
# b -> c
# EOF

# graph list-demo-2 <<EOF
# compound=true;
# rankdir=LR;
# subgraph cluster_a {
#     color="$focused"
#     label="a'"
#     style=rounded;
#     a1[label="a" style="filled" fillcolor="$focused"]
#     b1[label="b" style=filled fillcolor="$selected"]
#     c1[label="c" style=filled fillcolor="$selected"]
#     a1-> b1
#     b1-> c1
# }

# subgraph cluster_b {
#     color="$selected"
#     label="b'"
#     style=rounded;
#     b2[label="b" style="filled" fillcolor="$focused"]
#     c2[label="c" style="filled" fillcolor="$selected"]
#     b2 -> c2
# }

# subgraph cluster_c {
#     color="$selected"
#     label="c'"
#     style=rounded;
#     c3[label="c" style=filled fillcolor="$focused"]
# }

# c1 -> b2 [ ltail=cluster_a, lhead=cluster_b ];
# c2 -> c3 [ ltail=cluster_b, lhead=cluster_c ];
# EOF


# graph tree <<EOF
# a[label="" penwidth="3" style="filled" fillcolor="$focused"]
# b[label="" penwidth="3" style=filled fillcolor="$selected"]
# c[label="" penwidth="3" style=filled fillcolor="$selected"]
# a -> b
# a -> c
# EOF

# graph tree-demo-1 <<EOF
# a1[label="a" penwidth="3" style="filled" fillcolor="$focused"]
# { rank=same
#     b1[label="b" penwidth="3" style=filled fillcolor="$selected"]
#     d1[label="d" penwidth="3" style=filled fillcolor="$selected"]
# }
# {
#     rank=same
#     c1[label="c" penwidth="3" style=filled fillcolor="$selected"]
# }
# a1-> b1
# a1-> d1
# b1 -> c1
# EOF

# graph tree-demo-2 <<EOF
# compound=true;
# subgraph cluster_a {
#     label="a'"
#     style=rounded;
#     a1[label="a" penwidth="3" style="filled" fillcolor="$focused"]
#     { rank=same
#         b1[label="b" penwidth="3" style=filled fillcolor="$selected"]
#         d1[label="d" penwidth="3" style=filled fillcolor="$selected"]
#     }
#     {
#       rank=same
#         c1[label="c" penwidth="3" style=filled fillcolor="$selected"]
#     }
#     a1-> b1
#     a1-> d1
#     b1 -> c1
# }

# subgraph cluster_b {
#     label="b'"
#     style=rounded;
#     b2[label="b" penwidth="3" style="filled" fillcolor="$focused"]
#     c2[label="c" penwidth="3" style="filled" fillcolor="$selected"]
#     b2 -> c2
# }

# subgraph cluster_d {
#     label="d'"
#     style=rounded;
#     d3[label="d" penwidth="3" style=filled fillcolor="$focused"]
# }

# subgraph cluster_c {
#     label="c'"
#     style=rounded;
#     c4[label="c" penwidth="3" style=filled fillcolor="$focused"]
# }


# { rank=same; b2; d3 }

# c1 -> b2 [ ltail=cluster_a, lhead=cluster_b ];
# d1 -> d3 [ ltail=cluster_a, lhead=cluster_d ];
# c2 -> c4 [ ltail=cluster_b, lhead=cluster_c ];
# EOF


# graph zipper-duplicate <<EOF
# rankdir=LR;
# concentrate=true;

# subgraph cluster_0 {
#     rank=same;
#     color="$selected";
#     a1[label="a" penwidth="3" style=filled fillcolor="$selected"]
#     b1[label="b" penwidth="3" style="filled" fillcolor="$selected"]
#     c1[label="c" penwidth="3" style=filled fillcolor="$focused"]
#     b1 -> a1 [constraint=false]
#     c1 -> b1 [constraint=false]

# }

# subgraph cluster_1 {
#     rank=same;
#     color="$focused";
#     a2[label="a" penwidth="3" style=filled fillcolor="$selected"]
#     b2[label="b" penwidth="3" style="filled" fillcolor="$focused"]
#     c2[label="c" penwidth="3" style=filled fillcolor="$selected"]
#     b2 -> a2 [constraint=false]
#     b2 -> c2 [constraint=false]
# }

# subgraph cluster_2 {
#     rank=same;
#     color="$selected";
#     a3[label="a" penwidth="3" style=filled fillcolor="$focused"]
#     b3[label="b" penwidth="3" style="filled" fillcolor="$selected"]
#     c3[label="c" penwidth="3" style=filled fillcolor="$selected"]
#     a3 -> b3 [constraint=false]
#     b3 -> c3 [constraint=false]
# }
# b1 -> b2 [style=invis]
# b2 -> b3 [style=invis]
# EOF

# graph upnext <<EOF
#     rankdir=LR
#     subgraph cluster_ricky {
#         label="2"
#         ricky[image="./images/blocks/orange-ricky.png" shape=none label=""];
#     }
#     subgraph cluster_smashboy {
#         label="1"
#         smashboy[image="./images/blocks/smashboy.png" shape=none label=""];
#     }
#     subgraph cluster_hero {
#         label="0"
#         color="$focused"
#         hero[image="./images/blocks/hero.png" shape=none label="" lp="1000,1000?"];
#     }
#     subgraph cluster_teewee {
#         label="Nothing"
#         teewee[image="./images/blocks/teewee.png" shape=none label=""];
#     }
#     ricky -> smashboy
#     smashboy -> hero
#     hero -> teewee
# EOF

