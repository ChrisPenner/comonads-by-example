#!/bin/bash

graph(){
    dot -Tpng -Gdpi=300 > ./images/"$1".png
}

graph tree <<EOF
digraph {
a[label="" style="filled" fillcolor="red"]
b[label=""]
c[label=""]
a -> b
a -> c
}
EOF

graph list <<EOF
digraph {
rankdir=LR;
a[label=""]
b[label=""]
c[label="" style="filled" fillcolor="red"]
d[label=""]
e[label=""]
b -> a
c -> b
c -> d
d -> e
}
EOF


# graph linear <<EOF
# digraph {
#   rankdir=LR;
#   getPost -> getComments
#   getComments -> buildPage
# }
# EOF

# graph parallel <<EOF
# digraph {
#   rankdir=LR;
#   c [label="", shape=point]
#   getPost -> c [arrowhead=none]
#   getComments -> c [arrowhead=none]
#   c -> buildPage
# }
# EOF

# graph reactive <<EOF
# digraph {
#   rankdir=LR;
#   c [label="", shape=point]
#   "post\$" -> c [arrowhead=none, style=dotted]
#   "comments\$" -> c [arrowhead=none, style=dotted]
#   c -> buildPage [style=dotted]
#   buildPage -> renderPage [style=dotted]
# }
# EOF

# graph searchbox <<EOF
# digraph {
#   rankdir=LR;
#     wlf [label="withLatestFrom(_)"]
#     d [label="debounce(2000)", shape=rectangle, style=dotted]
#     sm [label="switchMap(_)"]
#     ic [label="inputChanged\$"]
#     fv [label="filterValues$"]
#     rs [label="runSearch(_)"]
#     rr [label="renderResults(_)"]

#     ic -> d
#     d -> wlf
#     wlf -> sm
#     sm -> rs [style=dotted]
#     sm -> rr
#     rs -> sm [style=dotted]
#     fv -> wlf
# }
# EOF


# graph subsearch <<EOF
# digraph {
#   rankdir=LR;
#     wlf [label="withLatestFrom(_)"]
#     d [label="debounce(2000)", shape=rectangle, style=dotted]
#     ic [label="inputChanged\$"]
#     fv [label="filterValues$"]

#     subgraph cluster_chunk {
#         sm [label="switchMap(_)"];
#         rs [label="runSearch(_)"];
#         rr [label="renderResults(_)"]
#         label="Search & Render";
#     }

#     ic -> d
#     d -> wlf
#     wlf -> sm
#     sm -> rs [style=dotted]
#     sm -> rr
#     rs -> sm [style=dotted]
#     fv -> wlf
# }
# EOF

# graph searchchunk <<EOF
# digraph {
#   rankdir=LR;

#   input

#    subgraph cluster {
#        sm [label="switchMap(_)"];
#        rs [label="runSearch(_)"];
#        label="Search & Render";
#    }

#    output

#   input -> sm
#   sm -> rs [style=dotted]
#   rs -> sm [style=dotted]
#   sm -> output
# }
# EOF

# graph func <<EOF
# digraph {
#   rankdir=LR;

#   subgraph cluster_composed {
#     rankdir=LR;
#     String ->  Bool
#     label="length >>> even"
#   }

#   subgraph cluster_even {
#       int2 [label="Int"]
#       bool2 [label="Bool"]
#       int2 -> bool2
#     label="even"
#   }

#   subgraph cluster_length {
#     str [label="String"]
#     int1 [label="Int"]
#     str -> int1
#     label="length"
#   }
# }
# EOF

# graph composed <<EOF
# EOF
