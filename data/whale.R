whale<-matrix(
      c(
              0,        0.0043,   0.1132,  0,
              0.9775,   0.9111,  0,        0,
              0,        0.0736,  0.9534,   0,
              0,        0,       0.0452,   0.9804
        ),
            nrow=4, byrow=TRUE,
     dimnames=list( c("yearling", "juvenile", "mature", "postreprod"),
                    c("yearling", "juvenile", "mature", "postreprod") )
     )
