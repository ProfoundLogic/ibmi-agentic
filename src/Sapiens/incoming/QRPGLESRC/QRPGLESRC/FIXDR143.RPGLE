     Fsmcgp     uf   e             disk
     Fsmhdp     uf   e             disk
     Fsmh1p     uf   e             disk
     Fsmpip     uf   e             disk
     Fsmppp     uf   e             disk
     Fsttbp     uf   e             disk
     F*
     C                   dou       *in99
     C                   read      smcgp                                  99
     C  n99              update    pcg01
     C                   enddo
     C*
     C                   dou       *in99
     C                   read      smhdp                                  99
     C  n99              update    phd01
     C                   enddo
     C*
     C                   dou       *in99
     C                   read      smh1p                                  99
     C  n99              update    ph101
     C                   enddo
     C*
     C                   dou       *in99
     C                   read      smpip                                  99
     C  n99              update    ppi01
     C                   enddo
     C*
     C                   dou       *in99
     C                   read      smppp                                  99
     C  n99              update    ppp01
     C                   enddo
     C*
     C                   dou       *in99
     C                   read      sttbp                                  99
     C  n99              update    ptb01
     C                   enddo
     C*
     C                   eval      *inlr = *on
