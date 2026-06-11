     C*
     C*  CHECK MISMATCH CODE IN F0070R
     C*     PLACE JUST BEFORE CALL TO F0086R
     C*
     C* SETUP PARMS
     C*
     C                   MOVE      UPCO#         ##CO#
     C                   MOVE      SCSUB#        ##SUB#
     C                   MOVE      SCFND         ##FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      SCCASE        ##CASE
     C                   MOVE      XXDCS#        ##DCS#
     C*
     C                   MOVEL     Q1USER        ##USER
     C                   MOVEL     Q1PGM         ##PGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X#DT              7 0
     C                   Z-ADD     X#DT          ##DATE
     C                   Z-ADD     DSTIME        ##TIME
     C*
     C                   MOVE      QQSYS         ##SYS
     C                   Z-ADD     QQTOTC        ##CTLC
     C                   Z-ADD     QQMRAR        ##CTLD
     C                   Z-ADD     QQOTHR        ##CTLO
     C                   Z-ADD     QQNDIS        ##CTLN
     C                   Z-ADD     QQAMT         ##CTLP
     C*
     C                   CALL      'XACMF70'
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##CASE            7 0
     C                   PARM                    ##DCS#            5 0
     C*
     C                   PARM                    ##PGM            10
     C                   PARM                    ##USER           10
     C                   PARM                    ##DATE            7 0
     C                   PARM                    ##TIME            6 0
     C*
     C                   PARM                    ##SYS             1
     C                   PARM                    ##CTLC            9 2
     C                   PARM                    ##CTLD            9 2
     C                   PARM                    ##CTLO            9 2
     C                   PARM                    ##CTLN            9 2
     C                   PARM                    ##CTLP            9 2
     C*
