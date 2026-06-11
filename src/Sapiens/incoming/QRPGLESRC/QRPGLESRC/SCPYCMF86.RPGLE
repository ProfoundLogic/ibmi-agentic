     C*
     C* COPY FOR CHECK MISMATCH CODE IN F0086R
     C*     PLACE JUST BEFORE CALL TO W0080R OR
     C*     IF USING F7301R JUST PRIOR TO COMPLETING THE EOB CYCLE
     C*
     C* SETUP PARMS
     C*
     C                   MOVE      UPCO#         ##CO#
     C                   MOVE      UPSUB#        ##SUB#
     C                   MOVE      UPFND         ##FND
     C                   MOVE      QQFYR         ##FYR
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
     C                   Z-ADD     QQTOTC        ##ETLC
     C     EOAMTC        SUB       EOAMT         ##ETLD
     C     ##ETLD        SUB       EOOTHR        ##ETLD
     C                   Z-ADD     EOOTHR        ##ETLO
     C                   Z-ADD     EONDIS        ##ETLN
     C                   Z-ADD     EOAMT         ##ETLA
     C     EOAMT         SUB       EONDIS        ##ETLP
     C                   Z-ADD     E1AMT3        ##PPAM
     C                   MOVE      E120A1        ##RCN
     C                   Z-ADD     EOEOB#        ##EOB#
     C*
     C                   CALL      'XACMF86'
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
     C                   PARM                    ##ETLC            9 2
     C                   PARM                    ##ETLD            9 2
     C                   PARM                    ##ETLO            9 2
     C                   PARM                    ##ETLN            9 2
     C                   PARM                    ##ETLA            9 2
     C                   PARM                    ##ETLP            9 2
     C                   PARM                    ##PPAM            9 2
     C                   PARM                    ##RCN            20
     C                   PARM                    ##EOB#            9 0
     C*
