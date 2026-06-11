     C*
     C* COPY FOR CHECK MISMATCH CODE IN F7301R
     C*     PLACE JUST AFTER CONDITIONING FOR WRITE TO FARPP
     C*     FOR THE BILL REVIEW SERVICES CHECK
     C*
     C* SETUP PARMS
     C**********
     C********** DATCHK    PLIST
     C**********           PARM           SRDTI   80
     C**********           PARM *BLANKS   SRFFMT  8
     C**********           PARM *BLANKS   SRTFMT  8
     C**********           PARM           SRDTO  10
     C**********           PARM           SRERR   1
     C**********
     C********** DTCYMD    PLIST
     C**********           PARM           SRDTI
     C**********           PARM *BLANKS   SRFFMT
     C**********           PARM '*CYMD'   SRTFMT
     C**********           PARM           SRDTO
     C**********           PARM           SRERR
     C**********
     C********** DTMDYY    PLIST
     C**********           PARM           SRDTI
     C**********           PARM '*CYMD'   SRFFMT
     C**********           PARM *BLANKS   SRTFMT
     C**********           PARM           SRDTO
     C**********           PARM           SRERR
     C*
     C                   MOVE      EOCO#         ##CO#
     C                   MOVE      EOSUB#        ##SUB#
     C                   MOVE      EOFND         ##FND
     C                   MOVE      EOFYR         ##FYR
     C                   MOVE      EOCASE        ##CASE
     C                   MOVE      EOEOB#        ##EOB#
     C*
     C                   MOVEL     Q1USER        ##USER
     C                   MOVEL     Q1PGM         ##PGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X#DT              7 0
     C                   Z-ADD     X#DT          ##DATE
     C                   Z-ADD     DSTIME        ##TIME
     C*
     C                   Z-ADD     RPAMT         ##CAMT
     C*
     C     RPAMT         IFGT      0
     C                   MOVE      'Y'           ##CHKW
     C                   ELSE
     C                   MOVE      'N'           ##CHKW
     C                   ENDIF
     C*
     C                   MOVE      'B'           ##FUNC
     C*
     C                   CALL      'XACMF73'
     C                   PARM                    ##FUNC            1
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##CASE            7 0
     C                   PARM                    ##EOB#            9 0
     C*
     C                   PARM                    ##PGM            10
     C                   PARM                    ##USER           10
     C                   PARM                    ##DATE            7 0
     C                   PARM                    ##TIME            6 0
     C*
     C                   PARM                    ##CAMT            9 2
     C                   PARM                    ##CHKW            1
     C*
