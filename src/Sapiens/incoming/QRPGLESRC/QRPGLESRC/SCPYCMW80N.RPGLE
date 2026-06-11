     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT

     C*
     C* COPY FOR CHECK MISMATCH CODE IN W0080R
     C*     PLACE JUST BEFORE UNCONDITIONED WRITE TO WARPP
     C*     FOR THE NETWORK SERVICES CHECK
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*
     C**  IS WCMC INSTALLED ?
     C*
     C     ##MCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C*
     C*
     C* SETUP PARMS
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C                   MOVE      RPCO#         ##CO#
     C                   MOVE      XXSUB#        ##SUB#
     C                   MOVE      RPFND         ##FND
     C                   MOVE      RPFYR         ##FYR
     C                   MOVE      RPCASE        ##CASE
     C                   MOVE      RPEOB#        ##EOB#
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
     C                   MOVE      'N'           ##FUNC
     C*
     C                   CALL      'XACMW80'                              99
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
     C                   ENDIF                                                              '1'
