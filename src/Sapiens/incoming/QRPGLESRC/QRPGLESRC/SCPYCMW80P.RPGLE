     C*
     C* COPY FOR CHECK MISMATCH CODE IN W0080R
     C*     PLACE JUST BEFORE UNCONDITIONED WRITE TO WARPP
     C*     FOR THE PROVIDER CHECK
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
     C*
     C* SETUP PARMS
     C*
     C                   MOVE      RPCO#         #NCO#
     C                   MOVE      RPFND         #NFND
     C                   MOVE      RPFYR         #NFYR
     C                   MOVE      RPCASE        #NCASE
     C                   MOVE      RPEOB#        #NEOB#
     C*
     C                   MOVE      #NCO#         #ACO#
     C                   MOVE      #NFND         #AWCF#
     C*
     C                   CALL      'FA700R'                             99
     C                   PARM      *BLANK        #ARTCD            1
     C                   PARM                    #ACO#             3
     C                   PARM                    #AWCF#            3
     C                   PARM      *BLANKS       #ASUB#            5
     C                   PARM      *BLANKS       #AFND             3
     C*
     C                   MOVE      #ASUB#        #NSUB#
     C                   MOVE      #AFND         #NFND
     C*
     C                   MOVEL     Q1USER        #NUSER
     C                   MOVEL     Q1PGM         #NPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X#DT              7 0
     C                   Z-ADD     X#DT          #NDATE
     C                   Z-ADD     DSTIME        #NTIME
     C*
     C                   Z-ADD     RPAMT         #NCAMT
     C*
     C     RPAMT         IFGT      0
     C                   MOVE      'Y'           #NCHKW
     C                   ELSE
     C                   MOVE      'N'           #NCHKW
     C                   ENDIF
     C*
     C                   MOVE      'P'           #NFUNC
     C*
     C                   CALL      'XACMW80'                            99
     C                   PARM                    #NFUNC            1
     C                   PARM                    #NCO#             3 0
     C                   PARM                    #NSUB#            5 0
     C                   PARM                    #NFND             3 0
     C                   PARM                    #NFYR             3 0
     C                   PARM                    #NCASE            7 0
     C                   PARM                    #NEOB#            9 0
     C*
     C                   PARM                    #NPGM            10
     C                   PARM                    #NUSER           10
     C                   PARM                    #NDATE            7 0
     C                   PARM                    #NTIME            6 0
     C*
     C                   PARM                    #NCAMT            9 2
     C                   PARM                    #NCHKW            1
     C*
     C                   ENDIF                                                              '1'
