     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/19/02 C:62 PROJECT: MC072 31085                            *
     H*               TRY TO CLEAR FXLOP AND IF CAN'T THEN DON'T      *
     H*               ALLOW USER TO EXECUTE THE BATCH.                *
     H*                                                               *
     H* 08/15/00 C:62 PROJECT: MC215                                  *
     H*               REPLACE SFBPF REFERENCES WITH APIS.             *
     H*                                                               *
     H* 01/25/99 C:62    REQ/PROJ: MC145                              *
     H*                  MAKE ALL DATE Y2K COMPLIENT.                 *
     H*                                                               *
     H* 09/10/93 C:20    REQ/PROJ: 127/MC101                          *
     H*                  1. CHANGED CODE TO NOT ALLOW SUBMISSION OF   *
     H*                     MORE THAN ONE DAILY BATCH AT A TIME       *
     H*                  A) WHEN SUBMITTED UPDATES DBFL11 WITH A 'Y'  *
     H*                  B) F60001R WILL UPDATE DBFL11 WITH A 'P'     *
     H*                     WHILE DAILY BATCH PROCESS IS RUNNING      *
     H*                  C) F60001R WILL UPDATE DBFL11 WITH A 'C'     *
     H*                     WHEN DAILY BATCH PROCESS HAS COMPLETED.   *
     H*                  2. IF TRY TO SUBMIT DAILY BATCH WHILE ONE IS *
     H*                     RUNNING(I.E. DBFL11 = Y) YOU WILL GO TO   *
     H*                     SCR04 ERROR SCREEN.                       *
     H*                  A) SAME IF DBFL11 = P                        *
     H*                  3. IF DAILY BATCH IS UNABLE TO ALLOCATE FDAIL*
     H*                     SZDBP RECORD WILL BE UPDATED WITH A 'C'   *
     H*                  4. IF DAILY BATCH CRASHES - DBFL11 WILL BE A *
     H*                     'P' BUT FDAILYA WILL BE DEALLOCATED SO    *
     H*                     USER WILL GOTO DAILY BATCH RE-SUBMIT      *
     H*                     SCREEN                                    *
     H*                                                               *
     H* 12/21/92 C:20    PROJ: 99AAE                                  *
     H*                  1. CHANGED CODE TO ALLOW WRITE OF SZDBP      *
     H*                     IF NO RECORDS EXIST                       *
     H*                                                               *
     H* 10/15/92 KRR     PROJ: 99AAD                                  *
     H*                  1. ADDED FILE SSYLF(FS BASE CONTROL FILE)    *
     H*                  2. ADDED UPDATE OF DBST01-10, DBTP01-10      *
     H*                     AND DBFL01-10 FROM FB FIELDS.             *
     H*                                                               *
     H*****************************************************************
     H/TITLE F6002R - DAILY BATCH PROCESS SUBMIT
     FF6002F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSZDBP     UF A E             DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************

     D DV              S             50    DIM(1) CTDATA PERRCD(1)              DATE VERIFY MSG
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
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
     C*
     C*  KEY LIST FOR USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C*** BEGIN ADD *** 08/15/00 **************************************
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
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
     C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C                   PARM      *BLANKS       ##TP01            2            TAPE STATE 01
     C                   PARM      *BLANKS       ##TP02            2            TAPE STATE 02
     C                   PARM      *BLANKS       ##TP03            2            TAPE STATE 03
     C                   PARM      *BLANKS       ##TP04            2            TAPE STATE 04
     C                   PARM      *BLANKS       ##TP05            2            TAPE STATE 05
     C                   PARM      *BLANKS       ##TP06            2            TAPE STATE 06
     C                   PARM      *BLANKS       ##TP07            2            TAPE STATE 07
     C                   PARM      *BLANKS       ##TP08            2            TAPE STATE 08
     C                   PARM      *BLANKS       ##TP09            2            TAPE STATE 09
     C                   PARM      *BLANKS       ##TP10            2            TAPE STATE 10
     C                   PARM      *BLANKS       ##ST01            2            STATE RULE 01
     C                   PARM      *BLANKS       ##ST02            2            STATE RULE 02
     C                   PARM      *BLANKS       ##ST03            2            STATE RULE 03
     C                   PARM      *BLANKS       ##ST04            2            STATE RULE 04
     C                   PARM      *BLANKS       ##ST05            2            STATE RULE 05
     C                   PARM      *BLANKS       ##ST06            2            STATE RULE 06
     C                   PARM      *BLANKS       ##ST07            2            STATE RULE 07
     C                   PARM      *BLANKS       ##ST08            2            STATE RULE 08
     C                   PARM      *BLANKS       ##ST09            2            STATE RULE 09
     C                   PARM      *BLANKS       ##ST10            2            STATE RULE 10
     C                   PARM      *BLANKS       ##DIFI            1            DIARY FUNCTION INST
     C                   PARM      *BLANKS       ##APRM            1            PROVIDER MAINT
     C                   PARM      *BLANKS       ##CLED            1            CLAIM EDIT INST
     C                   PARM      *BLANKS       ##DMEI            1            DME INSTALLED
     C                   PARM      *BLANKS       ##DLOS            1            DEFAULT LOS RGN
     C                   PARM      *BLANKS       ##PHPR            1            PHARMACY PROCESSING
     C*** END   ADD *** 08/15/00 **************************************
     C*
     C*  FS BASE CONTROL FILE
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      COCO#         SCCO#
     C*
     C*   GET THE DAILY BATCH RECORD
     C*
     C     1             CHAIN     PDB01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*** BEGIN ADD *** 02/19/02 **************************************
     C                   CALL      'F6002C2'
     C                   PARM      *BLANKS       ##FLAG            1
     C     ##FLAG        IFEQ      '1'
     C                   MOVEL(P)  'SCR05'       Q2RECF
     C                   ELSE
     C*** END   ADD *** 02/19/02 **************************************
     C     DBFL11        IFEQ      'Y'
     C                   EXCEPT    PDB01E
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   ELSE
     C     DBFL11        IFEQ      'P'
     C                   MOVE      *BLANKS       QQFLAG
     C                   CALL      'F6002C1'
     C                   PARM                    QQFLAG            1
     C     QQFLAG        IFEQ      '1'
     C                   EXCEPT    PDB01E
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   ELSE
     C                   MOVE      DBSUB#        SCSUB#
     C                   MOVE      DBSDT         SCDATE
     C                   MOVE      'N'           SCYEAR
     C                   MOVE      'N'           SCMNTH
     C                   MOVE      'N'           SCWKLY
     C                   MOVE      DBSDT         SCTWDT
     C                   MOVE      DBSDT         SCTMDT
     C                   MOVE      DBSDT         SCTYDT
     C*
     C     DBTAG         IFNE      *BLANK
     C                   MOVE      '1'           *IN90
     C                   MOVE      DBUSR         SCUSR
     C                   MOVE      DBJOB         SCJOB
     C                   MOVE      DBSDT         SCSDT
     C                   MOVE      DBSTM         SCSTM
     C                   MOVE      DBTAG         SCTAG
     C                   MOVE      DBCO#         SCRCO
     C                   MOVE      DBSUB#        SCSUB#
     C                   MOVE      DBMNTH        SCMNTH
     C                   MOVE      DBYEAR        SCYEAR
     C                   MOVE      DBWKLY        SCWKLY
     C                   MOVE      DBFWDT        SCFWDT
     C                   MOVE      DBTWDT        SCTWDT
     C                   MOVE      DBFMDT        SCFMDT
     C                   MOVE      DBTMDT        SCTMDT
     C                   MOVE      DBFYDT        SCFYDT
     C                   MOVE      DBTYDT        SCTYDT
     C                   END
     C                   END
     C                   ELSE
     C*
     C                   MOVE      DBSUB#        SCSUB#
     C                   MOVE      DBSDT         SCDATE
     C                   MOVE      'N'           SCYEAR
     C                   MOVE      'N'           SCMNTH
     C                   MOVE      'N'           SCWKLY
     C                   MOVE      DBSDT         SCTWDT
     C                   MOVE      DBSDT         SCTMDT
     C                   MOVE      DBSDT         SCTYDT
     C*
     C     DBTAG         IFNE      *BLANK
     C                   MOVE      '1'           *IN90
     C                   MOVE      DBUSR         SCUSR
     C                   MOVE      DBJOB         SCJOB
     C                   MOVE      DBSDT         SCSDT
     C                   MOVE      DBSTM         SCSTM
     C                   MOVE      DBTAG         SCTAG
     C                   MOVE      DBCO#         SCRCO
     C                   MOVE      DBSUB#        SCSUB#
     C                   MOVE      DBMNTH        SCMNTH
     C                   MOVE      DBYEAR        SCYEAR
     C                   MOVE      DBWKLY        SCWKLY
     C                   MOVE      DBFWDT        SCFWDT
     C                   MOVE      DBTWDT        SCTWDT
     C                   MOVE      DBFMDT        SCFMDT
     C                   MOVE      DBTMDT        SCTMDT
     C                   MOVE      DBFYDT        SCFYDT
     C                   MOVE      DBTYDT        SCTYDT
     C                   END
     C                   END
     C                   END
     C*** BEGIN ADD *** 02/19/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 02/19/02 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      DSDATE        SCDATE
     C                   MOVE      'N'           SCMNTH
     C                   MOVE      'N'           SCYEAR
     C                   MOVE      'N'           SCWKLY
     C*
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C     DBFL11        IFNE      'Y'
     C     QQFLAG        ANDNE     '1'
     C*** BEGIN ADD *** 02/19/02 **************************************
     C     ##FLAG        ANDNE     '1'
     C*** END   ADD *** 02/19/02 **************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C                   EXFMT     SCR04
     C                   GOTO      TERM
     C                   END
     C*** BEGIN ADD *** 02/19/02 **************************************
     C*
     C     Q2RECF        IFEQ      'SCR05'
     C                   EXFMT     SCR05
     C                   GOTO      TERM
     C                   ENDIF
     C*** END   ADD *** 02/19/02 **************************************
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C* * * PROCESS SUBSCRIBER SEARCH REQUEST
     C     *INKE         IFEQ      '1'
     C                   MOVEL     Q2RECF        XXRECF           10
     C                   CALL      'F0545R'
     C                   PARM                    QQPRDS
     C                   PARM      ' '           QQCALL            1
     C                   PARM                    SCSUB#
     C                   PARM      ' '           QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN09         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   CHECK FOR A VALID DATE
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEGIN ADD *** 01/25/99 **************************************
     C                   MOVEL     SRDTO         SCDATE
     C*** END   ADD *** 01/25/99 **************************************
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C**** CHECK FOR VALID SUBSCRIBER IF ENTERED.
     C*
     C     SCSUB#        IFNE      0
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         CABEQ     '1'           STRT                     33
     C                   END
     C*
     C*
     C*   WEEKLY FLAG MUST BE YES, NO
     C*
     C     SCWKLY        IFNE      'Y'
     C     SCWKLY        CABNE     'N'           STRT                 2525
     C                   END
     C*
     C*   CHECK FOR  VALID DATES FOR WEEKLY
     C*
     C     SCWKLY        IFEQ      'Y'
     C*
     C                   Z-ADD     SCFWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEGIN ADD *** 01/25/99 **************************************
     C                   MOVEL     SRDTO         SCFWDT
     C*** END   ADD *** 01/25/99 **************************************
     C*
     C     SRERR         CABEQ     '1'           STRT                     23
     C*
     C*
     C                   Z-ADD     SCTWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEGIN ADD *** 01/25/99 **************************************
     C                   MOVEL     SRDTO         SCTWDT
     C*** END   ADD *** 01/25/99 **************************************
     C*
     C     SRERR         CABEQ     '1'           STRT                     24
     C*
     C*** CHECK THAT THE FROM DATE IS NOT GREATER THAN TO DATE
     C*
     C*
     C                   Z-ADD     SCFWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SRFRDC            8 0
     C*
     C                   Z-ADD     SCTWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SRTODC            8 0
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC
     C                   PARM                    SRTODC
     C                   PARM                    SRDAYS            5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C     SRERR         CABEQ     '1'           STRT                     26
     C*
     C                   END
     C*
     C*   MONTHLY FLAG MUST BE YES, NO
     C*
     C     SCMNTH        IFNE      'Y'
     C     SCMNTH        ANDNE     'N'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   CHECK FOR  VALID DATES FOR MONTHLY
     C*
     C     SCMNTH        IFEQ      'Y'
     C*
     C                   Z-ADD     SCFMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEGIN ADD *** 01/25/99 **************************************
     C                   MOVEL     SRDTO         SCFMDT
     C*** END   ADD *** 01/25/99 **************************************
     C*
     C     SRERR         CABEQ     '1'           STRT                     27
     C*
     C*
     C                   Z-ADD     SCTMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEGIN ADD *** 01/25/99 **************************************
     C                   MOVEL     SRDTO         SCTMDT
     C*** END   ADD *** 01/25/99 **************************************
     C*
     C     SRERR         CABEQ     '1'           STRT                     28
     C*
     C*** CHECK THAT THE FROM DATE IS NOT GREATER THAN TO DATE
     C*
     C*
     C                   Z-ADD     SCFMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SRFRDC
     C*
     C                   Z-ADD     SCTMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SRTODC
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC
     C                   PARM                    SRTODC
     C                   PARM                    SRDAYS            5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C     SRERR         CABEQ     '1'           STRT                     29
     C*
     C                   END
     C*
     C*
     C*   YEARLY FLAG MUST BE YES, NO
     C*
     C     SCYEAR        IFNE      'Y'
     C     SCYEAR        ANDNE     'N'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*   CHECK FOR  VALID DATES FOR MONTHLY
     C*
     C     SCYEAR        IFEQ      'Y'
     C*
     C                   Z-ADD     SCFYDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEGIN ADD *** 01/25/99 **************************************
     C                   MOVEL     SRDTO         SCFYDT
     C*** END   ADD *** 01/25/99 **************************************
     C*
     C     SRERR         CABEQ     '1'           STRT                     30
     C*
     C*
     C                   Z-ADD     SCTYDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEGIN ADD *** 01/25/99 **************************************
     C                   MOVEL     SRDTO         SCTYDT
     C*** END   ADD *** 01/25/99 **************************************
     C*
     C     SRERR         CABEQ     '1'           STRT                     31
     C*
     C*** CHECK THAT THE FROM DATE IS NOT GREATER THAN TO DATE
     C*
     C*
     C                   Z-ADD     SCFYDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SRFRDC
     C*
     C                   Z-ADD     SCTYDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SRTODC
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC
     C                   PARM                    SRTODC
     C                   PARM                    SRDAYS            5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C     SRERR         CABEQ     '1'           STRT                     32
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        SCDTMS
     C                   MOVEL     DV(1)         SCDTMS
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C*   DATE VERIFICATION SCREEN
     C*
     C     SCR03T        TAG
     C*
     C*   CHECK FOR A VALID DATE
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEGIN ADD *** 01/25/99 **************************************
     C                   MOVEL     SRDTO         SCDATE
     C*** END   ADD *** 01/25/99 **************************************
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C**** CHECK FOR VALID SUBSCRIBER IF ENTERED.
     C*
     C     SCSUB#        IFNE      0
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         CABEQ     '1'           STRT                     33
     C                   END
     C*
     C*
     C*   WEEKLY FLAG MUST BE YES, NO
     C*
     C     SCWKLY        IFNE      'Y'
     C     SCWKLY        CABNE     'N'           STRT                 2525
     C                   END
     C*
     C*   CHECK FOR  VALID DATES FOR WEEKLY
     C*
     C     SCWKLY        IFEQ      'Y'
     C*
     C                   Z-ADD     SCFWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEGIN ADD *** 01/25/99 **************************************
     C                   MOVEL     SRDTO         SCFWDT
     C*** END   ADD *** 01/25/99 **************************************
     C*
     C     SRERR         CABEQ     '1'           STRT                     23
     C*
     C*
     C                   Z-ADD     SCTWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEGIN ADD *** 01/25/99 **************************************
     C                   MOVEL     SRDTO         SCTWDT
     C*** END   ADD *** 01/25/99 **************************************
     C*
     C     SRERR         CABEQ     '1'           STRT                     24
     C*
     C*** CHECK THAT THE FROM DATE IS NOT GREATER THAN TO DATE
     C*
     C*
     C                   Z-ADD     SCFWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SRFRDC
     C*
     C                   Z-ADD     SCTWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SRTODC
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC
     C                   PARM                    SRTODC
     C                   PARM                    SRDAYS            5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C     SRERR         CABEQ     '1'           STRT                     26
     C*
     C                   END
     C*
     C*   MONTHLY FLAG MUST BE YES, NO
     C*
     C     SCMNTH        IFNE      'Y'
     C     SCMNTH        ANDNE     'N'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   CHECK FOR  VALID DATES FOR MONTHLY
     C*
     C     SCMNTH        IFEQ      'Y'
     C*
     C                   Z-ADD     SCFMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEGIN ADD *** 01/25/99 **************************************
     C                   MOVEL     SRDTO         SCFMDT
     C*** END   ADD *** 01/25/99 **************************************
     C*
     C     SRERR         CABEQ     '1'           STRT                     27
     C*
     C*
     C                   Z-ADD     SCTMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEGIN ADD *** 01/25/99 **************************************
     C                   MOVEL     SRDTO         SCTMDT
     C*** END   ADD *** 01/25/99 **************************************
     C*
     C     SRERR         CABEQ     '1'           STRT                     28
     C*
     C*** CHECK THAT THE FROM DATE IS NOT GREATER THAN TO DATE
     C*
     C*
     C                   Z-ADD     SCFMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SRFRDC
     C*
     C                   Z-ADD     SCTMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SRTODC
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC
     C                   PARM                    SRTODC
     C                   PARM                    SRDAYS            5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C     SRERR         CABEQ     '1'           STRT                     29
     C*
     C                   END
     C*
     C*
     C*   YEARLY FLAG MUST BE YES, NO
     C*
     C     SCYEAR        IFNE      'Y'
     C     SCYEAR        ANDNE     'N'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*   CHECK FOR  VALID DATES FOR MONTHLY
     C*
     C     SCYEAR        IFEQ      'Y'
     C*
     C                   Z-ADD     SCFYDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEGIN ADD *** 01/25/99 **************************************
     C                   MOVEL     SRDTO         SCFYDT
     C*** END   ADD *** 01/25/99 **************************************
     C*
     C     SRERR         CABEQ     '1'           STRT                     30
     C*
     C*
     C                   Z-ADD     SCTYDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEGIN ADD *** 01/25/99 **************************************
     C                   MOVEL     SRDTO         SCTYDT
     C*** END   ADD *** 01/25/99 **************************************
     C*
     C     SRERR         CABEQ     '1'           STRT                     31
     C*
     C*** CHECK THAT THE FROM DATE IS NOT GREATER THAN TO DATE
     C*
     C*
     C                   Z-ADD     SCFYDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SRFRDC
     C*
     C                   Z-ADD     SCTYDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SRTODC
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC
     C                   PARM                    SRTODC
     C                   PARM                    SRDAYS            5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C     SRERR         CABEQ     '1'           STRT                     32
     C*
     C                   END
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, WRITE THE RECORD
     C*
     C*
     C* CHAIN OUT TO FS BASE CONTROL FILE TO UPDATE SZDBP STATE FIELDS
     C* AND TAPE SUBMISSION FIELDS AND WHAT PRODUCTS ARE INSTALLED
     C* FIELDS
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                40    CHG 12/21/92
     C********** *IN40     IFEQ '0'                        CHG 12/21/92
     C*** END   DEL *** 08/15/00 **************************************
     C*** BEGIN CHG *** 08/15/00 **************************************
     C*
     C* MOVE STATE RULES INSTALLED FIELDS
     C*
     C**********           MOVE FBST01    DBST01
     C**********           MOVE FBST02    DBST02
     C**********           MOVE FBST03    DBST03
     C**********           MOVE FBST04    DBST04
     C**********           MOVE FBST05    DBST05
     C**********           MOVE FBST06    DBST06
     C**********           MOVE FBST07    DBST07
     C**********           MOVE FBST08    DBST08
     C**********           MOVE FBST09    DBST09
     C**********           MOVE FBST10    DBST10
     C*
     C* MOVE STATE TAPE SUBMISSION INSTALLED FIELDS
     C*
     C**********           MOVE FBTP01    DBTP01
     C**********           MOVE FBTP02    DBTP02
     C**********           MOVE FBTP03    DBTP03
     C**********           MOVE FBTP04    DBTP04
     C**********           MOVE FBTP05    DBTP05
     C**********           MOVE FBTP06    DBTP06
     C**********           MOVE FBTP07    DBTP07
     C**********           MOVE FBTP08    DBTP08
     C**********           MOVE FBTP09    DBTP09
     C**********           MOVE FBTP10    DBTP10
     C*
     C* MOVE WHAT PRODUCTS ARE INSTALLED FIELDS
     C*
     C**********           MOVE FBFL01    DBFL01
     C**********           MOVE FBFL02    DBFL02
     C**********           MOVE FBFL03    DBFL03
     C**********           MOVE FBFL04    DBFL04
     C**********           MOVE FBFL05    DBFL05
     C**********           MOVE FBFL06    DBFL06
     C**********           MOVE FBFL07    DBFL07
     C**********           MOVE FBFL08    DBFL08
     C**********           MOVE FBFL09    DBFL09
     C**********           MOVE FBFL10    DBFL10
     C*
     C* MOVE STATE RULES INSTALLED FIELDS
     C*
     C                   MOVE      ##ST01        DBST01
     C                   MOVE      ##ST02        DBST02
     C                   MOVE      ##ST03        DBST03
     C                   MOVE      ##ST04        DBST04
     C                   MOVE      ##ST05        DBST05
     C                   MOVE      ##ST06        DBST06
     C                   MOVE      ##ST07        DBST07
     C                   MOVE      ##ST08        DBST08
     C                   MOVE      ##ST09        DBST09
     C                   MOVE      ##ST10        DBST10
     C*
     C* MOVE STATE TAPE SUBMISSION INSTALLED FIELDS
     C*
     C                   MOVE      ##TP01        DBTP01
     C                   MOVE      ##TP02        DBTP02
     C                   MOVE      ##TP03        DBTP03
     C                   MOVE      ##TP04        DBTP04
     C                   MOVE      ##TP05        DBTP05
     C                   MOVE      ##TP06        DBTP06
     C                   MOVE      ##TP07        DBTP07
     C                   MOVE      ##TP08        DBTP08
     C                   MOVE      ##TP09        DBTP09
     C                   MOVE      ##TP10        DBTP10
     C*
     C* MOVE WHAT PRODUCTS ARE INSTALLED FIELDS
     C*
     C                   MOVE      ##WCIN        DBFL01
     C                   MOVE      ##DIFI        DBFL02
     C                   MOVE      ##APRM        DBFL03
     C                   MOVE      ##CLED        DBFL04
     C                   MOVE      ##DMEI        DBFL05
     C                   MOVE      ##NTWK        DBFL06
     C                   MOVE      ##UR          DBFL07
     C                   MOVE      ##DLOS        DBFL08
     C                   MOVE      ##EOBI        DBFL09
     C                   MOVE      ##PHPR        DBFL10
     C*** END   CHG *** 08/15/00 **************************************
     C*
     C                   MOVE      'Y'           DBFL11
     C                   MOVE      *BLANKS       DBFL12
     C                   MOVE      *BLANKS       DBFL13
     C                   MOVE      *BLANKS       DBFL14
     C                   MOVE      *BLANKS       DBFL15
     C                   MOVE      *BLANKS       DBFL16
     C                   MOVE      *BLANKS       DBFL17
     C                   MOVE      *BLANKS       DBFL18
     C                   MOVE      *BLANKS       DBFL19
     C                   MOVE      *BLANKS       DBFL20
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           END                             END *IN60
     C*** END   DEL *** 08/15/00 **************************************
     C                   MOVE      SCDATE        DBSDT
     C                   MOVE      DSTIME        DBSTM
     C                   MOVE      SCWKLY        DBWKLY
     C                   MOVE      SCMNTH        DBMNTH
     C                   MOVE      SCYEAR        DBYEAR
     C                   MOVE      Q1USER        DBUSR
     C                   MOVE      Q1JOB         DBJOB
     C                   MOVE      COCO#         DBCO#
     C                   MOVE      SCSUB#        DBSUB#
     C                   MOVE      SCFWDT        DBFWDT
     C                   MOVE      SCTWDT        DBTWDT
     C                   MOVE      SCFMDT        DBFMDT
     C                   MOVE      SCTMDT        DBTMDT
     C                   MOVE      SCFYDT        DBFYDT
     C                   MOVE      SCTYDT        DBTYDT
     C     *IN90         IFEQ      '1'
     C                   MOVE      'Y'           DBRTRT
     C                   MOVE      SCRCO         DBCO#
     C                   MOVE      SCSUB#        DBSUB#
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PDB01
     C                   ELSE
     C                   WRITE     PDB01
     C                   END
     C*
     C*  SUBMIT THE JOB
     C*
     C                   CALL      'F6002C'
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPDB01     E            PDB01E
**  DV DATE VERIFICATION MESSAGE
VERIFY DATES ENTERED.  If correct, press <ENTER>.
