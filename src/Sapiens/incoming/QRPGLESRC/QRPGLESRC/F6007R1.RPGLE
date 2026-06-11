     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F6007R1 - PROCESS AUTO CHECK REQUESTS
     H*****************************************************************
     H* PROGRAM DESCRIPTIION:                                         *
     H*   Process Requests for Daily Batch                            *
     H*****************************************************************
     H* CALLED FROM:                                                  *
     H*   W6007C1                                                     *
     H* CALLS  TO:                                                    *
     H*   none                                                        *
     H*                                                               *
     H* CREATED 05/28/04  C:53  PROJECT: WC1198                       *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FSZDBP     UF A E             DISK
     FSMCOL     IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@BDAT            8
     C                   PARM                    @@WKLY            1
     C                   PARM                    @@FWDT            8
     C                   PARM                    @@MNTH            1
     C                   PARM                    @@FMDT            8
     C                   PARM                    @@YEAR            1
     C                   PARM                    @@FYDT            8
     C                   PARM                    @@RTCD            1
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
     C     PLPAST        PLIST
     C                   PARM                    XXBDAT
     C                   PARM      1             ##DAYS            5 0
     C                   PARM                    ##ERR             1
     C*
     C*  KEY LIST FOR USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    COCO#         XXCO#
     C     *LIKE         DEFINE    DBSDT         XXBDAT
     C     *LIKE         DEFINE    DBSDT         XXFWDT
     C     *LIKE         DEFINE    DBSDT         XXFMDT
     C     *LIKE         DEFINE    DBSDT         XXFYDT
     C*
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@BDAT        XXBDAT
     C                   MOVE      @@FWDT        XXFWDT
     C                   MOVE      @@FMDT        XXFMDT
     C                   MOVE      @@FYDT        XXFYDT
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
     C*
     C                   TIME                    DSDTTM
     C*
     C*  Get/validate the company #
     C*
     C     XXCO#         CHAIN     PCO01                              99
     C     *IN99         IFEQ      '1'
     C                   MOVE      '0'           @@RTCD
     C                   GOTO      EOJ
     C                   END
     C*
     C*  If batch date is 0 get system date and subtract 1 day.
     C*
     C     XXBDAT        IFLE      *ZEROS
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXBDAT
     C*
     C                   CALL      'SRPAST8'     PLPAST
     C                   ENDIF
     C*
     C*  Validate the Batch date
     C*
     C                   Z-ADD     XXBDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           @@RTCD
     C                   GOTO      EOJ
     C                   END
     C*
     C*  Check for valid dates for weekly
     C*
     C     @@WKLY        IFEQ      'Y'
     C                   Z-ADD     XXFWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXFWDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '3'           @@RTCD
     C                   GOTO      EOJ
     C                   END
     C*
     C*** CHECK THAT THE WEEKLY FROM DATE IS NOT GREATER THAN TO DATE
     C*
     C                   Z-ADD     XXFWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFWD7            7 0
     C*
     C                   Z-ADD     XXBDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBDA7            7 0
     C*
     C     XXFWD7        IFGT      XXBDA7
     C                   MOVE      '4'           @@RTCD
     C                   GOTO      EOJ
     C                   END
     C*
     C                   END
     C*
     C*   CHECK FOR VALID DATES FOR MONTHLY
     C*
     C     @@MNTH        IFEQ      'Y'
     C                   Z-ADD     XXFMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXFMDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '5'           @@RTCD
     C                   GOTO      EOJ
     C                   END
     C*
     C*** CHECK THAT THE FROM DATE IS NOT GREATER THAN TO DATE
     C*
     C                   Z-ADD     XXFMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFMD7            7 0
     C*
     C                   Z-ADD     XXBDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBDA7            7 0
     C*
     C     XXFMD7        IFGT      XXBDA7
     C                   MOVE      '6'           @@RTCD
     C                   GOTO      EOJ
     C                   END
     C                   END
     C*
     C*   CHECK FOR  VALID DATES FOR YEARLY
     C*
     C     @@YEAR        IFEQ      'Y'
     C                   Z-ADD     XXFYDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXFYDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '7'           @@RTCD
     C                   GOTO      EOJ
     C                   END
     C*
     C*** CHECK THAT THE FROM DATE IS NOT GREATER THAN TO DATE
     C*
     C                   Z-ADD     XXFYDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFYD7            7 0
     C*
     C                   Z-ADD     XXBDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBDA7            7 0
     C*
     C     XXFYD7        IFGT      XXBDA7
     C                   MOVE      '8'           @@RTCD
     C                   GOTO      EOJ
     C                   END
     C*
     C                   END
     C*
     C*  Get the DBP record
     C*
     C     1             CHAIN     PDB01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*** DBP submitted
     C     DBFL11        IFEQ      'Y'
     C                   MOVE      '2'           @@RTCD
     C                   GOTO      EOJ
     C                   ELSE
     C*
     C*** DBP active
     C     DBFL11        IFEQ      'P'
     C                   MOVE      *BLANKS       @@FLAG
     C                   CALL      'F6002C1'
     C                   PARM                    @@FLAG            1
     C     @@FLAG        IFEQ      '1'
     C                   MOVE      '2'           @@RTCD
     C                   GOTO      EOJ
     C                   END
     C                   END
     C                   END
     C                   END
     C*****************************************************
     C*
     C* WE HAVE PASSED ALL EDITS, WRITE THE RECORD
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
     C                   MOVE      XXBDAT        DBSDT
     C                   MOVE      DSTIME        DBSTM
     C                   MOVE      @@WKLY        DBWKLY
     C                   MOVE      @@MNTH        DBMNTH
     C                   MOVE      @@YEAR        DBYEAR
     C                   MOVE      Q1USER        DBUSR
     C                   MOVE      Q1JOB         DBJOB
     C                   MOVE      COCO#         DBCO#
     C                   MOVE      XXFWDT        DBFWDT
     C                   MOVE      XXBDAT        DBTWDT
     C                   MOVE      XXFMDT        DBFMDT
     C                   MOVE      XXBDAT        DBTMDT
     C                   MOVE      XXFYDT        DBFYDT
     C                   MOVE      XXBDAT        DBTYDT
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PDB01
     C                   ELSE
     C                   WRITE     PDB01
     C                   END
     C*
     C*  CLP WILL SUBMIT THE JOB IF NO ERRORS WERE DETECTED.
     C*
     C                   MOVE      'Y'           @@RTCD
     C*
     C*  END THE PROGRAM
     C*
     C     EOJ           TAG
     C                   MOVE      '1'           *INLR
