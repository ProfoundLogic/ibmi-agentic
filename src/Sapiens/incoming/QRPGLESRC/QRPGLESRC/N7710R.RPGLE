     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H/TITLE N7710R - PBR ACTIVITY REPORT
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*   01/28/10 C:249 Project: SY215                               *
     H*            C:189 Retrieve brand Name                          *
     H*                                                               *
     H*  10/11/99 C:62  Project: MC212
     H*                 ADD PROVIDER BILL PROCESSING.
     H*
     H*  01/04/99 C:31  Project: MC152
     H*                 1. Change size of invoice number
     H*
     H*  01/28/94 C:20  REQ/PROJ: ARMNT                               *
     H*                 1. FIX TO MAKE REPORT TITLE COME FROM OUTPUT  *
     H*                    ROUTING FILE INSTEAD OF HARDCODED          *
     H*                                                               *
     H*  11/07/95 C:41  REQ/PROJ: SUPT                                *
     H*                 1. FIX ARRAY INDEX ERROR ON INVA(I).          *
     H*                                                               *
     H*  06/28/96 C:45  REQ/PROJ: MCMNT                               *
     H*                 1. MODIFY INVOICE TOTALING TO ELIMINATE       *
     H*                    ARRAY INDEX ERROR                          *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFAEOL8    IF   E           K DISK
     F*** BEGIN ADD *** 10/11/99 **************************************
     FFAEAL1    IF   E           K DISK
     F*** END   ADD *** 10/11/99 **************************************
     FNMARL2    IF   E           K DISK
     FNMARL8    IF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FFMSML     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)

     D*** BEGIN CHG *** 01/04/99 **************************************
     D********            INVA     9999  5 0
     D INVA            S              9  0 DIM(9999)
     D*** END CHG *** 01/04/99 ****************************************
     D INVB            S              5  0 DIM(9999)                            ADD 11/07/95
     D INVC            S              5  0 DIM(9999)                            ADD 11/07/95
     D                 DS
     D  XSZIP                  1      9  0
     D  S1ZPP1                 1      5  0
     D  S1ZPP2                 6      9  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND
     D  CONAME                 1     40
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB             5
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** BEGIN ADD *** 10/11/99 **************************************
     C*
     C* FIND OUT IF PROVIDER BILLS IS INSTALLED
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
     C*** END   ADD *** 10/11/99 **************************************
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB         XXSUB             5 0
     C                   MOVE      QQFRDT        X1FRDT            8 0
     C*
     C                   Z-ADD     X1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   MOVE      QQTODT        X1TODT            8 0
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C                   Z-ADD     0             I                 4 0          CHG 11/07/95
     C                   Z-ADD     0             J                 4 0
     C                   Z-ADD     0             K                 4 0
     C                   Z-ADD     0             INVTL             7 0
     C*
     C* INITIALIZATION SECTION
     C*
     C     *LIKE         DEFINE    EOEOB#        HLEOB#
     C     *LIKE         DEFINE    UPDT          XXFDT
     C     *LIKE         DEFINE    UPDT          XXTDT
     C*
     C*  COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB
     C*
     C*  ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB
     C                   KFLD                    ARFND
     C                   KFLD                    ARFYR
     C                   KFLD                    ARCASE
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB
     C                   KFLD                    HLDFND
     C*
     C*  EOB WORK FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB
     C                   KFLD                    ARFND
     C                   KFLD                    ARINV#                         DEL 03/31/93
     C                   KFLD                    AREOB#
     C*** BEGIN ADD *** 10/11/99 **************************************
     C*
     C     EAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*** END   ADD *** 10/11/99 **************************************
     C*
     C*  A/R FILE
     C*
     C     ARKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB
     C     ARKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB
     C*** BEGIN CHG *** 01/04/99 **************************************
     C********             KFLD           XXINV#  50
     C                   KFLD                    XXINV#            9 0
     C*** END CHG *** 01/04/99 ****************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              30
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR REPORT
     C*
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C                   MOVE      '0'           XXPRT                           QSYSPRT
     C     ORKEY         CHAIN     POR01                              35
     C     *IN35         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION FOR REPORT
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C********           EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END                                                    END IFEQ '0'
     C*
     C*  GET SUBSCRIBER RECORD (CARRIER INFO)
     C*
     C     SMKEY         CHAIN     PSM01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C* * * GET COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              40
     C     *IN40         IFEQ      '0'
     C*                    -----***REMOVE LEADING BLANKS FROM COMPANY NAME
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    CONM(Z)                            45
     C                   MOVEA     CONM(Z)       XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   END
     C*
     C*   PRINT THE HEADING INFORMATION ON THE REPORT
     C*
     C                   EXCEPT    HEAD1
     C*
     C*  GET A/R RECORDS
     C*
     C     ARKEY         CHAIN     PAR01                              50
     C     *IN50         IFEQ      '1'
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END
     C*
     C                   ADD       1             I
     C                   MOVEA     ARINV#        INVA(I)
     C*
     C                   Z-ADD     AREOB#        HLEOB#
     C                   Z-ADD     ARCASE        HLDCSE            7 0
     C                   Z-ADD     ARFND         HLDFND            3 0
     C*
     C     EOKEY         CHAIN     PEO01                              51
     C     *IN51         IFEQ      '0'
     C*** BEGIN ADD *** 10/11/99 **************************************
     C     ##PRVB        IFNE      'Y'
     C*** END   ADD *** 10/11/99 **************************************
     C                   ADD       ARAMT         XCAMTI            9 2
     C                   ADD       EOAMTC        XCAMTC            9 2
     C     EOAMT         SUB       EONDIS        XXAMTA            9 2          ADD 08/05/93
     C                   ADD       XXAMTA        XCAMTA            9 2          ADD 08/05/95
     C     EO#BLL        IFEQ      *ZERO
     C                   ADD       1             XCCNT             6 0          ADD 08/20/93
     C                   ELSE
     C                   ADD       EO#BLL        XCCNT             6 0          ADD 08/20/93
     C                   END
     C*** BEGIN ADD *** 10/11/99 **************************************
     C                   ELSE
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   ADD       ARAMT         XCAMTI            9 2
     C                   ADD       EOCHRG        XCAMTC            9 2
     C     EAPAID        SUB       EANDIS        XXAMTA            9 2          ADD 08/05/93
     C                   ADD       XXAMTA        XCAMTA            9 2          ADD 08/05/95
     C     EO#BLL        IFEQ      *ZERO
     C                   ADD       1             XCCNT             6 0          ADD 08/20/93
     C                   ELSE
     C                   ADD       EO#BLL        XCCNT             6 0          ADD 08/20/93
     C                   END
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 10/11/99 **************************************
     C                   END
     C*
     C     CMKEY         CHAIN     PCM01                              52
     C     *IN52         IFEQ      '0'
     C                   MOVEL     CMSS#         XXSS#             9 0
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C                   MOVEL     CMCCL#        XXCLM#           13
     C                   END
     C*
     C     CDKEY         CHAIN     PCD01                              53
     C     *IN53         IFEQ      '0'
     C                   MOVEL     *BLANKS       SRLNAM           56
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM           15
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM            1
     C*
     C                   MOVEL     SRLNAM        XXNAME           25
     C                   END
     C*
     C     FNKEY         CHAIN     PFN01                              54
     C     *IN54         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNME           40
     C                   ELSE
     C                   MOVEL     *BLANKS       XXFNME
     C                   END
     C*
     C                   EXCEPT    HEAD2
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*  TEST FOR CASE LEVEL BREAK
     C*
     C     ARCASE        IFNE      HLDCSE
     C     ARFND         ANDEQ     HLDFND
     C                   EXSR      SRTCSE
     C                   END
     C*
     C*  TEST FOR GROUP LEVEL BREAK
     C*
     C     ARFND         IFNE      HLDFND
     C                   EXSR      SRTCSE
     C                   EXSR      SRTFND
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C                   END
     C*
     C*  ACCUMULATE CASE TOTALS
     C*
     C*          ARAMT     IFGE *ZERO
     C*                    ADD  1         XCCNT   30
     C*                    ELSE
     C*                    SUB  1         XCCNT
     C*                    END
     C*
     C*  TEST FOR CHANGE IN EOB#
     C*  IF CHANGED, READ EOB FILE FOR AMT CHARGED AND REC. ALLOWANCE
     C*
     C     AREOB#        IFNE      HLEOB#
     C     EOKEY         CHAIN     PEO01                              51
     C     *IN51         IFEQ      '0'
     C*** BEGIN ADD *** 10/11/99 **************************************
     C     ##PRVB        IFNE      'Y'
     C*** END   ADD *** 10/11/99 **************************************
     C                   ADD       ARAMT         XCAMTI            9 2
     C                   ADD       EOAMTC        XCAMTC            9 2
     C     EOAMT         SUB       EONDIS        XXAMTA            9 2          ADD 08/05/93
     C                   ADD       XXAMTA        XCAMTA            9 2          ADD 08/05/95
     C     EO#BLL        IFEQ      *ZERO
     C                   ADD       1             XCCNT             6 0          ADD 08/20/93
     C                   ELSE
     C                   ADD       EO#BLL        XCCNT             6 0          ADD 08/20/93
     C                   END
     C*** BEGIN ADD *** 10/11/99 **************************************
     C                   ELSE
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   ADD       ARAMT         XCAMTI            9 2
     C                   ADD       EOCHRG        XCAMTC            9 2
     C     EAPAID        SUB       EANDIS        XXAMTA            9 2          ADD 08/05/93
     C                   ADD       XXAMTA        XCAMTA            9 2          ADD 08/05/95
     C     EO#BLL        IFEQ      *ZERO
     C                   ADD       1             XCCNT             6 0          ADD 08/20/93
     C                   ELSE
     C                   ADD       EO#BLL        XCCNT             6 0          ADD 08/20/93
     C                   END
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 10/11/99 **************************************
     C                   END
     C                   Z-ADD     AREOB#        HLEOB#
     C                   END
     C*
     C*  READ THE NEXT A/R RECORD FOR THIS SUBSCRIBER
     C*
     C     ARKEY         READE     PAR01                                  50
     C                   END                                                    ENDDOW *IN50
     C*
     C*** END PROGRAM
     C*
     C                   EXSR      SRTCSE
     C                   EXSR      SRTFND
     C                   EXSR      SRTOTL
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*****************************************************************
     C*  TOTAL LINE FOR CASE (DETAIL SUMMARY)            S R T C S E  *
     C*****************************************************************
     C     SRTCSE        BEGSR
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C     XCAMTC        SUB       XCAMTA        XCAMTS            9 2
     C                   Z-ADD     HLDCSE        XXCASE            7 0
     C*
     C     XCCNT         IFNE      0                                            ## LJD 9/21
     C                   EXCEPT    TOTCSE
     C                   END                                                    ##
     C*
     C                   ADD       XCCNT         XGCNT             6 0          ADD 08/20/93
     C                   ADD       XCAMTC        XGAMTC            9 2
     C                   ADD       XCAMTS        XGAMTS            9 2
     C                   ADD       XCAMTA        XGAMTA            9 2
     C                   ADD       XCAMTI        XGAMTI            9 2
     C*
     C                   Z-ADD     *ZERO         XXSS#
     C                   Z-ADD     *ZERO         XXACDT
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVE      *BLANKS       XXCLM#
     C                   Z-ADD     *ZERO         XCCNT
     C                   Z-ADD     *ZERO         XCAMTC
     C                   Z-ADD     *ZERO         XCAMTS
     C                   Z-ADD     *ZERO         XCAMTA
     C                   Z-ADD     *ZERO         XCAMTI
     C*
     C                   Z-ADD     ARCASE        HLDCSE
     C*
     C     CMKEY         CHAIN     PCM01                              52
     C     *IN52         IFEQ      '0'
     C                   MOVEL     CMSS#         XXSS#             9 0
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C                   MOVEL     CMCCL#        XXCLM#           13
     C                   END
     C*
     C     CDKEY         CHAIN     PCD01                              53
     C     *IN53         IFEQ      '0'
     C                   MOVEL     *BLANKS       SRLNAM           56
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM           15
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM            1
     C*
     C                   MOVEL     SRLNAM        XXNAME           25
     C                   END
     C*
     C                   ENDSR
     C*****************************************************************
     C*  TOTAL LINE FOR FUND                             S R T F N D  *
     C*****************************************************************
     C     SRTFND        BEGSR
     C                   EXCEPT    TOTFND
     C*
     C                   ADD       XGCNT         XTCNT             6 0          ADD 08/20/93
     C                   ADD       XGAMTC        XTAMTC            9 2
     C                   ADD       XGAMTS        XTAMTS            9 2
     C                   ADD       XGAMTA        XTAMTA            9 2
     C                   ADD       XGAMTI        XTAMTI            9 2
     C*
     C                   Z-ADD     *ZERO         XGCNT
     C                   Z-ADD     *ZERO         XGAMTC
     C                   Z-ADD     *ZERO         XGAMTS
     C                   Z-ADD     *ZERO         XGAMTA
     C                   Z-ADD     *ZERO         XGAMTI
     C*
     C                   Z-ADD     ARFND         HLDFND
     C*
     C     FNKEY         CHAIN     PFN01                              54
     C     *IN54         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNME
     C                   ELSE
     C                   MOVEL     *BLANKS       XXFNME
     C                   END
     C*
     C                   ENDSR
     C*****************************************************************
     C*  FINAL TOTAL - REPORT SUMMARY                    S R T O T L  *
     C*****************************************************************
     C     SRTOTL        BEGSR
     C*
     C*  CALCULATE/PRINT SUMMARY PAGE TOTALS
     C*
     C     XTAMTC        IFNE      *ZERO
     C     XTAMTS        DIV(H)    XTAMTC        XXPERS            5 3
     C                   ELSE
     C                   Z-ADD     *ZERO         XXPERS            5 3
     C                   END
     C     XXPERS        MULT      100           XXPERS
     C                   Z-ADD     XXPERS        XXPPRT            3 1
     C     XTAMTI        IFNE      *ZERO
     C     XTAMTS        DIV(H)    XTAMTI        XXROIA            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         XXROIA            4 2
     C                   END
     C     XTAMTS        SUB       XTAMTI        XXNETS            9 2
     C     XTCNT         IFNE      *ZERO
     C     XTAMTI        DIV(H)    XTCNT         XXAVBR            9 2
     C     XXNETS        DIV(H)    XTCNT         XXAVSV            9 2
     C                   ELSE
     C                   Z-ADD     *ZERO         XXAVBR            9 2
     C                   Z-ADD     *ZERO         XXAVSV            9 2
     C                   END
     C*
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C*
     C                   Z-ADD     1             XXINV#
     C*
     C     ARKY2         SETLL     PAR02
     C                   READ      PAR02                                  70
     C*
     C     *IN70         DOWEQ     '0'
     C*
     C*** CONVERT FROM AND TO DATES AND COMPARE
     C*
     C*
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDT
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT
     C*
     C     ARTRDT        CABLT     XXFDT         READNW
     C     ARTRDT        CABGT     XXTDT         READNW
     C*
     C     XXONCE        IFEQ      ' '
     C                   MOVE      'X'           XXONCE            1
     C*** BEGIN CHG *** 01/04/99 **************************************
     C********             Z-ADDARINV#    XAINV#  50
     C                   Z-ADD     ARINV#        XAINV#            9 0
     C*** END CHG *** 01/04/99 ****************************************
     C                   ADD       1             INVTL
     C                   ENDIF
     C*
     C     ARINV#        IFNE      XAINV#
     C     ARSUB#        ANDEQ     XXSUB
     C                   Z-ADD     ARINV#        XAINV#
     C                   ADD       1             INVTL
     C                   ENDIF
     C*
     C     READNW        TAG
     C*
     C                   READ      PAR02                                  70
     C*
     C                   ENDDO
     C                   EXCEPT    LRTOTL
     C                   ENDSR
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             Q1USER              27
     O**********                                 29 '-'
     O**********             UPUSNM              80
     O**********E            ROUTE          1
     O**********E            ROUTE          1
     O**********E            ROUTE          2
     O**********E            ROUTE          2
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       Q1USER              27
     O                                           29 '-'
     O                       UPUSNM              80
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              96
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE          2
     O                                           21 'REPORT SELECTION:'
     O          E            ROUTE          2
     O                                           31 'SUBSCRIBER......:'
     O                       XXSUB               38
     O          E            ROUTE          2
     O                                           31 'DATE RANGE:.....:'
     O                       XXFRDT        Y     41
     O                                           44 'TO'
     O                       XXTODT        Y     53
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               62 'INC. 1997          '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT   E            HEAD1          1 06
     O                       DSDATE        Y      8
     O                       ORTITL              80
     O                       DSTIME             126 '  :  :  '
     O          E            HEAD1          2
     O                                           11 'N7710R     '
     O************************ ORPNAM    11                        LGC
     O                       XXCONM              94
     O                       PAGE               126
     O                                          121 'PAGE:'
     O          E            HEAD1          2
     O                       XXFRDT        Y     61
     O                                           63 '-'
     O                       XXTODT        Y     72
     O          E            ERRMSG         2
     O                                           17 '*** NO BILLS MEET'
     O                                           36 'SELECTION CRITERIA'
     O          E            HEAD2          1
     O                                           11 'SUBSCRIBER'
     O                       XXSUB               17
     O                                           19 '-'
     O                       XXSBNM              60
     O          E            HEAD2          2
     O                                            6 'GROUP'
     O                       HLDFND              17
     O                                           19 '-'
     O                       XXFNME              60
     O          E            HEAD2          1
     O                                           26 'ACCIDENT   #OF'
     O                                           34 'CASE'
     O                                           86 'AMOUNT'
     O                                           99 'AMOUNT'
     O                                          116 'RECOMMENDED'
     O                                          125 'REVIEWER'
     O          E            HEAD2          2
     O                                           19 ' SOC SEC #   DATE'
     O                                           27 'BILLS'
     O                                           32 '#'
     O                                           47 'CLIENT NAME'
     O                                           72 'CLAIM #'
     O                                           87 'CHARGED'
     O                                           98 'SAVED'
     O                                          115 'ALLOWANCE'
     O                                          125 'FEE'
     O          E            HEAD3          1
     O                                           11 'SUBSCRIBER'
     O                       XXSUB               17
     O                                           19 '-'
     O                       XXSBNM              60
     O          E            HEAD3       1  2
     O                                           21 'ALL GROUP TOTALS'
     O          E            TOTCSE         1
     O                       XXSS#               11 '0  -  -    '
     O                       XXACDT        Y     20
     O                       XCCNT         Z     27
     O                       XXCASE        Z     34
     O                       XXNAME              61
     O                       XXCLM#              75
     O                       XCAMTC        J     88
     O                       XCAMTS        J    101
     O                       XCAMTA        J    114
     O                       XCAMTI        J    126
     O          E            TOTFND      2
     O                                            6 'GROUP'
     O                       HLDFND              10
     O                                           18 'TOTALS:'
     O                       XGCNT         Z     27
     O                       XGAMTC        J     88
     O                       XGAMTS        J    101
     O                       XGAMTA        J    114
     O                       XGAMTI        J    126
     O          E            LRTOTL         2
     O                                           64 'TOTAL NUMBER OF BILLS:'
     O                       XTCNT         1     78
     O          E            LRTOTL         2
     O                                           55 'TOTAL NUMBER OF '
     O                                           64 'INVOICES:'
     O                       INVTL         1     78
     O          E            LRTOTL         2
     O                                           64 'TOTAL AMOUNT CHARGED:'
     O                       XTAMTC        J     79 '$'
     O          E            LRTOTL         2
     O                                           64 'TOTAL AMOUNT SAVED:'
     O                       XTAMTS        J     79 '$'
     O          E            LRTOTL         2
     O                                           64 'TOTAL REC. ALLOWANCE:'
     O                       XTAMTA        J     79 '$'
     O          E            LRTOTL         2
     O                                           64 'TOTAL REVIEWER FEE:'
     O                       XTAMTI        J     79 '$'
     O          E            LRTOTL         2
     O                                           64 'NET SAVINGS:'
     O                       XXNETS        J     79 '$'
     O          E            LRTOTL         2
     O                                           64 'PERCENT SAVINGS:'
     O                       XXPPRT        J     77
     O                                           78 '%'
     O          E            LRTOTL         2
     O                                           64 'AVERAGE BILLING RATE:'
     O                       XXAVBR        J     79 '$'
     O          E            LRTOTL         2
     O                                           64 'AVERAGE SAVINGS:'
     O                       XXAVSV        J     79 '$'
     O          E            LRTOTL         2
     O                                           64 'RETURN ON INVESTMENT:'
     O                       XXROIA        J     79
     O                                           73 '1:'
