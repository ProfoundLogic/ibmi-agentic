     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H**                                                             **
     H/TITLE F9605RTX - TEXAS STATE TAPE EDIT PROGRAM                **
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program produce an edit report to identify any     **
     H**     possible errors with Texas tape submission data.        **
     H**   CALLED FROM:                                              **
     H**     F9605CTX - TX State tape edit                           **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: C:41 - Pat Jeanniton                          **
     H**   CREATED:   10/03/95      PROJECT: TX003                   **
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  08/17/99  C:79   Support#:19005
     H*                   1. Add error message when there are no detail
     H*                      amount records (FDDAP).
     H*                   2. Use FAEAP for main read.
     H*
     H*  05/05/99  C:56   MC152
     H*                   Change FAEAL to FAEAL1, duplicate logicals
     H*
     H*  01/06/99 C:79  Project: TX009                               **
     H*                 1. Modify to work with YR2000 dates          **
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      OF      OVERFLOW FOR PRINTING                          **
     H**      08      Batch Mode                                     **
     H**      09      Demand Mode                                    **
     H**      50      General Usage (CHAIN)                          **
     H**      60      READ/DOWEQ of Open Query File                  **
     H*****************************************************************
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO                                        **
     H**    FAEOL4(PUP01): Q1USER                                    **
     H**    FAE1L (POR01): CO/FND                                    **
     H**    FMCML1(PFN01): CO/FND                                    **
     H**    FDCALH(PCA01): CO/FND/FYR/CASE                           **
     H**    FMP1L (PCA01): CO/FND/FYR/CASE                           **
     H**    SMUPL (PFN01): CO/FND                                    **
     H**    SMORL (PFN01): CO/FND                                    **
     H**    FMSML (PFN01): CO/FND                                    **
     H**    SMCOL (PFN01): CO/FND                                    **
     H**    FMFNL (PFN01): CO/FND                                    **
     H*****************************************************************
     FFAEOL4    IF   E           K DISK
     F*** BEG CHG *** 05/05/99 ****************************************
     F**********FAEAL   IF  E           K        DISK
     FFAEAL17   IF   E           K DISK
     F*** END CHG *** 05/05/99 ****************************************
     FFMCML1    IF   E           K DISK
     FFDCALH    IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFAEUL     IF   E           K DISK
     FFAEHL     IF   E           K DISK
     FFDDAL     IF   E           K DISK
     FQSYSPRT   O    F  129        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)

     D*
     D ERR             S             67    DIM(13) CTDATA PERRCD(1)             ERROR MSG.
     D EMSG            S             67    DIM(9999)                            ERROR LINE
     D FND             S              3  0 DIM(30)                              FUND
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D* Phys FL Lic#
     D EOFLIC          DS             9
     D  PREFX                  1      2
     D  MDA                    3      3
     D  MDN                    4      7
     D  MDB                    8      9
     D  DDDN                   3      6
     D  DDDB                   7      9
     D  POCN                   3      8
     D  POCB                   9      9
     D  OPAB                   3      9
     D  DPN                    3      5
     D  DPNB                   6      9
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  FNNM                   1     60
     D                                     DIM(60) ASCEND                       FUND NAME
     D  FNNAME                 1     60
     D*
     D                 DS
     D  CASE                   1     10
     D  XXYR                   1      2  0
     D  XXHYP1                 3      3
     D  XXCASE                 4     10  0
     D*
     D* NAMED CONSTANT FIELDS
     D*
     D ALPHA           C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D*
     D ALL             C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ0123456789 ')
     D*
     D NUM19B          C                   CONST('0123456789 ')

     I*

     C*
     C* Key List for the EOB Master file
     C*
     C     EAKY          KLIST                                                  from FAEOP
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C     EAKY1         KLIST                                                  from FAEOP
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXWCPD
     C*
     C* Key List for the EOB Auxiliary Master file
     C*
     C     EOKY          KLIST                                                  from FAEOP
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C* Key List for the Physician header file
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C* Key List for the Hospital header file
     C     EUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C* Key List for the Claim Master File                 from FAE1P
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C* Key List for the Case Detail File                  from FMCMP
     C*
     C     CAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C* Key List for the Provider Auxiliary File           from FDCAP
     C*
     C     P1KY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C* Key List for the Override File
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* Subscriber Master
     C*
     C     SMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C* User Parmeter File
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C* Fund Master
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*** BEG ADD *** 08/17/99 ****************************************
     C* Key List for the Case Detail File                  from FMCMP
     C*
     C     DAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXRCN
     C*** END ADD *** 08/17/99 ****************************************
     C*
     C* ENTRY PARMS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFDT             8
     C                   PARM                    QQTDT             8
     C                   PARM                    QQJDT             3
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQPROC            1
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
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C* Set ind for HCFA-1500 or UB-92 to be printed on routing page
     C     QQPROC        IFEQ      'P'
     C                   SETON                                        88
     C                   ELSE
     C                   SETOFF                                       88
     C                   END
     C*
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOFEIN        XXFEIN
     C     *LIKE         DEFINE    EHLIC#        XXFLIC
     C     *LIKE         DEFINE    P1MCR#        XXMCR#
     C     *LIKE         DEFINE    EOMOD         XXMOD
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    FNNAME        XXFNME
     C     *LIKE         DEFINE    EARCN         XXRCN
     C     *LIKE         DEFINE    EAWCPD        XXWCPD
     C     *LIKE         DEFINE    CAFDAT        ZZFDAT
     C     *LIKE         DEFINE    CATDAT        ZZTDAT
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFDT         XXFDT             8 0
     C                   MOVE      QQTDT         XXTDT             8 0
     C                   MOVE      QQFDT         XXFDT2            8 0
     C                   MOVE      QQTDT         XXTDT2            8 0
     C                   MOVE      QQPROC        XXPROC            1
     C                   MOVE      *ZEROS        XXFYR
     C                   MOVE      *ZEROS        XXCASE
     C                   MOVE      *ZEROS        XXEOB#
     C                   MOVE      '-'           XXHYP1
     C                   MOVE      *ZEROS        XXFEIN
     C                   Z-ADD     1             X                 5 0
     C*
     C                   Z-ADD     XXFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZFDAT
     C*
     C                   Z-ADD     XXTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZTDAT
     C*
     C* Convert system type 'P' to bill type 'N"
     C     XXPROC        IFEQ      'P'
     C                   MOVE      'N'           XXPROC            1
     C                   ENDIF
     C*
     C* CHAIN TO THE USER PARM FILE TO GET THE USER NAME
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C* GET THE OUTPUT ROUTING RECORDS
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C*
     C* READ THROUGH EOMB AUXILIARY FILE
     C*
     C                   Z-ADD     ZZFDAT        XXWCPD                                     FND
     C*
     C     EAKY1         SETLL     PEA01                                                    FND
     C     EAKY          READE     PEA01                                  50                FND
     C*
     C     *IN50         IFEQ      *OFF
     C     EAWCPD        ANDLE     ZZTDAT
     C*
     C                   Z-ADD     EAFYR         XXFYR
     C                   Z-ADD     EACASE        XXCASE
     C                   Z-ADD     EAEOB#        XXEOB#
     C*
     C     EOKY          CHAIN     PEO01                              60                    FND
     C*
     C* GET THE COMPANY NAME
     C*
     C     XXCO#         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* GET THE SUBSCRIBER NAME
     C*
     C     SMKY          CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C* CHAIN TO THE FUND MASTER FILE TO GET THE FUND NAME
     C*
     C                   MOVEL     *BLANKS       XXFNME
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* Remove leading blanks from Fund Name
     C*
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    FNNM(Z)                            60
     C                   MOVEA     FNNM(Z)       XXFNME
     C                   END
     C                   EXSR      FIRST
     C*
     C                   ELSE
     C                   EXSR      FIRST
     C*
     C                   END                                                    *IN50=OFF
     C*
     C     *IN50         DOWEQ     *OFF
     C     EAWCPD        ANDLE     ZZTDAT
     C*
     C     EAEDI         IFEQ      'Y'
     C     EOBTYP        ANDEQ     XXPROC
     C*
     C* ONLY PROCESS IF THE SYSTEM IS 'P' OR 'H'
     C*
     C     EOBTYP        IFEQ      'N'
     C     EOBTYP        OREQ      'H'
     C*
     C* Build case number
     C*
     C                   Z-ADD     EOFYR         XXYR              2 0
     C                   Z-ADD     EOCASE        XXCASE
     C                   Z-ADD     EOFYR         XXFYR
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT            8 0
     C*
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT            8 0
     C*
     C                   MOVE      EOEOB#        XXEOB#
     C                   MOVE      EOFEIN        XXFEIN
     C                   MOVE      EOMOD         XXMOD
     C*
     C     XXMOD         IFNE      *ZEROS
     C                   MOVE      '1'           *IN25
     C*
     C                   ELSE
     C                   MOVE      '0'           *IN25
     C                   END
     C*
     C* Get the physician and hospital header file information
     C     EASYS         IFEQ      'P'
     C     EHKY          CHAIN     PEH01                              60
     C*
     C                   MOVE      EHLIC#        XXFLIC
     C*
     C                   ENDIF                                                  EASYS=P
     C*
     C     EASYS         IFEQ      'H'
     C     EUKY          CHAIN     PEU01                              60
     C                   ENDIF                                                  EASYS=P
     C*
     C                   Z-ADD     EAWCPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXENDT            8 0
     C*
     C                   Z-ADD     EOCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCRDT            8 0
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C     P1KY          CHAIN     PP101                              60
     C*
     C     EUMCAR        IFNE      *BLANKS
     C                   MOVE      EUMCAR        XXMCR#
     C*
     C                   ELSE
     C                   MOVE      P1MCR#        XXMCR#
     C*
     C                   ENDIF                                                  EUMCAR IFNE
     C*
     C                   MOVE      *BLANKS       EMSG
     C*                    ----------
     C                   EXSR      EDIT1
     C*                    ----------
     C     X             IFGT      0
     C*
     C*                    -----------------
     C*                    Print Detail Line
     C*                    -----------------
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   END                                                    END 1 *INOF
     C                   EXCEPT    DETAIL
     C*
     C                   EXCEPT    HEAD2
     C*
     C     CAKY          CHAIN     PCA01                              61
     C*
     C     *IN61         DOWEQ     '0'
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFDAT            8 0
     C*
     C                   Z-ADD     CATDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTDAT            8 0
     C*
     C                   Z-ADD     CAACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C                   Z-ADD     CADSEQ        XXDSEQ
     C*
     C     *INOF         IFEQ      '1'
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C*
     C                   END                                                    END 1 *INOF
     C*
     C                   EXCEPT    DETLN2
     C     CAKY          READE     PCA01                                  61
     C                   ENDDO
     C*
     C     1             DO        X             Y                 4 0
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   END                                                    END 1 *INOF
     C                   EXCEPT    ERRORL                                       PRT ERR LINE
     C                   ENDDO
     C*
     C                   END                                                    END X GT 0
     C*
     C                   END                                                    END EAWCPD
     C*
     C                   END                                                    END EASYS
     C*
     C     EAKY          READE     PEA01                                  50     ALL CO/SUB/FND
     C*
     C* READ EOMB MASTER FILE RECORDS
     C                   Z-ADD     EAFYR         XXFYR
     C                   Z-ADD     EACASE        XXCASE
     C                   Z-ADD     EAEOB#        XXEOB#
     C*
     C     EOKY          CHAIN     PEO01                              60                    FND
     C*
     C                   ENDDO                                                  END DOWEQ 50
     C*                    ------------
     C     TERM          TAG
     C*                    ------------
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       Except Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*---------------------------------------------------------------
     C*                    BEGIN SUBROUTINE
     C**--------------------------------------------------------------
     CSR   FIRST         BEGSR
     C*                    ---------------------------------------
     C*                    If first time, print output routing and
     C*                    headings before printing detail line.
     C*                    ---------------------------------------
      ***** Beg Del ***** 08/26/14 *********************************************
     C***********        EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     CSR                 ENDSR
     C**---------------------------------------------------------------
     CSR   EDIT1         BEGSR
     C*                    --------------------------------------------
     C*                    All dates must be greater than accident date
     C*                    --------------------------------------------
     C                   Z-ADD     *ZEROS        X
     C     EOBTYP        IFNE      'X'
     C*
     C     CAKY          CHAIN     PCA01                              61
     C*
     C     *IN61         DOWEQ     '0'
     C*
     C     EOFRDT        IFGE      CAACDT
     C     EOTODT        ANDGE     CAACDT
     C     EOCRDT        ANDGE     CAACDT
     C     EAWCPC        ORGT      *ZEROS
     C     EAWCPD        ANDGE     CAACDT
     C                   ELSE
     C                   ADD       1             X
     C                   MOVEL     ERR(1)        EMSG(X)
     C                   ENDIF
     C*                    -------------------------------------------
     C*                    From/To serv not within range From/To date.
     C*                    -------------------------------------------
     C     EOFRDT        IFLE      CAFDAT
     C     EOTODT        ANDGE     CATDAT
     C                   ELSE
     C                   ADD       1             X
     C                   MOVEL     ERR(2)        EMSG(X)
     C                   ENDIF
     C*                    -------------------------------------
     C*                    From service date <> To service date.
     C*                    -------------------------------------
     C     CAFDAT        IFNE      CATDAT
     C                   ADD       1             X
     C                   MOVEL     ERR(3)        EMSG(X)
     C                   ENDIF
     C     CAKY          READE     PCA01                                  61
     C  N61              ENDDO
     C*** BEG ADD *** 08/17/99 ****************************************
     C*                    --------------------------------------
     C*                    No detail amount records for this EOB#
     C*                    --------------------------------------
     C*
     C                   Z-ADD     EARCN         XXRCN
     C*
     C     DAKY          CHAIN     PDA01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   ADD       1             X
     C                   MOVEL     ERR(13)       EMSG(X)
     C*
     C                   ENDIF
     C*** END ADD *** 08/17/99 ****************************************
     C*                    -------------------------------------
     C*                    Recv. date is not >= To service date.
     C*                    -------------------------------------
     C     EOCRDT        IFLT      EOTODT
     C                   ADD       1             X
     C                   MOVEL     ERR(4)        EMSG(X)
     C                   ENDIF
     C*                    --------------------------------------------
     C*                    If CK# > zeros, recv. DT. should not be zero
     C*                    --------------------------------------------
     C     EAWCPC        IFNE      *ZEROS
     C     EAWCPD        IFGT      *ZEROS
     C     EAPAID        ANDGT     *ZEROS
     C                   ELSE
     C                   ADD       1             X
     C                   MOVEL     ERR(6)        EMSG(X)
     C                   ENDIF
     C                   ELSE
     C     EAWCPD        IFGT      *ZEROS
     C     EAPAID        ANDGT     *ZEROS
     C     EAWCPC        IFEQ      *ZEROS
     C                   ADD       1             X
     C                   MOVEL     ERR(6)        EMSG(X)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*                    -------------------------
     C*                    Tape should equal to 'Y'.
     C*                    -------------------------
     C     EOBTYP        IFEQ      'P'
     C     EOBTYP        OREQ      'H'
     C     EAEDI         IFNE      'Y'
     C                   ADD       1             X
     C                   MOVEL     ERR(8)        EMSG(X)
     C                   ENDIF
     C                   ENDIF
     C*                    --------------------------------------------
     C*                    Diag. code should not equal to zeros/blanks.
     C*                    --------------------------------------------
     C     EASYS         IFEQ      'P'
     C*
     C     EHDIA1        IFEQ      *ZEROS
     C     EHDIA1        OREQ      *BLANKS
     C                   ADD       1             X
     C                   MOVEL     ERR(9)        EMSG(X)
     C                   ENDIF
     C*                    -------------------------------------
     C*                    Diag. code should not equal to zeros.
     C*                    -------------------------------------
     C     EHDIA2        IFEQ      *ZEROS
     C                   ADD       1             X
     C                   MOVEL     ERR(10)       EMSG(X)
     C                   ENDIF
     C*                    -------------------------------------
     C*                    Diag. code should not equal to zeros.
     C*                    -------------------------------------
     C     EHDIA3        IFEQ      *ZEROS
     C                   ADD       1             X
     C                   MOVEL     ERR(10)       EMSG(X)
     C                   ENDIF
     C*                    -------------------------------------
     C*                    Diag. code should not equal to zeros.
     C*                    -------------------------------------
     C     EHDIA4        IFEQ      *ZEROS
     C                   ADD       1             X
     C                   MOVEL     ERR(10)       EMSG(X)
     C                   ENDIF
     C*
     C                   ENDIF                                                  EASYS=P
     C*                    --------------------------------------------
     C*                    Diag. code should not equal to zeros/blanks.
     C*                    --------------------------------------------
     C     EASYS         IFEQ      'H'
     C*
     C     EUDIA1        IFEQ      *ZEROS
     C     EUDIA1        OREQ      *BLANKS
     C                   ADD       1             X
     C                   MOVEL     ERR(9)        EMSG(X)
     C                   ENDIF
     C*                    -------------------------------------
     C*                    Diag. code should not equal to zeros.
     C*                    -------------------------------------
     C     EUDIA2        IFEQ      *ZEROS
     C                   ADD       1             X
     C                   MOVEL     ERR(10)       EMSG(X)
     C                   ENDIF
     C*                    -------------------------------------
     C*                    Diag. code should not equal to zeros.
     C*                    -------------------------------------
     C     EUDIA3        IFEQ      *ZEROS
     C                   ADD       1             X
     C                   MOVEL     ERR(10)       EMSG(X)
     C                   ENDIF
     C*                    -------------------------------------
     C*                    Diag. code should not equal to zeros.
     C*                    -------------------------------------
     C     EUDIA4        IFEQ      *ZEROS
     C                   ADD       1             X
     C                   MOVEL     ERR(10)       EMSG(X)
     C                   ENDIF
     C*
     C                   ENDIF                                                  EASYS=P
     C*                    -------------------------------------------
     C*                    Medicare number should not equal to blanks.
     C*                    -------------------------------------------
     C     EASYS         IFEQ      'H'
     C     P1MCR#        IFEQ      *BLANKS
     C     EUMCAR        ANDEQ     *BLANKS
     C                   ADD       1             X
     C                   MOVEL     ERR(11)       EMSG(X)
     C                   ENDIF
     C                   ENDIF
     C*                    -------------------------------------------
     C*                    Check for invalid data in positions 3 to 9.
     C*                    -------------------------------------------
     C     EASYS         IFEQ      'P'
     C     PREFX         IFEQ      'MD'
     C     PREFX         OREQ      'DO'
     C     ALPHA         CHECK     MDA                                    30
     C                   TESTN                   MDN                  31
     C                   TESTN                   MDB                      32
     C                   SELECT
     C     *IN30         WHENEQ    '1'
     C                   ADD       1             X
     C                   MOVEL     ERR(12)       EMSG(X)
     C     *IN31         WHENEQ    '0'
     C                   ADD       1             X
     C                   MOVEL     ERR(12)       EMSG(X)
     C     *IN32         WHENEQ    '0'
     C                   ADD       1             X
     C                   MOVEL     ERR(12)       EMSG(X)
     C                   ENDSL
     C                   ELSE                                                     not MD/DO
     C     PREFX         IFEQ      'DS'
     C     PREFX         OREQ      'DC'
     C     PREFX         OREQ      'OD'
     C                   TESTN                   DDDN                 30
     C                   TESTN                   DDDB                     31
     C                   SELECT
     C     *IN30         WHENEQ    '0'
     C                   ADD       1             X
     C                   MOVEL     ERR(12)       EMSG(X)
     C     *IN31         WHENEQ    '0'
     C                   ADD       1             X
     C                   MOVEL     ERR(12)       EMSG(X)
     C                   ENDSL
     C                   ELSE                                                     not DS/DC/DO
     C     PREFX         IFEQ      'PT'
     C     PREFX         OREQ      'OT'
     C     PREFX         OREQ      'CR'
     C                   TESTN                   POCN                 30
     C                   TESTN                   POCB                     31
     C                   SELECT
     C     *IN30         WHENEQ    '0'
     C                   ADD       1             X
     C                   MOVEL     ERR(12)       EMSG(X)
     C     *IN31         WHENEQ    '0'
     C                   ADD       1             X
     C                   MOVEL     ERR(12)       EMSG(X)
     C                   ENDSL
     C                   ELSE                                                     not PT/OT/CR
     C     PREFX         IFEQ      'OP'
     C     ALL           CHECK     OPAB                                   30
     C                   SELECT
     C     *IN30         WHENEQ    '1'
     C                   ADD       1             X
     C                   MOVEL     ERR(12)       EMSG(X)
     C                   ENDSL
     C                   ELSE                                                     not OP
     C     PREFX         IFEQ      'DP'
     C                   TESTN                   DPN                  30
     C     NUM19B        CHECK     DPNB                                   31
     C                   SELECT
     C     *IN30         WHENEQ    '0'
     C                   ADD       1             X
     C                   MOVEL     ERR(12)       EMSG(X)
     C     *IN31         WHENEQ    '1'
     C                   ADD       1             X
     C                   MOVEL     ERR(12)       EMSG(X)
     C                   ENDSL
     C                   ENDIF                                                  MD/DO
     C                   ENDIF                                                  DS/DC/DO
     C                   ENDIF                                                  PT/OT/CR
     C                   ENDIF                                                  OP
     C                   ENDIF                                                  DP
     C                   ENDIF                                                  EOSYS = P
     C                   ENDIF                                                  ENDIF EOBTYP
     C*                    -----------------------------
     C*                    Tape should not equal to 'Y'.
     C*                    -----------------------------
     C     EOBTYP        IFEQ      'X'
     C     EAEDI         IFEQ      'Y'
     C                   ADD       1             X
     C                   MOVEL     ERR(7)        EMSG(X)
     C                   ENDIF
     C                   ENDIF
     C*
     CSR                 ENDSR
      *-----------------------------------------------------------
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
      *-----------------------------------------------------------
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE          2
     O                                           19 'FORM TYPE   :'
     O               88                          31 '(HCFA-1500)'
     O              N88                          27 '(UB-92)'
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
     O                                           30 'SELECTION:'
     O          E            ROUTE          2
     O                                           39 'DATE RANGE:'
     O                       XXFDT2              50 '  /  /    '
     O                                           54 'TO'
     O                       XXTDT2              65 '  /  /    '
     O          E            ROUTE          1
     O                                           39 'GROUP:'
     O                       XXFND         Z     43
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                              +  1 'INC.'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *-----------------------------------------------------------
     OQSYSPRT   E            HEAD           2 06
     O                       DSDATE        Y     11
     O                       CONAME              73
     O                       DSTIME             104 '  :  :  '
      *
     O          E            HEAD           2
     O                       ORPNAM              13
     O                       ORTITL              73
     O                                           99 'PAGE:'
     O                       PAGE               104
      *
     O          E            HEAD           2
     O                                           59 'DATE RANGE:'
     O                       XXFDT2              70 '  /  /    '
     O                                           74 'TO'
     O                       XXTDT2              85 '  /  /    '
     O          E            HEAD1          2
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#              17
     O                                           19 '-'
     O                       XXSBNM              60
      *
     O          E            HEAD1          2
     O                                           11 'GROUP.....:'
     O                       XXFND               17
     O                                           19 '-'
     O                       XXFNME              80
     O*
     O          E            HEAD1          1
     O                                           88 'DATE'
     O                                          100 'CARRIER'
     O          E            HEAD1          1
     O                                           66 'FROM'
     O                                           76 'TO'
     O                                           90 'PROVIDER'
     O                                          101 'RECEIVED'
     O                                          116 'AMOUNT'
     O          E            HEAD1          1
     O                                            8 'EOMB#'
     O                                           19 'CASE #'
     O                                           36 'TAX ID#/MOD'
     O                                           46 'LICENSE #'
     O                                           58 'MEDICARE #'
     O                                           66 'DATE'
     O                                           77 'DATE'
     O                                           88 'PAID'
     O                                           99 'DATE'
     O                                          114 'PAID'
     O*
     O          E            DETAIL      1  1
     O                       XXEOB#        Z      9
     O                       CASE                21
     O                       XXFEIN        ZB    32
     O               25                          33 '-'
     O               25      XXMOD         Z     36
     O                       XXFLIC         B    46
     O                       XXMCR#         B    58
     O                       XXFRDT         B    70 '  /  /    '
     O                       XXTODT         B    81 '  /  /    '
     O                       XXENDT         B    92 '  /  /    '
     O                       XXCRDT         B   103 '  /  /    '
     O                       EAPAID        3B   117
     O          E            HEAD2          1
     O                                           29 'DETAIL'
     O                                           40 'DETAIL'
     O          E            HEAD2          1
     O                                           19 'ACCIDENT'
     O                                           28 'FROM'
     O                                           39 'TO'
     O          E            HEAD2          1
     O                                           17 'DATE'
     O                                           28 'DATE'
     O                                           39 'DATE'
     O                                           50 'SEQ#'
     O*
     O          E            DETLN2         1
     O                       XXACDT         B    21 '  /  /    '
     O                       XXFDAT         B    32 '  /  /    '
     O                       XXTDAT         B    44 '  /  /    '
     O                       XXDSEQ        Z     49
      *
     O          E            ERRORL         1
     O                       EMSG(Y)             78
     O*
     O***********************************************************************
     O*
**   Error Message Array **
All dates must be greater than accident date.                       e1.
From/To service date are not within range of from/to date.          e2.
From service date does not equal to service date.                   e3.
Received date is not greater or equal to service date.              e4.
If check# equal zeros than all dates must be <= to paid date.       e5.
If check# <> to zeros than received date should not equal zeros.    e6.
Tape should not equal to 'Y'.                                       e7.
Tape should equal to 'Y'.                                           e8.
Diag. code should not equal to zeros or blanks.                     e9.
Diag. code should not equal to zeros.                              e10.
Medicare number should not equal to blanks.                        e11.
There are invalid data in positions 3 to 9 for this licence #.     e12.
There are no detail amount records for this EOB#.                  e13.
