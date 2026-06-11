     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4131R - PREMIUM / PAYMENT COMPARISON REPORT.
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*     Program reads the workfile produced by SRRPTR (WXEBP)     *
     H*     and prints a report listing of premiums and payments based*
     H*     on whether an account is audited or not.  The report can  *
     H*     ordered by employer alpha, employer number, agent alpha,  *
     H*     or agent number.  The billing status can be audited, non- *
     H*     audited or both.  The account type can be estimated,      *
     H*     self reporting or both.  1 to 30 A/R codes can also be    *
     H*     chosen for the sort.  These values can be excluded or     *
     H*     included.                                                 *
     H*                                                               *
     H*     Printed info include: Employer Number, Employer Name,     *
     H*     Standard Premium, Normal Premium, Expense Constant, Annual*
     H*     Premium, Premium Payments, Expense Constant Paid, Audit   *
     H*     Flag, and either a Debit or Credit Balance.  For a sort   *
     H*     by agent # or agent name, both those fields will appear at*
     H*     the top.                                                  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     W4131C - OVERRIDE CL                                      *
     H*     RUN FROM MENU - WORKERS' COMP MAIN MENU                   *
     H*                     REPORTS MENU                              *
     H*                     ADMINISTRATIVE REPORTS                    *
     H*                     PREMIUM / PAYMENT COMPARISION REPORT      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     NONE                                                      *
     H*                                                               *
     H*  CREATED: 02/19/98        PROGRAMMER: 86       PROJECT: WC577 *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*   08   BATCH MODE                                             *
     H*   09   DEMAND MODE                                            *
     H*   20 - 49   WHICH A/R CODES TO PROCESS                        *
     H*   50   NEW AGENT                                              *
     H*   54   AUDIT TYPE RECORD                                      *
     H*   55   NON AUDIT TYPE RECORD                                  *
     H*   56   AUDITED  TOTALS                                        *
     H*   57   NON-AUDITED  TOTALS                                    *
     H*   58   INCLUDE OR EXCLUDE A/R CODES                           *
     H*   60   General Read & Chain                                   *
     H*   61   General Read & Chain                                   *
     H*   62   General Read & Chain                                   *
     H*   63   General Read & Chain                                   *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*   SMCOL   PCO01: CO                                   Company *
     H*   SMORL   POR01: CO/PGM                              Override *
     H*   WMUPL   PUP01: USER                               User Parm *
     H*   WMFNL   PFN01: CO/FND                          Group Master *
     H*   WMAGL   PAG02: CO/FEIN/MOD                     Agent Master *
     H*   WMEML   PEM01: CO/FND/EMP#/DIV              Employer Master *
     H*   WDELL   PEL01: CO/FND/EMP#/DIV/FYR          Employer Detail *
     H*   WMEAL8  PEA01: CO/FND/FYR/EMP#/DIV                  A/R FILE*
     H*   WMAAL1  PAL01: CO/KE                        AGENT ASSIGNMENT*
     H*   WTABL   PAB01: CO/KEY                             TABLE FILE*
     H*   WXEBL   PEB01: SEQUENTIAL READ                     WORK FILE*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 02/19/04 C:136 PROJECT: WC1033                                *
     H*                Add code for new A/R type of 'T' (True Void)   *
     H*                                                               *
     H* 11/28/01  C:69   Support: 29582                               *
     H*                  Added seton LR when no records selected so   *
     H*                  Report Ordering & Sorting will not have error*
     H*                                                               *
     H* 06/20/00  C:35   PROJECT: SUPPORT WA071 #23271                *
     H*                  INCLUDE/EXCLUDE LOGIC WAS INCORRECT.         *
     H*                                                               *
     H* 04/10/00  C:118  PROJECT: WC987                               *
     H*                  LOAD ALTERNATE COMPANY NAME FROM FUND MASTER *
     H*                  IF AVAILABLE.                                *
     H*                                                               *
     H* 04/12/99  C:90   PROJECT: 96WAD (IN BASE: 07/19/99 C:33 WC899)*
     H*                  DO NOT PROCESS ACCOUNT CURRENT POLICIES.     *
     H*                                                               *
     H* 03/05/99  C:65   PROJECT: SUPPORT WA031 #16972                *
     H*                  USE THE RUN DATE WHEN FINDING PAYMENTS.      *
     H*                                                               *
     H* 11/07/98  C:108  PROJECT: WC577                               *
     H*                  CORRECT VERBIAGE IN ROUTING.                 *
     H*                                                               *
     H* 10/21/98  C:35   PROJECT: SUPPORT WA031 #14556                *
     H*                  CHANGED MAIN READ LOOP TO USE INDICATOR 64   *
     H*                  SINCE INDICATOR 60 WAS BEING SET BY OTHER    *
     H*                  LOGIC.                                       *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMEAL8    IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWXEBL     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F*****************************************************************

     D*
     D* Define Sort Selection Compile time Array
     D*
     D SRT             S             25    DIM(4) CTDATA PERRCD(1)              Sort Sequence
     D*
     D* Define arrays for A/R Code selection criteria
     D COD             S              2    DIM(30) ASCEND                       selected a/r code
     D DES             S             30    DIM(30)                              description
     D CDE             S              2    DIM(30)                              a/r output code
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  KEY LIST FOR THE GROUP MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* Key List for the Agent Assignment Master File
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EBAGT#
     C*
     C*  KEY LIST FOR THE AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C                   KFLD                    EBFYR
     C*
     C*  KEY LIST FOR THE A/R FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*_______________________________________________________________
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    EBAGT#        SVAGT#
     C*_______________________________________________________________
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQRNDT            8
     C                   PARM                    QQFYR             3
     C                   PARM                    QQACST            1
     C                   PARM                    QQCD             60
     C                   PARM                    QQARFL            1
     C                   PARM                    QQSORT            2
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQQUE            10
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQRNDT        X8RNDT            8 0
     C*** BEGIN ADD ***  03/05/99  ************************************
     C                   Z-ADD     X8RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT            7 0
     C*** END ADD *****  03/05/99  ************************************
     C*
     C                   EXSR      SR1ST
     C*
     C*===============================================================
     C*   MAIN LOOP
     C*===============================================================
     C*
     C*  Read Work file records
     C*
     C                   READ      PEB01                                  60
     C*
     C*     *** If no records in workfile
     C*
     C     *IN60         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
      **** Begin add **** 11/28/01 *********************************************
     C                   SETON                                        LR
      **** End   add **** 11/28/01 *********************************************
     C                   RETURN
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*     *** While read Work File record
     C*
     C     *LOVAL        SETLL     PEB01
      **** BEGIN CHG **** 10/21/98 *********************************************
     C********** *IN60     DOWEQ*OFF
     C**********           READ PEB01                    60
     C******** 60          LEAVE
     C     *IN64         DOWEQ     *OFF
     C                   READ      PEB01                                  64
     C   64              LEAVE
      **** END   CHG **** 10/21/98 *********************************************
     C*
     C*     *** If Agent Name or Number Sort
     C*
     C     QQSORT        IFEQ      'AN'
     C     QQSORT        OREQ      'AA'
     C                   MOVE      *ON           *IN50
     C                   ENDIF                                                              T#
     C*
     C*     *** If NO Agent
     C*
     C   50EBAGT#        IFEQ      *ZERO
     C                   ITER
     C                   ENDIF                                                  EBAGT#=*ZERO
     C*
     C*     *** If Agent Break, and Agent Sort
     C*
     C   50EBAGT#        IFNE      SVAGT#
     C                   EXSR      AGTBRK
     C                   ENDIF                                                  EBAGT#<>SVAGT#
     C*
     C*     *** If Audited or Non-audited only
     C*
     C     QQACST        IFEQ      'A'
     C                   MOVE      *ON           *IN56
     C                   ELSE                                                               T#
     C     QQACST        IFEQ      'N'
     C                   MOVE      *ON           *IN57
     C                   ENDIF                                                              T#
     C                   ENDIF                                                              T#
     C*
     C*     *** Write detail record
     C*
     C                   EXSR      SRDET
     C                   ENDDO                                                  *IN60=*OFF
     C  N50              EXCEPT    TOTAL
     C   50              EXCEPT    AGTOT
     C*
     C*  End of Processing
     C*
     C     END           TAG
      /Free
       // ***** Beg Add ***** 08/26/14 ********************************
       xxPrt = '0';
       Chain orKy POR01;
       If %found;
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 ********************************
      /End-free
     C                   MOVE      *ON           *INLR
     C*===============================================================
     C*  SR1ST - Set-up and Initialize
     C*===============================================================
     CSR   SR1ST         BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     *ZERO         SVAGT#
     C                   MOVE      *ON           *INOF
     C                   MOVE      'Y'           XX1ST             1
     C                   MOVE      'Y'           XX1STA            1
     C                   MOVE      QQFRDT        X1FDT             8 0
     C                   MOVE      QQTODT        X1TDT             8 0
     C                   MOVE      QQRNDT        X1RDT             8 0
     C*
     C     QQARFL        IFEQ      'E'
     C                   MOVE      *ON           *IN58
     C                   ELSE
     C                   MOVE      *OFF          *IN58
     C                   ENDIF
     C*
     C*  SET UP AND INITIALIZE
     C*
     C     QQQUE         COMP      *BLANK                             080809
     C*
     C*  GET USER, COMPANY, AND GROUP INFORMATION
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C     COKY          CHAIN     PCO01                              60
     C     FNKY          CHAIN     PFN01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C                   Z-ADD     X1FDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         RSFDT             8 0
     C*
     C                   Z-ADD     X1TDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         RSTDT             8 0
     C*
     C                   Z-ADD     X1RDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         RSRDT             8 0
     C*
     C     QQSORT        IFEQ      'AN'
     C                   MOVEL     SRT(1)        RSSRT            25
     C                   ELSE
     C     QQSORT        IFEQ      'AA'
     C                   MOVEL     SRT(2)        RSSRT
     C                   ELSE
     C     QQSORT        IFEQ      'NA'
     C                   MOVEL     SRT(3)        RSSRT
     C                   ELSE
     C                   MOVEL     SRT(4)        RSSRT
     C                   END
     C                   END
     C                   END
     C*
     C* Sort the Code Array
     C*
     C                   MOVEA     QQCD          COD
     C                   SORTA     COD
     C                   MOVE      *BLANKS       CDE
     C                   MOVE      *BLANKS       DES
     C*
     C     QQARFL        IFNE      *BLANKS
     C*
     C* Fill the Code Decription Array which Corresponds to the Code
     C*
     C                   Z-ADD     1             Z                 2 0
     C     1             DO        30            Y                 2 0
     C*
     C* Indicators 20 to 49 are used for Output Purposes
     C*
     C     COD(Y)        IFNE      *BLANKS
     C     Z             ADD       19            Z1                2 0
     C                   MOVE      '1'           *IN(Z1)
     C                   MOVEL(P)  COD(Y)        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     COD(Y)        CDE(Z)
     C                   MOVEL     TTDESC        DES(Z)
     C                   ADD       1             Z                 2 0
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ELSE
     C*
     C* PRINT *ALL CODES ON ROUTING PAGE
     C*
     C                   MOVE      '*ALL'        RSCODE            4
     C*
     C                   END                                                    COD = BLANKS
     C*
     C*  GET THE DISTRIBUTION INFORMATION AND PRINT
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C                   ENDIF                                                  ORTITL<>*BLANKS
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C********  N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
     C*  Center Text in Field
     C*
     C                   MOVEL     ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        ORTITL
     C*
     C*  Center Company name in display field
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL(P)  FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C*
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
     C*
     C                   ENDSR                                                  SR1ST
     C*===============================================================
     C*    DETAIL SUBROUTINE
     C*===============================================================
     CSR   SRDET         BEGSR
     C*
     C                   DO        1
     C*
     C* Get the Employer Detail Record
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     QQACST        IFEQ      'A'
     C     ELFAF         ANDNE     'A'
     C                   LEAVE
     C                   ELSE
     C     QQACST        IFEQ      'N'
     C     ELFAF         ANDEQ     'A'
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C* Initialize Detail Fields
     C*
     C                   MOVE      *BLANKS       XXNAUD
     C                   MOVE      *BLANKS       XXNAME
     C                   Z-ADD     0             XXTMP1
     C                   Z-ADD     0             XXTMP2
     C                   Z-ADD     0             XXTMP3
     C                   Z-ADD     0             XXTMP4
     C                   Z-ADD     0             XXTMP5
     C                   Z-ADD     0             XXTMP6
     C                   Z-ADD     0             XXCRTB
     C                   Z-ADD     0             XXDBTB
     C                   Z-ADD     0             XXTBAL           13 2
     C                   Z-ADD     0             XXSTDP           11 2
     C                   SETOFF                                       5455
     C*
     C* Calculate Detail Record Amounts
     C*
     C     ELFAF         IFNE      'A'
     C     ELSTPR        MULT(H)   ELPROR        XXTMP1           11 2
     C     ELANPR        SUB       ELPRTD        XXTMP2           11 2
     C                   Z-ADD     ELANPR        XXTMP4           11 2
     C                   MOVE      ' '           XXNAUD            1
     C                   MOVE      *ON           *IN54
     C                   ELSE
     C                   Z-ADD     ELASPR        XXTMP1
     C                   Z-ADD     ELANMP        XXTMP2
     C                   Z-ADD     ELAAPR        XXTMP4
     C                   MOVE      'A'           XXNAUD
     C                   MOVE      *ON           *IN55
     C                   END
     C*
     C**Chain to the A/R File to get payment info
     C*
     C     EAKY          CHAIN     PEA01                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C*** BEGIN ADD ***  03/05/99  ************************************
     C     EAPODT        IFLE      XXRNDT
     C*** END ADD *****  03/05/99  ************************************
     C                   EXSR      ARSR
     C*** BEGIN ADD ***  03/05/99  ************************************
     C                   ENDIF                                                  EAPODT IFLE
     C*** END ADD *****  03/05/99  ************************************
     C*
     C     EAKY          READE     PEA01                                  61
     C                   ENDDO
     C*
     C                   Z-ADD     ELPRTD        XXTMP3           11 2
     C                   Z-ADD     XXTMP1        XXSTDP           11 2
     C                   Z-ADD     XXTMP2        XXNRMP           11 2
     C                   Z-ADD     XXTMP3        XXEXPC           11 2
     C                   Z-ADD     XXTMP4        XXANLP           11 2
     C                   Z-ADD     XXTMP5        XXPRMP           11 2
     C                   Z-ADD     XXTMP6        XXEXPP           11 2
     C     XXPRMP        ADD       XXEXPP        XXSUBT           11 2
     C   54ELANPR        SUB       XXSUBT        XXSUB2           11 2
     C   55ELAAPR        SUB       XXSUBT        XXSUB2           11 2
     C     XXSUB2        IFGE      0
     C                   Z-ADD     XXSUB2        XXDBTB           11 2
     C                   ELSE
     C                   Z-ADD     XXSUB2        XXCRTB           11 2
     C                   END
     C*
     C* Get the Employer Master Record
     C*
     C     EMKY          CHAIN     PEM01                              61
     C                   MOVE      *BLANKS       XXNAME
     C     *IN61         IFEQ      '0'
     C*
     C                   MOVEL     EMNAME        XXNAME           20
     C                   END                                                    Emp Mast End
     C*
     C* Check for Page Overflow
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C* Print the Detail Line
     C*
      **** BEGIN ADD **** 04/12/99 ************************************
     C     EBACUR        IFNE      'Y'
      **** END   ADD **** 04/12/99 ************************************
     C                   EXCEPT    DETL
     C*
     C* Add Detail Fields to Total Fields
     C*
     C* ADD TOTAL FOR AUDITED ACCOUNTS
     C*
     C     ELFAF         IFEQ      'A'
     C                   ADD       XXSTDP        XASTDP           13 2
     C                   ADD       XXNRMP        XANRMP           13 2
     C                   ADD       XXEXPC        XAEXPC           13 2
     C                   ADD       XXANLP        XAANLP           13 2
     C                   ADD       XXPRMP        XAPRMP           13 2
     C                   ADD       XXEXPP        XAEXPP           13 2
     C                   ADD       XXCRTB        XACRTB           13 2
     C                   ADD       XXDBTB        XADBTB           13 2
     C*
     C                   ELSE
     C*
     C* ADD TOTAL FOR NO AUDITED ACCOUNTS
     C*
     C                   ADD       XXSTDP        XNSTDP           13 2
     C                   ADD       XXNRMP        XNNRMP           13 2
     C                   ADD       XXEXPC        XNEXPC           13 2
     C                   ADD       XXANLP        XNANLP           13 2
     C                   ADD       XXPRMP        XNPRMP           13 2
     C                   ADD       XXEXPP        XNEXPP           13 2
     C                   ADD       XXCRTB        XNCRTB           13 2
     C                   ADD       XXDBTB        XNDBTB           13 2
     C*
     C                   ENDIF
     C*
     C* ADD TOTAL AGENTS
     C*
     C   50              ADD       XXSTDP        XGSTDP           13 2
     C   50              ADD       XXNRMP        XGNRMP           13 2
     C   50              ADD       XXEXPC        XGEXPC           13 2
     C   50              ADD       XXANLP        XGANLP           13 2
     C   50              ADD       XXPRMP        XGPRMP           13 2
     C   50              ADD       XXEXPP        XGEXPP           13 2
     C   50              ADD       XXCRTB        XGCRTB           13 2
     C   50              ADD       XXDBTB        XGDBTB           13 2
     C*
     C* ADD TOTAL FOR ALL ACCOUNTS
     C*
     C                   ADD       XXSTDP        XRSTDP           13 2
     C                   ADD       XXNRMP        XRNRMP           13 2
     C                   ADD       XXEXPC        XREXPC           13 2
     C                   ADD       XXANLP        XRANLP           13 2
     C                   ADD       XXPRMP        XRPRMP           13 2
     C                   ADD       XXEXPP        XREXPP           13 2
     C                   ADD       XXCRTB        XRCRTB           13 2
     C                   ADD       XXDBTB        XRDBTB           13 2
     C*
      **** BEGIN ADD **** 04/12/99 ************************************
     C                   ENDIF                                                  EBACUR = Y
      **** END   ADD **** 04/12/99 ************************************
     C                   ENDDO
     C*
     C                   ENDSR                                                  SR1ST
     C*===============================================================
     C*  A/R SUBROUTINE - PROCESS ALL PAYMENTS FOR THE POLICY
     C*===============================================================
     CSR   ARSR          BEGSR
     C*
     C                   SETOFF                                       62
     C*
     C     QQARFL        IFNE      *BLANKS
     C                   EXSR      CHARSR
      **** BEGIN ADD **** 06/20/00 ************************************
     C                   ELSE
     C                   SETON                                        62
      **** END   ADD **** 06/20/00 ************************************
     C                   ENDIF
     C*
      **** BEGIN CHG **** 06/20/00 ************************************
     C********** *IN62     IFEQ *OFF
     C     *IN62         IFEQ      *ON
      **** END   CHG **** 06/20/00 ************************************
     C                   DO
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      '0'
     C     TTCTYP        IFEQ      'P'
     C     EAARTY        IFEQ      'P'
      *** BEG ADD ***** 02/19/04 **********************************
     C     EAARTY        OREQ      'T'
      *** END ADD ***** 02/19/04 **********************************
     C                   ADD       EAAMT         XXTMP5           11 2
     C                   ENDIF
     C     EAARTY        IFEQ      'R'
     C     EAARTY        OREQ      'N'
     C     EAARTY        OREQ      'V'
     C                   SUB       EAAMT         XXTMP5
     C                   ENDIF
     C                   ENDIF
     C*
     C     TTCTYP        IFEQ      'E'
     C     EAARTY        IFEQ      'P'
     C                   ADD       EAAMT         XXTMP6           11 2
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C*
     C                   ENDSR
     C*===============================================================
     C*  CHARSR - CHECK IF A/R CODE WILL BE EXCLUDED OR INCLUDED
     C*===============================================================
     C*
     CSR   CHARSR        BEGSR
     C*
     C                   Z-ADD     1             Y
      **** BEGIN CHG **** 06/20/00 ************************************
     C********** EAARCD    LOKUPCOD,Y                62
     C     EAARCD        LOOKUP    COD(Y)                                 62
      **** END   CHG **** 06/20/00 ************************************
     C     QQARFL        IFEQ      'E'
     C     *IN62         IFEQ      *OFF
     C                   MOVE      *ON           *IN62
     C                   ELSE
     C                   MOVE      *OFF          *IN62
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*===============================================================
     C*  AGTBRK - Agent Break
     C*===============================================================
     C*
     CSR   AGTBRK        BEGSR
     C*
     C*  Print Totals for agent
     C*
     C     XX1ST         IFEQ      'Y'
     C                   MOVE      'N'           XX1ST
     C                   ELSE
     C                   EXCEPT    AGTOT
     C                   SETON                                        OF
     C                   ENDIF                                                  XX1ST=Y
     C*
     C* ZERO OUT AGENT TOTALS
     C*
     C                   Z-ADD     *ZERO         XGSTDP           13 2
     C                   Z-ADD     *ZERO         XGNRMP           13 2
     C                   Z-ADD     *ZERO         XGEXPC           13 2
     C                   Z-ADD     *ZERO         XGANLP           13 2
     C                   Z-ADD     *ZERO         XGPRMP           13 2
     C                   Z-ADD     *ZERO         XGEXPP           13 2
     C                   Z-ADD     *ZERO         XGCRTB           13 2
     C                   Z-ADD     *ZERO         XGDBTB           13 2
     C*
     C*  Get Agent name
     C*
     C                   EXSR      SRAGT
     C*
     C                   Z-ADD     EBAGT#        SVAGT#
     C*
     C                   ENDSR                                                  AGTBRK
     C*================================================================
     C* SUBROUTINE    **  SRAGT  **  GET AGENT NAME
     C*================================================================
     C     SRAGT         BEGSR
     C*
     C     AAKY          CHAIN     PAA01                              60
     C     *IN60         IFEQ      '0'
     C     AGKY          CHAIN     PAG01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     AGNAME        AGTNAM           40
     C                   ELSE
     C                   MOVE      *BLANKS       AGTNAM
     C                   END
     C                   ELSE
     C                   MOVE      *BLANKS       AGTNAM
     C                   END
     C*
     C                   ENDSR
     C*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14  **********************************
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
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          1
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14  **********************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          1
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14  **********************************
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
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14  **********************************
     O          E            ROUTE       2  2
     O                                           16 'SELECTION:'
     O          E            ROUTE          1
      ****** BEG CHG ***** 11/07/98 ***********************************
     O*********************************  26 'COMPANY  . . . . :'
     O                                           26 'COMPANY  . . . . . :'
      ****** END CHG ***** 11/07/98 ***********************************
     O                       XXCO#         Z   +001
     O          E            ROUTE          1
      ****** BEG CHG ***** 11/07/98 ***********************************
     O*********************************  26 'GROUP  . . . . . :'
     O                                           26 'GROUP  . . . . . . :'
      ****** END CHG ***** 11/07/98 ***********************************
     O                       XXFND         Z   +001
     O          E            ROUTE          1
      ****** BEG CHG ***** 11/07/98 ***********************************
     O*********************************  26 'DATE RANGE . . . :'
     O                                           26 'POLICY EFF DT RANGE:'
      ****** END CHG ***** 11/07/98 ***********************************
     O                       RSFDT             +  1 '  /  /    '
     O                                         +  2 '-'
     O                       RSTDT             +  2 '  /  /    '
     O*
     O          E            ROUTE          1
      ****** BEG CHG ***** 11/07/98 ***********************************
     O*********************************  26 'AS OF POLICY DATE:'
     O                                           26 'PAYMENTS AS OF DATE:'
      ****** END CHG ***** 11/07/98 ***********************************
     O                       RSRDT             +  1 '  /  /    '
     O*
     O          E            ROUTE          1
      ****** BEG CHG ***** 11/07/98 ***********************************
     O*********************************  26 'ACCOUNT TYPE . . :'
     O                                           26 'ACCOUNT TYPE . . . :'
      ****** END CHG ***** 11/07/98 ***********************************
     O                       QQTYPE            +  1
     O*
     O          E            ROUTE          1
      ****** BEG CHG ***** 11/07/98 ***********************************
     O*********************************  26 'BILLING STATUS . :'
     O                                           26 'BILLING STATUS . . :'
      ****** END CHG ***** 11/07/98 ***********************************
     O                       QQACST            +  1
     O*
     O          E            ROUTE          1
      ****** BEG CHG ***** 11/07/98 ***********************************
     O*********************************  26 'SORT BY  . . . . :'
     O                                           26 'SORT BY  . . . . . :'
      ****** END CHG ***** 11/07/98 ***********************************
     O                       RSSRT             +  1
     O*
     O          E            ROUTE       1  1
     O                                           30 'A/R CODES'
     O              N58                          42 'INCLUDED IN'
     O               58                          42 'EXCLUDED IN'
     O                                           54 'CALCULATION'
     O          E      N20   ROUTE          1
     O                       RSCODE              26
     O*
     O          E            ROUTE          1
     O                  20                       11 '1.'
     O                       CDE(1)              14
     O                       DES(1)              45
     O                  30                       50 '11.'
     O                       CDE(11)             53
     O                       DES(11)             84
     O                  40                       89 '21.'
     O                       CDE(21)             92
     O                       DES(21)            123
     O          E            ROUTE          1
     O                  21                       11 '2.'
     O                       CDE(2)              14
     O                       DES(2)              45
     O                  31                       50 '12.'
     O                       CDE(12)             53
     O                       DES(12)             84
     O                  41                       89 '22.'
     O                       CDE(22)             92
     O                       DES(22)            123
     O          E            ROUTE          1
     O                  22                       11 '3.'
     O                       CDE(3)              14
     O                       DES(3)              45
     O                  32                       50 '13.'
     O                       CDE(13)             53
     O                       DES(13)             84
     O                  42                       89 '23.'
     O                       CDE(23)             92
     O                       DES(23)            123
     O          E            ROUTE          1
     O                  23                       11 '4.'
     O                       CDE(4)              14
     O                       DES(4)              45
     O                  33                       50 '14.'
     O                       CDE(14)             53
     O                       DES(14)             84
     O                  43                       89 '24.'
     O                       CDE(24)             92
     O                       DES(24)            123
     O          E            ROUTE          1
     O                  24                       11 '5.'
     O                       CDE(5)              14
     O                       DES(5)              45
     O                  34                       50 '15.'
     O                       CDE(15)             53
     O                       DES(15)             84
     O                  44                       89 '25.'
     O                       CDE(25)             92
     O                       DES(25)            123
     O          E            ROUTE          1
     O                  25                       11 '6.'
     O                       CDE(6)              14
     O                       DES(6)              45
     O                  35                       50 '16.'
     O                       CDE(16)             53
     O                       DES(16)             84
     O                  45                       89 '26.'
     O                       CDE(26)             92
     O                       DES(26)            123
     O          E            ROUTE          1
     O                  26                       11 '7.'
     O                       CDE(7)              14
     O                       DES(7)              45
     O                  36                       50 '17.'
     O                       CDE(17)             53
     O                       DES(17)             84
     O                  46                       89 '27.'
     O                       CDE(27)             92
     O                       DES(27)            123
     O          E            ROUTE          1
     O                  27                       11 '8.'
     O                       CDE(8)              14
     O                       DES(8)              45
     O                  37                       50 '18.'
     O                       CDE(18)             53
     O                       DES(18)             84
     O                  47                       89 '28.'
     O                       CDE(28)             92
     O                       DES(28)            123
     O          E            ROUTE          1
     O                  28                       11 '9.'
     O                       CDE(9)              14
     O                       DES(9)              45
     O                  38                       50 '19.'
     O                       CDE(19)             53
     O                       DES(19)             84
     O                  48                       89 '29.'
     O                       CDE(29)             92
     O                       DES(29)            123
     O          E            ROUTE          1
     O                  29                       11 '10.'
     O                       CDE(10)             14
     O                       DES(10)             45
     O                  39                       50 '20.'
     O                       CDE(20)             53
     O                       DES(20)             84
     O                  49                       89 '30.'
     O                       CDE(30)             92
     O                       DES(30)            123
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
      ****** BEG CHG ***** 11/07/98 ***********************************
     O************************           46 'VE DATA SOLUTIONS. INC,'
     O***                                47 'VE DATA SOLUTIONS, INC.,'
      ****** END CHG ***** 11/07/98 ***********************************
      ****** END DLT ***** 01/28/10 **********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O*_______________________________________________________________
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             120
     O                       DSDATE        Y    198
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL             130
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       FNFND         Z   +  3
     O                       FNNAME            +  2
     O                                          187 'PAYMENTS AS OF:'
     O                       X8RNDT        Y   +  1
     O          E    50      HEAD           1
     O                                           13 'AGENT NUMBER:'
     O                       EBAGT#            +  2
     O          E    50      HEAD           2
     O                                           06 'AGENT:'
     O                       AGTNAM            +  3
     O          E            HEAD           1
     O                                            3 'POL'
     O                                           13 'EMPLOYER'
     O                                           20 'DIV'
     O                                           35 'EMPLOYER NAME'
     O                                           60 'STANDARD'
     O                                           78 'NORMAL'
     O                                           95 'EXPENSE'
     O                                          114 'ANNUAL'
     O                                          133 'PREMIUM'
     O                                          151 'EXP CONSTANT'
     O                                          169 'CREDIT'
     O                                          187 'DEBIT'
     O                                          198 'AUDIT'
     O          E            HEAD           2
     O                                            3 'YR'
     O                                           59 'PREMIUM'
     O                                           79 'PREMIUM'
     O                                           96 'CONSTANT'
     O                                          115 'PREMIUM'
     O                                          134 'PAYMENTS'
     O                                          143 'PAID'
     O                                          170 'BALANCE'
     O                                          189 'BALANCE'
     O                                          198 'FLAG'
     O*
     O* Detail Line Specs
     O*
     O          E            DETL           1
     O                       ELFYR         Z      3
     O                       ELEMP#        Z     13
     O                       ELDIV         Z     20
     O                       XXNAME              42
     O                       XXSTDP        J     61
     O                       XXNRMP        J     80
     O                       XXEXPC        J     97
     O                       XXANLP        J    116
     O                       XXPRMP        J    135
     O                       XXEXPP        J    152
     O                       XXCRTB        J    171
     O                       XXDBTB        J    190
     O                       XXNAUD             197
     O*
     O* Total Line Specs
     O*
     O          E   N56      TOTAL       2
     O                                           27 'NON-AUDIT TOTALS:'
     O                       XNSTDP        J     61
     O                       XNNRMP        J     80
     O                       XNEXPC        J     97
     O                       XNANLP        J    116
     O                       XNPRMP        J    135
     O                       XNEXPP        J    152
     O                       XNCRTB        J    171
     O                       XNDBTB        J    190
     O*
     O          E   N57      TOTAL       2
     O                                           23 'AUDIT TOTALS:'
     O                       XASTDP        J     61
     O                       XANRMP        J     80
     O                       XAEXPC        J     97
     O                       XAANLP        J    116
     O                       XAPRMP        J    135
     O                       XAEXPP        J    152
     O                       XACRTB        J    171
     O                       XADBTB        J    190
     O*
     O          E   N56N57   TOTAL       2
     O                                           24 'REPORT TOTALS:'
     O                       XRSTDP        J     61
     O                       XRNRMP        J     80
     O                       XREXPC        J     97
     O                       XRANLP        J    116
     O                       XRPRMP        J    135
     O                       XREXPP        J    152
     O                       XRCRTB        J    171
     O                       XRDBTB        J    190
     O*
     O          E            AGTOT       2
     O                                           24 'AGENT TOTALS:'
     O                       XGSTDP        J     61
     O                       XGNRMP        J     80
     O                       XGEXPC        J     97
     O                       XGANLP        J    116
     O                       XGPRMP        J    135
     O                       XGEXPP        J    152
     O                       XGCRTB        J    171
     O                       XGDBTB        J    190
     O*_______________________________________________________________
     O*
     O          E            NOREC       3  1
     O                                           80 '*** NO RECORD FOUND'
     O                                         +  1 'FOR THE SELECTED'
     O                                         +  1 'CRITERIA. ***'
**     SRT
AGENT NUMBER SEQUENCE
AGENT NAME SEQUENCE
EMPLOYER NUMBER SEQUENCE
EMPLOYER NAME SEQUENCE
