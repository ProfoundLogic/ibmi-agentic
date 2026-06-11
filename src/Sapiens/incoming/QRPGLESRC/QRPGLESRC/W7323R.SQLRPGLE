     Hcopyright('(c) 2025 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)  bnddir('W7323R')
     H optimize(*none)
     H*****************************************************************
     H/TITLE W7324R - Deductible Billing Deletion
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*  Provides the ability to delete deductible billings for a     *
     H*  specific posting date. The entered posting must equal an     *
     H*  record posting dates with deductibles.  If A/R               *
     H*  (Accounts/Receivable) batch has been transferred.  If not    *
     H*  tranferred, then deletion of records will proceed.           *
     H*                                                               *
     H* CALLED FROM: Accounting Processing menu                       *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H*                                                               *
     H* CREATED:                                                      *
     H*                                                               *
     H* 10/22/13 C:180 SUPPORT WA136 83512                            *
     H*                Do not allow invoice deletion if there have    *
     H*                been recoveries.                               *
     H*                                                               *
     H* 08/26/99 C:112 PROJECT WC449                                  *
     H*                Provide the ability to delete the dedcutible   *
     H*                billings for a specific billing date.          *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/13/25 C:344 Project: WC2989                                *
     H*                Added capability to pass-thru/ call the newer  *
     H*                Deductible Billing Reprint prompt program.     *
     H*                                                               *
     H* 06/25/10 C:189 Project: WC1878                                *
     H*                 Add Single Employer Processing to the         *
     H*                 Deductible Invoice Deletion. The program has  *
     H*                 been coverted to SQLRPGLE for efficient       *
     H*                 processing                                    *
     H*                                                               *
     H* 03/25/10 C:33  015802 : WC1838                                *
     H*                A/R Batch records will now have more than a    *
     H*                 single group.  Don't delete the entire batch  *
     H*                 unless it's for a single group.               *
     H*                                                               *
     H* 03/18/10 C:136 PROJECT: 25WQI PRISS 25                        *
     H*                DELETE DEDUCTIBLE ACCUMULATION FILE            *
     H*                                                               *
     H* 08/21/01 C:15  Support WC048/28625                            *
     H*                Chgd edits to allow the deletion process if an *
     H*                A/R batch record is not found.                 *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  71 - 80  Used for error messaging                            *
     H*  85 - 88  Used for positioning cursor                         *
     H*  XX - XX  XXX............................XXX                  *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
     H*
     FW7323F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FWMFNP     UF   E             DISK
     F                                     RENAME(PFN01:DBRCDF)
     F* Deductible Detail File
      *** BEG DEL ***** 06/25/10 **************************************
     F********WDDDL2    UF   E           K DISK
      *** END DEL ***** 06/25/10 **************************************
     F* Employer A/R Batch File
      *** BEG DEL ***** 06/25/10 **************************************
     F***********wdeal4    if   e           K disk
     F**************                                     rename(Pea01:pea04) prefix(A_)
     F**************WDEAL4    UF   E           K DISK
     F* Deductible Detail File
     F**************WDDAL2    UF   E           K DISK
     F*********Wdeal29   uf a e           k disk    usropn
      *** END DEL ***** 06/25/10 **************************************
      *** BEG ADD ***** 06/25/10 **************************************
     Fwmupl     if   e           K disk
     fwmemp     if   e             disk    rename(pem01 : pem0p)
     fwmfnl     if   e           k disk
     fwtabl     if   e           k disk
      *** END ADD ***** 06/25/10 **************************************
      *** BEG ADD ***** 10/22/13 **************************************
     fWDDDL9    if   e           k disk
      *** END ADD ***** 10/22/13 **************************************
     D*
      *** BEG DEL ***** 06/25/10 **************************************
     D******/COPY SCOPYMT01I
      *** END DEL ***** 06/25/10 **************************************
      *** BEG ADD ***** 06/25/10 **************************************
     d getDDPCnt       pr
     d   xxpodt                       7  0
     d   xxdiv                        5  0
     d   xxcount                       n
     d   xxtransfer                    n
      *** END ADD ***** 06/25/10 **************************************
      *** Beg Add ***** 01/13/25 **************************************
     d DedBlDel        pr                  Extpgm('WTDEDBLDEL')
      *** End Add ***** 01/13/25 **************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Deductible Billing D-
     D                                     eletion')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D #OFSCR          C                   CONST(1)
      *** BEG ADD ***** 06/25/10 **************************************
     d XXSYTP          s                   like(SYTYPE)
     d XXBAT#          s              9  0
     d @batch#         s              9  0
     d X@PODT          s              7  0
     d xxbatch#        s              9  0 dim(9999)
     d xxtrans#        s               n   dim(9999)
     d   xxcount       s               n   inz(*off)
     d   xxtransfer    s               n   inz(*off)
     d found           s               n   inz(*off)
     d ##co#           s              3  0
     d ##fnam          s             40
     d ##fnd           s              3  0
     d inkc            s              1
     d error           s              1
     d inkl            s              1
     d rrnGrp          s              9  0
     d rrnEmp          s              9  0
     d xxdiv1          s              5  0
     d grpselect       s               n   inz(*off)
     d EmpSelect       s               n   inz(*off)
     d DivSelect       s               n   inz(*off)
     d Xcounter        s              5  0
     d Z               s              5  0
     d xxbata          s             11  0
      *** BEG ADD ***** 10/22/13 **************************************
     d XXEMP#          s                   LIKE(SCEMP#)
      *** END ADD ***** 10/22/13 **************************************
     d/copy wprgroup
     d/copy wpremplr
     C/COPY SCOPYMT01C
      *** END ADD ***** 06/25/10 **************************************
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @#MODE            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C*
     C*  KEY LIST FOR THE DEDUCTIBLE DETAIL FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPODT
      *** BEG ADD ***** 10/22/13 **************************************
     C     DDKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPODT
      *
     C     DDKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPODT
     C                   KFLD                    XXEMP#
      *
     C     DDKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPODT
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *** END ADD ***** 10/22/13 **************************************
     C*
     C*  KEY LIST FOR THE A/R batch file
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBAT#
     C*
     C***  Begin Add  ***  03/25/10  **************************************
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBAT#
     C                   KFLD                    XXFND
     C***  End   Add  ***  03/25/10  **************************************
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
      *** BEG ADD ***** 06/25/10 **************************************
     c     exky          klist
     c                   kfld                    ##co#
     c                   kfld                    scfnd
     C*
     c     eaky#         klist
     c                   kfld                    ##co#
     c                   kfld                    scfnd
     c                   kfld                    x@podt
     c                   kfld                    @batch#
     c                   kfld                    scemp#
     c                   kfld                    xxdiv
     C*
     c     eaky@         klist
     c                   kfld                    ##co#
     c                   kfld                    scfnd
     c                   kfld                    x@podt
     c                   kfld                    @batch#
     c                   kfld                    scemp#
     C*
     c     eaky$         klist
     c                   kfld                    ##co#
     c                   kfld                    scfnd
     c                   kfld                    x@podt
     c                   kfld                    @batch#
     C*
     c     tbkey         klist
     c                   kfld                    ##co#
     c                   kfld                    xxkey            10
      *** END ADD ***** 06/25/10 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    @#MODE        @@MODE
      *** BEG CHG ***** 06/25/10 **************************************
     C**** *LIKE         DEFINE    DDFND         XXFND
     C**** *LIKE         DEFINE    DDCO#         XXCO#
     C**** *LIKE         DEFINE    DDPODT        XXPODT
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMEFD1        XXPODT
      ***END  CHG ***** 06/25/10 **************************************
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
      *
      *** BEG ADD ***** 06/25/10 **************************************
     C                   time                    dsdttm
     C                   SELECT
     C                   WHEN      SCDIV <> *BLANK
     C                   EVAL      DIVSELECT = *ON
     C
     C                   WHEN      SCEMP# <> 0
     C                   EVAL      EMPSELECT = *ON
     C
     C                   OTHER
     C                   EVAL      GRPSELECT = *ON
     C                   ENDSL
      *
     c                   eval      found = validGrp(scfnd)
     c                   eval      *in65 = not(found)
     c                   eval      *in90 = *in65
     C*
     c*    exky          chain     pfn01
     c*                  eval      scname = fnname
     C*
     c                   if        scemp#<> 0     and not(*in90)
     c                   eval      found=validEmp(scemp#:0:rrnEmp:scfnd)
     C*
     c                   eval      *in66 = not(found)
     c                   eval      *in90 = *in66
     c                   endif
     C*
     c                   if        scdiv <> *blanks and not(*in90)
     c                   move(p)   scdiv         xxdiv             5 0
     c                   eval      found = validEmp(scemp#:xxdiv:rrnEmp:scfnd)
     c                   eval      *in67 = not(found)
     c                   eval      *in66 = not(found)
     c                   eval      *in90 = *in67
     c                   endif
      *
      *** END ADD ***** 06/25/10 **************************************
      *  DROP OUT OF LOOP IF *IN90 IS SET ON.
      *  INDICATORS 71 - 90 ARE RESET IN COPYBOOK SCOPYMT01C.
      *
     C                   DO
     C*
     C*  Posting date entered must be valid
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9071
     C                   LEAVE
     C                   ENDIF
     C*
      *** BEG ADD ***** 06/25/10 **************************************
     C                   MOVE      ##CO#         XXCO#
     C                   MOVE      ##FND         XXFND
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         XXPODT
     C                   CAllp     getDDPCnt(xxpodt:xxdiv:xxcount:xxtransfer)
     C*
     C                   If        xxcount = '0'
     C                   SETON                                        9072
     C                   Endif
     C*
      *** END ADD ***** 06/25/10 **************************************
     C*
      *** BEG DEL ***** 06/25/10 **************************************
     C* Check to ensure there are deductibles posted for the entered
     C* Post Date (SCPODT)
     C*
     C***************    MOVE      ##CO#         XXCO#
     C***************    MOVE      ##FND         XXFND
     C****************   Z-ADD     SCPODT        SRDTI
     C************       CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C**************     MOVEL     SRDTO         XXPODT
     C**** DDKY          CHAIN     PDD01                              60
     C***  *IN60         IFEQ      '1'
     C****               SETON                                        9072
     C****               LEAVE
     C**********         ENDIF
      *** END DEL ***** 06/25/10 **************************************
     C*
     C* Determine whether the A/R batch has been transferred by
     C* chaining to the A/R batch file (WDEAP) using the batch # from
     C* the Deduct Detail record (WDDDP/DDBAT#).  If the transfer date
     C* (WDEAP/EATFDT)is NOT zero, then the batch has been transferred
     C* and an error message is displayed
     C*
      *** BEG DEL ***** 06/25/10 **************************************
     C*************      MOVE      DDBAT#        XXBAT#
     C**** EAKY***       CHAIN     PEA01                              60
     C**** *IN60         IFEQ      '1'
     C*** Begin dlt *** 08/21/01 **************************************
     C******************** SETON                     9073
     C******************** LEAVE
     C*** End dlt ***** 08/21/01 **************************************
     C*************      ELSE
     C************       EATFDT        IFNE      *ZERO
     C************       SETON                                        9073
     C************       LEAVE
     C**********         ENDIF
     C***********        ENDIF
      *** END DEL ***** 06/25/10 **************************************
     C*
      *** BEGIN ADD *** 10/22/13 **************************************
      /Free
                        XXFND = SCFND;
                        XXEMP# = SCEMP#;
                        *IN74 = *OFF;

                        Select;
                        When scdiv <> *blanks;
                             Setll DDKY4 WDDDL9;
                             if %equal;
                             *in74 = *on;
                             *in90 = *on;
                             endif;
                        When xxemp#  <> *zeros;
                             Setll DDKY3 WDDDL9;
                             if %equal;
                             *in74 = *on;
                             *in90 = *on;
                             endif;
                        When xxfnd   <> *zeros;
                             Setll DDKY2 WDDDL9;
                             if %equal;
                             *in74 = *on;
                             *in90 = *on;
                             endif;
                        Endsl;
      /End-free
      *** END ADD   *** 10/22/13 **************************************
     C                   ENDDO
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C*  KMDO   - Process F13; user parameters
     C*================================================================
     C*
     C     KMDO          BEGSR
     C*
     C                   CALL      USPAPI                               99
     C*
     C                   EXSR      GETCOF
     C*
     C                   EXSR      PGMSTR
     C*
     C                   ENDSR                                                  KMDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVE      *ZEROS        SCPODT
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C* Read the 4C submit Deductible invoice print program (W7325C)
     C* and the Deductible detail report program (W7926C)
     C*
     C*
     C*
     C* Check to ensure there are deductibles posted for the entered
     C* Post Date (SCPODT)
     C*
      *** BEG DEL ***** 06/25/10 **************************************
     C**** DDKY *****    CHAIN     PDD01                              60
     C*
     C******IN60 *****   DOWEQ     *OFF
     C*
     C**************     MOVE      DDBAT#        XXBAT#
     C*
     C*  delete the Deduction Detail record matching entered Posted
     C*  Date
     C*
     C************       DELETE    PDD01
     C*
     C* Determine whether the A/R batch has been transferred by
     C* chaining to the A/R batch file (WDEAP) using the batch # from
     C* the Deduct Detail record (WDDDP/DDBAT#).  If there are no
     C* records or transfer date (WDEAP/EATFDT)is NOT zero, then the
     C* batch has been trasferred and the record cannot be deleted
     C*
     C***  Begin Chg  ***  03/25/10  **************************************
     C********** EAKY      CHAINPEA01                60
     C**** EAKY1 ****    CHAIN     PEA01                              60
     C**** *LIKE****     DEFINE    EABATA        XXBATA
     C**********         Z-ADD     EABATA        XXBATA
     C***  End   Chg  ***  03/25/10  **************************************
      *
     C***  *IN60****     DOWEQ     *OFF
      *
     C**** EATFDT*****   IFEQ      *ZERO
     C***  Begin Add  ***  03/25/10  **************************************
     C************       SUB       EAAMT         XXBATA                   60
     C***  End   Add  ***  03/25/10  **************************************
     C************       DELETE    PEA01
     C***************    ENDIF
     C*
     C***  Begin Chg  ***  03/25/10  **************************************
     C********** EAKY      READEPEA01                    60
     C***  EAKY1****     READE     PEA01                                  60
     C***  End   Chg  ***  03/25/10  **************************************
     C*
     C*************      ENDDO
     C*
     C***  Begin Add  ***  03/25/10  **************************************
     C* Update remaining items with new batch amount
     C**** EAKY ****     CHAIN     PEA01                              60
     C***  *IN60****     DOWEQ     *OFF
     C**** EATFDT****    IFEQ      *ZERO
     C***********        Z-ADD     XXBATA        EABATA
     C**********         UPDATE    PEA01
     C*********          ENDIF
     C**** EAKY***       READE     PEA01                                  60
     C************       ENDDO
     C***  End   Add  ***  03/25/10  **************************************
     C*
     C**** DDKY*****     READE     PDD01                                  60
     C*
     C*************      ENDDO
     C*
      *** BEG ADD ***** 03/18/10 *************************************
     C**** DDKY          CHAIN     PDA01                              60
     C***  *IN60         DOWEQ     *OFF
     C****               DELETE    PDA01
     C***  DDKY          READE     PDA01                                  60
     C*****              ENDDO
      *** END ADD ***** 03/18/10 *************************************
      *** END DEL ***** 06/25/10 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   EXSR      MVSCDB
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
     C*
      *** BEG CHG ***** 06/25/10 **************************************
     C*******     *INKM         CASEQ     *ON           KMDO                     90
     C     *inkj         caseq     *on           kjdo                     90
      *** END CHG ***** 06/25/10 **************************************
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      *** BEG ADD ***** 06/25/10 **************************************
     c                   if        *inkc = *off
     C                   Move      xxco#         @@co#             3
     C                   Move      scfnd         @@fnd             3
     C                   Move      scemp#        @@emp#            9
     C                   Move      xxpodt        @@podt            7
     C
     C                   CALL      'W7323CA'
     C                   PARM                    @@co#
     C                   PARM                    @@fnd
     C                   PARM                    @@emp#
     C                   PARM                    scdiv
     C                   PARM                    @@podt
     c                   endif
     C                   SETON                                        LR
      *** END ADD ***** 06/25/10 **************************************
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     Q1PRMS        IFGE      1
     C     @@PRDS        ANDNE     *BLANKS
     C                   MOVEL(P)  @@PRDS        ##DATA
     C                   ELSE
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
      *** BEG DEL ***** 06/25/10 **************************************
     C************       MOVE      ##FND         SCFND
     C*************      MOVEL     ##FNAM        SCNAME
      *** END DEL ***** 06/25/10 **************************************
     C*
     C                   CLEAR                   @@MODE
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVEL(P)  @#MODE        @@MODE
     C                   ENDIF                                                  Q1PRMS>=2
      *** Beg Add ***** 01/13/25 **************************************
       If @@Mode <> 'O';
          DedBlDel();
          *InLr = *On;
          Return;
       Endif;
      *** End Add ***** 01/13/25 **************************************
     C*
     C                   Z-ADD     *ZEROS        @@RRN             9 0
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C                   EXSR      MVSCDB
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   EXSR      MVSCDB
     C                   ENDSR
      *** BEG ADD ***** 06/25/10 **************************************
    c*====================================================================
    c* Search employers
    c*====================================================================
     c     EmplrSearch   begsr
     c                   if        scFnd = *zero
     c                   exsr      GroupSearch
     c                   endif
     c                   clear                   rrnEmp
     c                   if        scFnd <> *zero
     c                   eval      rrnEmp = EmpSrch(inkc:inkl:error:
     c                                              scFnd:##Co#)
     c                   if        rrnEmp <> *zero
     c     rrnEmp        chain     WMEMP
     c                   eval      scEmp# = EMEMP#
     c                   eval      scDiv  = %editc(EMDIV:'3')
     c                   setoff                                       3132
     c                   seton                                        33
     c                   endif                                                  rrnEMP
     c                   endif                                                  scfnd <> 0
     c                   if        rrnEmp = *zero
     c                   setoff                                       3133
     c                   seton                                        32
     c                   endif                                                  rrnEMP
     c                   endsr
    c*====================================================================
    c* Search groups
    c*====================================================================
     c     GroupSearch   begsr
     c                   eval      rrnGrp = GrpSrch(inkc:inkl:scFnd:##Co#)
     c                   if        rrnGrp <> *zero
     c     rrnGrp        chain     WMFNP
     c                   eval      scFnd  = FNFND
     c                   eval      scName = FNNAME
     c                   setoff                                       31
     c                   seton                                        32
     c                   endif                                                  WMFNP
     c                   if        rrnGrp = *zero
     c                   setoff                                       32
     c                   seton                                        31
     c                   endif                                                  WMFNP
     c                   endsr
    c*====================================================================
    c* KJDO   - Processing for F10
    c*====================================================================
     c     KJDO          begsr
     c                   select
     c                   when      hfld = 'SCFND'
     c                   exsr      GroupSearch
     c                   when      hfld = 'SCEMP#' or hfld = 'SCDIV'
     c                   exsr      EmplrSearch
     c                   endsl
     c                   endsr
     c*=====================================================================---*
     c* getDDPCnt  - gets the count of WDDDP  records
     c*=====================================================================---*
     p getDDPCnt       B
     d
     d getDDPcnt       PI
     d   x1podt                       7  0
     d   x1div                        5  0
     d   x1count                       n
     d   x1transfer                    n
     d inputSrchCnd    s            256a
     d SqlStmtStr      s            256a
     d openArrChar     s              3a
     d openArray       s               n   dim(3)

     c*
     c                   if        SCFND  <> 0
     c                   eval      inputSrchCnd = 'DDFND = ? AND DDPODT = ?'
     c                   eval      openArray(1) = *on
     c                   Endif
     c*
     c                   if        SCEMP# <> 0
     c                   eval      inputSrchCnd = %trim(inputSrchCnd) +
     c                             ' and ' + 'DDEMP# = ?'
     c                   eval      openArray(2) = *on
     c                   Endif
     c
     c                   if        SCDIV <> ' '
     c                   eval      inputSrchCnd = %trim(inputSrchCnd) +
     c                             ' and ' + 'DDDIV  = ?'
     c                   eval      openArray(3) = *on
     c                   Endif
     c
     c/exec SQL
     c+                  declare DARTScursor cursor
     c+                    with Hold
     c+                    for dynSqlStmt
     c/end-exec
     c
     c                   eval      sqlStmtStr = 'Select DISTINCT(DDBAT#) +
     c                             from  WDDDP where ' + inputSrchCnd
     c
     c/exec SQL
     c+                  prepare   dynSqlStmt
     c+                    from :sqlStmtStr
     c/end-exec
     c*
     c* OPEN THE CURSOR DEPENDING ON SCREEN PARAMETERS
     c*
     c                   movea     openArray     openArrChar
     c*
     c                   select
     c*
     c*only fnd and Posting date
     c                   when      openArrChar = '100'
     c/exec SQL
     c+                  open      DARTScursor
     c+                  using     :scfnd, :x1podt
     c/end-exec
     c*
     c*only fnd ,Emp and Posting date
     c                   when      openArrChar = '110'
     c/exec SQL
     c+                  open      DARTScursor
     c+                  using     :scfnd, :x1podt, :scemp#
     c/end-exec
     c*
     c*Fnd ,Emp, Division and Posting date
     c                   when      openArrChar = '111'
     c/exec SQL
     c+                  open      DARTScursor
     c+                  using     :scfnd, :x1podt, :scemp#, :x1div
     c/end-exec
     C                   ENDSL
     C
     C*First Time fetch the cursor
     C
     c/exec SQL
     c+                  fetch next
     c+                    from  DARTScursor
     c+                    into :XXBAT#
     c/end-exec
     C                   if        sqlstt = '02000'                             (No successful read)
     C                   eval      x1count = '0'
     C                   Endif
     c
     C*
     C                   IF        sqlstt = '00000'                             (Read was success)
     C                   eval      x1count = '1'                                (Any WDDDP rcd fnd)
     C*
     C                   endif
     c/exec SQL
     C+                  Close DARTScursor
     c/end-exec

     p getDDPcnt       E
      *** END ADD ***** 06/25/10 **************************************
