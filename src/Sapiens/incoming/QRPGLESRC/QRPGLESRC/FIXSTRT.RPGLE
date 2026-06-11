     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

      /title FIXSTRT - LOAD WMRTP State Rate Change File
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *****************************************************************
      * CALLED FROM:                                                  *
      * CALLED TO:                                                    *
      *                                                               *
      * Created 10/11/10  C:255  Project: WC1939                      *
      *****************************************************************
      * Revision:                                                     *
      *                                                               *
      * 01/30/18 C:291 PROJECT: WC2560                                *
      *                Add NY Rate Split to CTDATA                    *
      *                Added FltAdjStat compile time array            *
      *                                                               *
      * 07/16/14 C:136 Project: WC2295                                *
      *                Add OK Rate Split to CTDATA                    *
      *                                                               *
      * 06/16/14 C:136 Project: WC2283                                *
      *                Add Tn Rate Split to CTDATA                    *
      *                                                               *
      * 10/13/11  C:190  PROJECT: WC2079                              *
      *                  Modify program to write "IL" record.         *
      *                                                               *
      *****************************************************************
     FWMRTL     UF A E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     D                 ds
     D DSDTTM                        12  0
     D  DSTIME                        6  0 OVERLAY(DSDTTM)
     D  DSDATE                        6  0 OVERLAY(DSDTTM:7)
     D SRDTI           S              8  0
     D SRDTO           S             10
     D SRERR           S              1
     D SRFFMT          S              8
     D SRTFMT          S              8
     D XXDT            S              7  0
     D XXEFDT          S              7  0
     D XXST            S                   LIKE(RTST)
      *** beg add ***** 01/30/18 ******************************************
     D XXSTAT          S              5  0
      *** end add ***** 01/30/18 ******************************************
     DPSDS           ESDS                  EXTNAME(SZQ1P)
     D@@CO#            S                   LIKE(RTCO#)
      *** beg add ***** 06/16/14 ******************************************
      *** beg Chg ***** 01/30/18 ******************************************
     D***arySize          c                   const(4)
     DarySize          c                   const(5)
      *** end Chg ***** 01/30/18 ******************************************
      *** end add ***** 06/16/14 ******************************************
     d*** beg add *** 10/13/11 ********************************************
     d i               s              1  0 inz(1)
      * Compile time array
      *** beg chg ***** 06/16/14 ******************************************
     d*********** State           s              2    dim(2) ctdata perrcd(1)
     d*********** Effdate         s              8  0 dim(2) ctdata perrcd(1)
     d State           s              2    dim(arySize) ctdata perrcd(1)
     d Effdate         s              8  0 dim(arySize) ctdata perrcd(1)
      *** beg add ***** 01/30/18 ******************************************
     d FltAdjStat      s              5  0 dim(arySize) ctdata perrcd(1)
      *** end add ***** 01/30/18 ******************************************
      *** end chg ***** 06/16/14 ******************************************
     d*** end add *** 10/13/11 ********************************************
    C**********************************************************************
    C*Parm Lists/Key Lists
    C**********************************************************************
     C     TBKEY         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C     dtcymd        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
    C*
     C*Build key to chain State Rate Change File.
     c*** Beg Chg *** 10/13/11 ********************************************
     C***********        MOVEL     'OK'          XXST
     C***********        MOVEL     '09012010'    SRDTI
      *** BEG CHG ***** 06/16/14 ******************************************
     C***********        dow       i <=2
     C                   dow       i <= arySize
      *** END CHG ***** 06/16/14 ******************************************
     C                   eval      xxst = State(i)
     C                   eval      srdti = Effdate(i)
     c*** End Chg *** 10/13/11 ********************************************
     c*** Beg add *** 01/30/18 ********************************************
     C                   eval      xxstat = FltAdjStat(i)
     c*** End add *** 01/30/18 ********************************************
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     SRDTO         XXEFDT
     C*
     C     *LOVAL        SETLL     SMCOL
     C                   READ      PCO01                                  60
     C                   DOW       *IN60 = *OFF
     C                   MOVE      COCO#         @@CO#
     C     @@CO#         CHAIN     PFN01                              70
     C                   DOW       *IN70 = *OFF
     C                   EXSR      WRT
     C*
     C                   READ      PFN01                                  70
     C                   ENDDO                                                  END FUND MASTER
     C                   READ      PCO01                                  60
     C                   ENDDO                                                  END COMP# MASTER
      *** Beg Add *** 10/13/11 ********************************************
     C                   eval      i = i + 1
     C                   enddo                                                  i <=2
      *** End Add *** 10/13/11 ********************************************
     C*
     C                   EVAL      *INLR = *ON
     C*=====================================================================
     C     WRT           begsr
     C*=====================================================================
     C                   EXSR      INITZ
     C* If Record is not present in State Rate Change File then write new
     C     TBKEY         CHAIN     PRT01                              70
     C                   IF        *IN70=*ON
     C                   MOVE      @@CO#         RTCO#
     C                   MOVE      FNFND         RTFND
     C                   EVAL      RTST= XXST
     C                   MOVE      XXEFDT        RTEFDT
     C                   EVAL      RTOPT=' '
     C                   EVAL      RTUSER=Q1USER
     C                   EVAL      RTPGM =Q1PGM
     C                   MOVE      XXDT          RTDT
     C                   MOVE      DSTIME        RTTIME
     C                   EVAL      RTIUSR=Q1USER
     C                   EVAL      RTIPGM =Q1PGM
     C                   MOVE      XXDT          RTIDT
     C                   MOVE      DSTIME        RTITIM
      *** Beg Add *** 10/13/11 ********************************************
      *** Beg Chg *** 01/30/18 ********************************************
      ***                eval      rtstat = *zeros
     C                   eval      rtstat = xxStat
      *** End Chg *** 01/30/18 ********************************************
      *** End Add *** 10/13/11 ********************************************
     C*
     C                   WRITE     PRT01
     C                   ENDIF
     C                   ENDSR
     C*=====================================================================
     C     INITZ         BEGSR
     C*=====================================================================
     C                   TIME                    DSDTTM
     C                   EVAL      SRDTI = DSDATE
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C                   ENDSR
** CDDATA State                                                                           **10/13/11
OK
IL
TN
OK
NY
** CDDATA Effdate
09012010
09012011
07012014
02012014
04102017
** Flat Rate Adjustment Stat Code   FltAdjStat                                            **01/30/18
00000
00000
00000
00000
00994
