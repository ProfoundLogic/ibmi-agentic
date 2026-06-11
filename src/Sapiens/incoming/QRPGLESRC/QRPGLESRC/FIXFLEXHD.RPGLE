     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

      /title FIXFLEXHD - Transfer Flex Heading Information
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *****************************************************************
      * CALLED FROM:                                                  *
      * CALLED TO:                                                    *
      *                                                               *
      * Created 07/28/10  C:255  Project: 132WOJ                      *
      *****************************************************************
      * Revision:                                                     *
      *                                                               *
      * 04/19/11 C:136 Support: WA099 75581                           *
      *                Default sixth flex heading to PTYP and         *
      *                not be required if CLTID <> '132'              *
      *                                                               *
      * 02/21/11 Project 132WOJ Priss#37                              *
      *                          1) Populate maintenace fields for fi-*
      *                             le WMEXP.                         *
      *                                                               *
      * 12/30/10 Support 75013 : 1) Populate maintenace fields,       *
      *                          2) default fld6 only for client 132. *
      *                          3) write fields in WDFHP if WMEXP fl-*
      *                             ex heading values are populated   *
      *                                                               *
      *****************************************************************
    F*  Physical file definition
     FWMFNL     IF   E           K DISK
     F***  Beg   Chg  ***  12/30/10  **************************************
     F***********WMEXL     IF   E           K DISK
     FWMEXL     UF   E           K DISK
     F***  End   Chg  ***  12/30/10  **************************************
     FWDFHL     UF A E           K DISK
     D***  Beg   Add  ***  12/30/10  **************************************
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D srdti           s              8  0
     D srdto           s             10
     D XXDT            s              7  0
     DPSDS           ESDS                  Extname(SZQ1P)
     D srffmt          s              8
     D srtfmt          s              8
     D srerr           s              1
     D***  End   Add  ***  12/30/10  **************************************
     C*
     C***  Beg   Del  ***  12/30/10  **************************************
     C******ENTRY        PLIST
     C*******            PARM                    CO#               3
     C***  End   Del  ***  12/30/10  **************************************
      *
     C     ADMKEY        KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
      *
     C***  Beg   Add  ***  12/30/10  **************************************
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C***  End   Add  ***  12/30/10  **************************************
     C***  Beg   Chg  ***  12/30/10  **************************************
     C************       MOVE      CO#           @@CO#             3 0
     C*****@@CO#**       Chain     PFN01                              60
     C     *loval        setll     PFN01
     C                   Read      PFN01                                  60
     C***  End   Chg  ***  12/30/10  **************************************
     C                   Dow       *In60 =*Off
     C* Look records in Admin/Darts Control file
     C     ADMKEY        Chain     PEX01                              60
     C                   If        *In60 = *off
     C     ADMKEY        Chain     PFH01                              60
     C                   If        *In60 = *on
     C*Write Record,If not found in WDFHP file
     C***  Beg   Chg  ***  12/30/10  **************************************
     C*********          MOVE      @@CO#         FHCO#
     C                   MOVE      FNCO#         FHCO#
     C***  End   Chg  ***  12/30/10  **************************************
     C                   MOVE      FNFND         FHFND
     C*FIELD#1
     C                   Eval      FHFH#1=EXEHD1
     C                   Eval      FHFH1Q=EXERQ1
     C                   Eval      FHFH1W=EXERW1
     C                   Eval      FHF1TB=EXTBD1
     C                   Eval      FHF1DF=EXFDF1
     C*FIELD#2
     C                   Eval      FHFH#2=EXEHD2
     C                   Eval      FHFH2Q=EXERQ2
     C                   Eval      FHFH2W=EXERW2
     C                   Eval      FHF2TB=EXTBD2
     C                   Eval      FHF2DF=EXFDF2
     C***  Beg   Add  ***  12/30/10  **************************************
     C* Clear the fields after moving in WDFHP.
     C*FIELD#1
     C                   Eval      EXEHD1=*BLANKS
     C                   Eval      EXERQ1=*BLANKS
     C                   Eval      EXERW1=*BLANKS
     C                   Eval      EXTBD1=*BLANKS
     C                   Eval      EXFDF1=*BLANKS
     C*FIELD#2
     C                   Eval      EXEHD2=*BLANKS
     C                   Eval      EXERQ2=*BLANKS
     C                   Eval      EXERW2=*BLANKS
     C                   Eval      EXTBD2=*BLANKS
     C                   Eval      EXFDF2=*BLANKS
     C***  End   Add  ***  12/30/10  **************************************
     C*FIELD#6
     C***  Beg   Add  ***  12/30/10  **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C                   IF        CLTID = '132'
     C***  End   Add  ***  12/30/10  **************************************
     C                   Eval      FHFH#6= 'Type of Policy'
     C                   Eval      FHFH6Q= 'Y'
     C                   Eval      FHFH6W= 'Y'
     C                   Eval      FHF6TB= 'PTYP'
     C                   Eval      FHF6DF= *BLANKS
     C***  Beg   Add  ***  12/30/10  **************************************
     C                   EndIf                                                  CLTID = '132'
      *** beg add ***** 04/19/11 ******************************************
     c                   if        cltid <> '132'
     C                   eval      FHFH#6= 'Type of Policy'
     C                   eval      FHFH6Q= *blank
     C                   eval      FHFH6W= *blank
     C                   eval      FHF6TB= 'PTYP'
     C                   eval      FHF6DF= *blanks
     c                   endif
      *** end add ***** 04/19/11 ******************************************
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         XXDT
     C*
     C                   EVAL      FHUSER =Q1USER
     C                   EVAL      FHIUSR =Q1USER
     C                   EVAL      FHPGM  =Q1PGM
     C                   EVAL      FHIPGM  =Q1PGM
     C                   MOVE      dstime        FHTIME
     C                   MOVE      dstime        FHITIM
     C                   MOVE      xxdt          FHDT
     C                   MOVE      xxdt          FHIDT
     C*Updating maintenance fields of WMEXP
     C***  Beg   Add  ***  02/21/11  **************************************
     C                   EVAL      EXUSER =Q1USER
     C                   EVAL      EXPGM  =Q1PGM
     C                   MOVE      dstime        EXTIME
     C                   MOVE      xxdt          EXDT
     C***  End   Add  ***  02/21/11  **************************************
     C                   update    PEX01                                        update wmexp
     C***  End   Add  ***  12/30/10  **************************************
     C                   Write     PFH01
     C                   EndIf                                                  For WDFHL Ends
     C*
     C                   EndIf                                                  For WMEXL Ends
     C*
     C***  Beg   Chg  ***  12/30/10  **************************************
     C*****@@CO#         ReadE     PFN01                                  60
     C                   Read      PFN01                                  60
     C***  End   Chg  ***  12/30/10  **************************************
     C                   Enddo                                                  For Fund Master
     C                   EVAL      *Inlr = *On
