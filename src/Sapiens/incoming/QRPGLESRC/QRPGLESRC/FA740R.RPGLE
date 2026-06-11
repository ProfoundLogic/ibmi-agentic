     H**********************************************************************
     H* (C)COPYRIGHT PROGRESSIVE DATA SOLUTIONS INC., 2004
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: FA740R
    H* DESCRIPTION: API - UPDATE PROVIDER BILLS HEADER EDI FLAGS
     H**********************************************************************
     H* CREATED: 04/15/04 C: 079 PROJECT: FL046
     H**********************************************************************
     H* REVISIONS:
     H*
     H* 11/08/13  C: 274  Support: 83991
     H*                   Remove calls to FA746R.
     H*
     H* 08/03/10  C: 062  Support: MC048 73699
     H*                   Don't call license check process if called from
     H*                   F0493R.
     H*
     H* 07/12/10  C: 079  Support: 73533
     H*                   Add logic to check for valid license prefix.
     H*
     H**********************************************************************
     F*
     F*** beg chg *** 07/12/10 *********************************************
     F**** FAEOP     IF   E             DISK
     FFAEOP     UF   E             DISK
     F*** end chg *** 07/12/10 *********************************************
     FFAEHL     UF   E           K DISK
     FFAEUL     UF   E           K DISK
     FFAEPL     UF   E           K DISK
     FFAEDL     UF   E           K DISK
     F*** beg add *** 07/12/10 *********************************************
     FFDCALH    IF   E           K DISK
     F*** end add *** 07/12/10 *********************************************
     F*
     D*
     D psds          esds                  extname(szq1p)
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##rrn           s              9  0
     D @@rrn           s              9  0
     D @@rtcd          s              1
     D xxdt            s                   like(eodt)
     D kkco#           s                   like(eoco#)
     D kksub#          s                   like(eosub#)
     D kkfnd           s                   like(eofnd)
     D kkfyr           s                   like(eofyr)
     D kkcase          s                   like(eocase)
     D kkeob#          s                   like(eoeob#)
     D*** beg add *** 07/12/10 *********************************************
     D ##ast           s                   like(eoast)
     D ##lic           s                   like(ehflic)
     D ##rtcd          s              1A
     D ##btyp          s                   like(eobtyp)
     D ##sdte          s                   like(eofrdt)
     D*** end add *** 07/12/10 *********************************************
     D*** beg add *** 08/03/10 *********************************************
     D ediCall         s               n
     D ##pgm           s             10a
     D ##err           s              1a
     D*** end add *** 08/03/10 *********************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     C     genky         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*
     C     *entry        plist
     C                   parm                    @@rrn
     C                   parm                    @@rtcd
     C*
     C*=====================================================================
     C* MAIN PROGRAM
     C*=====================================================================
     C*** beg add *** 08/03/10 *********************************************
     C*
     C* Check to see where this program was called.
     C*
     C                   eval      ediCall=*off
     C*
     C                   call      'SRCHKPGM'                           99
     C                   parm      'F0493R'      ##PGM
     C                   parm      *blanks       ##ERR
     C*
     C                   if        ##err=*on
     C                   eval      ediCall=*on
     C                   endif
     C*** beg add *** 08/03/10 *********************************************
     C*
     C                   eval      ##rrn  = @@rrn
     C                   eval      @@rtcd = *blank
     C*
     C     ##rrn         chain     peo01                              99
     C*
     C                   if        *in99 = *off
     C*
     C                   exsr      loadkeys
     C*
     C*** beg add *** 07/12/10 *********************************************
     C*
     C* Get the earliest service date
     C*
     C     genky         chain     pca01                              99
     C                   if        *in99 = *off
     C                   eval      ##sdte = cafdat
     C                   endif
     C*** end add *** 07/12/10 *********************************************
     C*
     C                   select
     C*
     C                   when      eobtyp = 'N'
     C                   exsr      updfaehp
     C*
     C                   when      eobtyp = 'H'
     C                   exsr      updfaeup
     C*
     C                   when      eobtyp = 'X'
     C                   exsr      updfaepp
     C*
     C                   when      eobtyp = 'D'
     C                   exsr      updfaedp
     C*
     C                   other
     C                   eval      @@rtcd = 'F'
     C*
     C                   endsl
     C*
     C*** beg add *** 07/12/10 *********************************************
     C                   exsr      updfaeop
     C*** end add *** 07/12/10 *********************************************
     C*
     C                   else
     C                   eval      @@rtcd = 'F'
     C*
     C                   endif
     C*
     C*
     C                   eval      *inlr = *on
     C                   return
     C*** beg add *** 07/12/10 *********************************************
     C*
     C*=====================================================================
     C* SUBROUTINE Checklic
     C*=====================================================================
     C*
     C     Checklic      begsr
     C*
     C*** beg add *** 08/03/10 *********************************************
     C*
     C* Don't process if called from F0493R.
     C*
     C                   if        not ediCall
     C*** end add *** 08/03/10 *********************************************
     C*
     C* Check license number prefix for Florida bills
     C*
     C                   if        eoast = 'FL'
     C*
     C                   select
     C*
     C                   when      eobtyp = 'N'
     C                   eval      ##lic = ehflic
     C*
     C                   when      eobtyp = 'H'
     C                   eval      ##lic = euflic
     C*
     C                   when      eobtyp = 'X'
     C                   eval      ##lic = epplic
     C*
     C                   when      eobtyp = 'D'
     C                   eval      ##lic = edflic
     C*
     C                   endsl
     C*
     C*** Beg Del *** 11/08/13 *********************************************
     C******************
     C****************** call      'FA746R'
     C****************** parm      eoast         ##ast
     C****************** parm                    ##lic
     C****************** parm      ' '           ##rtcd
     C****************** parm      eobtyp        ##btyp
     C****************** parm                    ##sdte
     C******************
     C****************** if        ##rtcd = 'Y'
     C****************** eval      eotape = 'N'
     C******************
     C****************** else
     C****************** eval      eotape = 'E'
     C******************
     C****************** endif
     C*** End Del *** 11/08/13 *********************************************
     C*
     C*** Check Operating license number
     C*
     C                   if        eobtyp = 'H' and euolic <> *blanks
     C                             and eotape = 'E'
     C*
     C*** Beg Del *** 11/08/13 *********************************************
     C****************
     C****************   call      'FA746R'
     C****************   parm      eoast         ##ast
     C****************   parm      euolic        ##lic
     C****************   parm      ' '           ##rtcd
     C****************   parm      eobtyp        ##btyp
     C****************   parm                    ##sdte
     C****************
     C****************   if        ##rtcd = 'Y'
     C****************   eval      eotape = 'N'
     C****************   endif
     C*** End Del *** 11/08/13 *********************************************
     C*
     C                   endif
     C*
     C                   endif
     C*** beg add *** 08/03/10 *********************************************
     C                   endif
     C*** end add *** 08/03/10 *********************************************
     C*
     C                   endsr
     C*** end add *** 07/12/10 *********************************************
     C*
     C*=====================================================================
     C* SUBROUTINE loadkeys
     C*=====================================================================
     C*
     C     loadkeys      begsr
     C                   eval      kkco#  = eoco#
     C                   eval      kksub# = eosub#
     C                   eval      kkfnd  = eofnd
     C                   eval      kkfyr  = eofyr
     C                   eval      kkcase = eocase
     C                   eval      kkeob# = eoeob#
     C                   endsr
     C*** beg add *** 07/12/10 *********************************************
     C*=====================================================================
     C* SUBROUTINE updfaeop
     C*=====================================================================
     C*
     C     updfaeop      begsr
     C*
     C*
     C                   eval      eopgm  = q1pgm
     C*
     C                   time                    sys_time_n
     C                   move      *date         sys_date
     C     *hms          move      sys_time_n    sys_time
     C                   move      sys_time      eotime
     C     *cymd         move      sys_date      eodt
     C*
     C                   update    peo01
     C*
     C                   endsr
     C*** end add *** 07/12/10 *********************************************
     C*=====================================================================
     C* SUBROUTINE updfaehp
     C*=====================================================================
     C*
     C     updfaehp      begsr
     C*
     C     genky         chain     peh01                              99
     C*
     C                   if        not *in99
     C*
     C*** beg add *** 07/12/10 *********************************************
     C                   exsr      checklic
     C*** end add *** 07/12/10 *********************************************
     C*
     C                   eval      ehedi  = eotape
     C                   eval      ehuser = q1user
     C                   eval      ehpgm  = q1pgm
     C*
     C                   time                    sys_time_n
     C                   move      *date         sys_date
     C     *hms          move      sys_time_n    sys_time
     C                   move      sys_time      ehtime
     C     *cymd         move      sys_date      ehdt
     C*
     C*
     C                   update    peh01
     C*
     C                   endif
     C*
     C                   endsr
     C*=====================================================================
     C* SUBROUTINE updfaeup
     C*=====================================================================
     C*
     C     updfaeup      begsr
     C     genky         chain     peu01                              99
     C                   if        not *in99
     C*
     C*** beg add *** 07/12/10 *********************************************
     C                   exsr      checklic
     C*** end add *** 07/12/10 *********************************************
     C*
     C                   eval      euedi  = eotape
     C                   eval      euuser = q1user
     C                   eval      eupgm  = q1pgm
     C*
     C                   time                    sys_time_n
     C                   move      *date         sys_date
     C     *hms          move      sys_time_n    sys_time
     C                   move      sys_time      eutime
     C     *cymd         move      sys_date      eudt
     C*
     C*
     C                   update    peu01
     C*
     C                   endif
     C                   endsr
     C*=====================================================================
     C* SUBROUTINE updfaepp
     C*=====================================================================
     C*
     C     updfaepp      begsr
     C     genky         chain     pep01                              99
     C                   if        not *in99
     C*
     C*** beg add *** 07/12/10 *********************************************
     C                   exsr      checklic
     C*** end add *** 07/12/10 *********************************************
     C*
     C                   eval      epedi  = eotape
     C                   eval      epuser = q1user
     C                   eval      eppgm  = q1pgm
     C*
     C                   time                    sys_time_n
     C                   move      *date         sys_date
     C     *hms          move      sys_time_n    sys_time
     C                   move      sys_time      eptime
     C     *cymd         move      sys_date      epdt
     C*
     C                   update    pep01
     C*
     C                   endif
     C                   endsr
     C*=====================================================================
     C* SUBROUTINE updfaedp
     C*=====================================================================
     C*
     C     updfaedp      begsr
     C     genky         chain     ped01                              99
     C                   if        not *in99
     C*
     C*** beg add *** 07/12/10 *********************************************
     C                   exsr      checklic
     C*** end add *** 07/12/10 *********************************************
     C*
     C                   eval      ededi  = eotape
     C                   eval      eduser = q1user
     C                   eval      edpgm  = q1pgm
     C*
     C                   time                    sys_time_n
     C                   move      *date         sys_date
     C     *hms          move      sys_time_n    sys_time
     C                   move      sys_time      edtime
     C     *cymd         move      sys_date      eddt
     C*
     C                   update    ped01
     C                   endif
     C                   endsr
     C
