     H*****************************************************************
     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H*****************************************************************
     H/TITLE W0069R50 : EDI Claim Control Maintenance - Master.
     H*****************************************************************
     H*****************************************************************
     H*  DESCRIPTION: EDI Claim Control Maintenance - Master.         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0069R5 - EDI Claim Control Maintenance.                   *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 03/24/10  PROGRAMMER: C:232 PROJECT: 139WNN         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  02/14/22 C:079 Project: WC2797
     H*                 Add IAIABC 3.1.4 fields
     H*
     H*  07/14/21 C:079 Project: WC2816
     H*                 Add Exclude USL&H checkbox
     H*
     H*  05/30/18 C:180 Project: WC2515
     H*                 Add release 3.1
     H*
     H*****************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0069f50  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmixp     uf a e             disk
     F                                     rename(pix01:dbrcdf)

     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMIXL     IF   E           K DISK    PREFIX(@)
     FSTCTL1    IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)

     F*  GENERIC TABLE FILE
     FWTGTL     IF   E           K DISK
     F*  STATE MASTER
     FSTSTL3    IF   E           K DISK


    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'EDI Claim Control Maintenance'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
      *
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *
     C*   KEY LIST for Generic Table File
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY

     C*  EDI CLAIMS CONTROL FILE
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XXTRID
     C                   KFLD                    XXEDIF

     C*  DARTS TABLE CODE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD

     C*  KEY LIST FOR THE SYSTEM-WIDE STATE CODE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
      *
     C     IXkey2        klist
     C                   kfld                    XXCO#
     C                   kfld                    SCEDST
     C                   kfld                    XXTRID
     C                   kfld                    SCEDIF
      *
    C*================================================================
    C*  *LIKE DEFN statements
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    CONAME        XXCONM
     C     *LIKE         DEFINE    CTRMCD        XXRMCD
     C     *LIKE         DEFINE    SCEDST        XXJRST
     C     *LIKE         DEFINE    IXTRID        XXTRID
     C     *LIKE         DEFINE    IXEDIF        XXEDIF
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     C     *LIKE         DEFINE    STKEY         XXKEY
     C     *LIKE         DEFINE    IXCSDT        XXCSDT
     C     *LIKE         DEFINE    IXCSDT        XXDSDT
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTTBID        QQTBID
     C     *LIKE         DEFINE    GTKEY         QQKEY
     C     *LIKE         DEFINE    Q1USER        XXUSER
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      *
    C*================================================================
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
      *
     C                   EVAL      *IN90 = '0'
      *
     C* EDI state should not be blanks..
     C*
     C                   IF        SCEDST = *BLANKS
     C                   EVAL      *IN62 = '1'
     C                   EVAL      *IN90 = '1'
     C                   ELSE
     C                   EVAL      *IN62 = '0'
     C                   ENDIF
      *
     C*  EDI State must be in state table
      *
     C                   IF        *IN90 <> '1'
      *
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     SCEDST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C                   IF        *IN60 = '1'
     C                   EVAL      *IN63 = '1'
     C                   EVAL      *IN90 = '1'
     C                   ELSE
     C                   EVAL      *IN63 = '0'
     C                   ENDIF
      *
     C                   ENDIF
      *
     C*    If present, start must be valid date.
     C*
     C                   IF        *IN90 <> '1'
     C     SCCSDT        IFGT      0
     C                   Z-ADD     SCCSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCSDT
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN20
     C                   EVAL      *IN90 = '1'
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
      *
     C*  The max # of transactions cannot be less than 0
     C*
     C                   IF        *IN90 <> '1'
      *
     C     SCMAXT        IFLT      0
     C                   MOVE      '1'           *IN21
     C                   EVAL      *IN90 = '1'
     C                   ENDIF
      *
     C                   ENDIF
      *
     C*  The version flag must be '1' or '2'.
     C*
      /Free
       If *in90 = *Off;
          If SCrls = *blanks;
             *in25 = *on;
             *in90 = *on;
          Else;
             *in25 = *off;
          Endif;
       Endif;
      /End-Free
      *
     C*  If EDI Format not entered, check the fund file to see if it is required.
     C                   IF        *IN90 <> '1' AND SCEDIF = *BLANKS
      *
     C                   CALL      'WA075R'
     C                   PARM                    XXCO#
     C                   PARM      000           ##fnd
     C                   PARM      *BLANK        QQRTCD
     C     QQRTCD        IFEQ      'F'
     C                   SETON                                        656990
     C                   ENDIF
      *
     C                   ENDIF
      *
     C*  The EDI Format ID Must be in the table file if entered.
      *
     C                   IF        *IN90 <> '1'
      *
     C                   IF        SCEDIF <> *BLANKS
     C                   MOVEL     'EDIF'        XXTBID
     C                   MOVEL     SCEDIF        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C                   IF        *IN60 = '0' AND GTSTAT = 'A'
     C                   ELSE
     C                   EVAL      *IN66 = '1'
     C                   EVAL      *IN90 = '1'
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
      *
     C* System should not allow the duplicate records...
      *
     C                   IF        *IN90 <> '1'
      *
     C                   IF        SCEDIF <> IXEDIF
     C     IXKEY2        CHAIN     PIX01
     C                   IF        %FOUND(WMIXL)
     C                   SETON                                          6990
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
       //*** Beg Add *** 02/14/22 *********************************************
       ms441Date = 'Invalid Date';
       er441Date = *off;
     C     scD441D       IFGT      0
     C                   Z-ADD     scD441D       SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scD441D
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           er441Date
     C                   EVAL      *IN90 = '1'
     C                   ENDIF
     C                   ENDIF
      *
       //*** End Add *** 02/14/22 *********************************************
      *
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   EVAL      SCEDST =  IXJRST
      *
     C                   Z-ADD     IXCSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCSDT
      *
     C                   EVAL      SCMAXT =  IXMAXT
     C                   EVAL      SCRLS  =  IXRLS
     C                   EVAL      SCEDIF =  IXEDIF
     C                   MOVEL     IXSEIN        SCSEIN
     C                   EVAL      SCSZP  =  IXSZP
     C                   MOVEL     IXREIN        SCREIN
     C                   EVAL      SCRZP  =  IXRZP
     C                   EVAL      SCCNNM =  IXCNNM
     C                   EVAL      SCCNPH =  IXCNPH
     C                   EVAL      SCCNFX =  IXCNFX
     C                   EVAL      SCCNEM =  IXCNEM
       scUsLh = IXusLh;
       //*** Beg Add *** 02/14/22 **********************************************
       scD433  = IXD433;
       scD440  = IXD440;
       scD441  = IXD441;
       Monitor;
       scD441D =%Dec(%Char(%Date(IXd441D:*CYMD):*usa0):8:0);
       On-error;
          scD441D = 0;
       Endmon;
       scD442  = IXD442;
       //*** End Add *** 02/14/22 **********************************************
      *
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
      *
     C                   EVAL      IXCO# = UPCO#
     C                   EVAL      IXJRST =  SCEDST
      *
     C                   Z-ADD     SCCSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IXCSDT
      *
     C                   EVAL      IXMAXT =  SCMAXT
     C                   EVAL      IXRLS  =  SCRLS
     C                   EVAL      IXEDIF =  SCEDIF
     C                   MOVEL     SCSEIN        IXSEIN
     C                   EVAL      IXSZP  =  SCSZP
     C                   MOVEL     SCREIN        IXREIN
     C                   EVAL      IXRZP  =  SCRZP
     C                   EVAL      IXCNNM =  SCCNNM
     C                   EVAL      IXCNPH =  SCCNPH
     C                   EVAL      IXCNFX =  SCCNFX
     C                   EVAL      IXCNEM =  SCCNEM
       IXusLh = scUsLh;
       //*** Beg Add *** 02/14/22 **********************************************
       IXD433  = scD433;
       IXD440  = scD440;
       IXD441  = scD441;
       Monitor;
       IXd441D = %dec(%char(%date(scD441D:*usa):*cymd0):7:0);
       On-error;
          IXd441D = 0;
       Endmon;
       IXD442  = scD442;
       //*** End Add *** 02/14/22 **********************************************
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
      *
     C* Process search key
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
      *
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
      *
     C                   if        @@mode = 'C' or @@mode = 'D'
     C     @@rrn         chain(n)  dbrcdf                             99
     C                   select
     C                   when      ixtrid = 'A49'
     C                   CALL      'W0069R52'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@mode        ##MODE            1
     C                   PARM      @@rrn         ##RRN             9 0
      *
     C                   eval      *INLR  ='1'
     c                   return
      *
     C                   when      ixtrid = '148'
     C                   call      'W0069R51'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      @@mode        ##MODE
     C                   PARM      @@rrn         ##RRN
      *
     C                   eval      *INLR  ='1'
     c                   return
      *
     C                   endsl
     C                   endif
      *
     C     Q1USER        CHAIN     PUP01
     C                   IF        %FOUND
     C                   EVAL      XXCO# = UPCO#
     C                   ENDIF
      *
     C                   EVAL      SCEDST = *BLANKS
     C                   EVAL      SCCSDT = *ZEROS
     C                   EVAL      SCMAXT = *ZEROS
     C                   EVAL      SCRLS  = *BLANKS
     C                   EVAL      SCEDIF = *BLANKS
     C                   EVAL      SCSEIN = *ZEROS
     C                   EVAL      SCSZP  = *BLANKS
     C                   EVAL      SCREIN = *ZEROS
     C                   EVAL      SCRZP  = *BLANKS
     C                   EVAL      SCCNNM = *BLANKS
     C                   EVAL      SCCNPH = *BLANKS
     C                   EVAL      SCCNFX = *BLANKS
     C                   EVAL      SCCNEM = *BLANKS
       scUsLh = *blanks;
      *
       *in25 = *off;
       //*** Beg Add *** 02/14/22 **********************************************
       If IXrls = 'A' and IXtrId = *blank;
          isFldCtrl = *on;
       Else;
          isFldCtrl = *off;
       Endif;
       //*** End Add *** 02/14/22 **********************************************
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
      *
     C                   SELECT
     C                   WHEN      @@MODE = 'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
      *
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
      *
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C                   IF        *IN99 = *OFF
     C                   EXSR      MVSCDB
      *
     C                   MOVE      Q1PGM         IXPGM
     C                   MOVE      Q1USER        IXUSER
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        IXTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IXDT
      *
     C                   UPDATE    DBRCDF
     C                   MOVE      *ON           OUTFLG
      *
E01  C                   ENDIF
      *
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
      *
     C                   clear                   dbrcdf
      *
     C                   EXSR      MVSCDB
      *
     C                   EVAL      IXSTAT = 'A'
      *
     C                   MOVE      Q1PGM         IXPGM
     C                   MOVE      Q1USER        IXUSER
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        IXTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IXDT
     C                   MOVE      Q1USER        IXIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IXIDT
      *
     C                   WRITE     DBRCDF
     C                   FEOD      WMIXP
      *
     C                   MOVE      *ON           OUTFLG
      *
     C                   endsr
    C*================================================================
    C* KJDO - Processing for F10
    C*================================================================
     C     kjdo          begsr
      *
     C                   Z-ADD     SCCSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCCSDT
     C                   ENDIF
      *
     C                   IF        HFLD = 'SCEDST'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM      'JRST'        QQTBID
     C                   PARM      *BLANK        QQKEY
     C                   PARM      ' '           QQRTCD            1
      *
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     QQKEY         SCEDST
     C                   MOVE      *ON           *IN61
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  SEARCH THE EDI FORMATE ID...
      *
     C                   IF        HFLD = 'SCEDIF'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM      'EDIF'        QQTBID
     C                   PARM      *BLANK        QQKEY
     C                   PARM      ' '           QQRTCD            1
      *
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     QQKEY         SCEDIF
     C                   MOVE      *ON           *IN64
     C                   ELSE
     C                   MOVE      *ON           *IN67
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   endsr
    C*================================================================
    C* CHGSTS - Subroutine for changing status
    C*================================================================
     C     chgsts        begsr

     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C                   IF        *IN90 = *OFF
      *
     C                   IF        IXSTAT = 'I'
     C                   EVAL      IXSTAT = 'A'
     C                   ELSE
     C                   EVAL      IXSTAT = 'I'
     C                   ENDIF
      *
     C                   EVAL      IXUSER = q1user
     C                   EVAL      IXPGM  = q1pgm
     C                   TIME                    DSDTTM
      *
     C                   EVAL      SRDTI  = DSDATE
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IXDT
      *
     C                   EVAL      IXTIME = dstime
     C                   UPDATE    DBRCDF
     C                   ENDIF
      *
     C                   endsr
      *
