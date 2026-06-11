     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE W0069R59 : EDI Claim Control Maintenance - Auto Submit
     H*****************************************************************
     H*****************************************************************
     H*  DESCRIPTION: EDI Claim Control Maintenance - Auto Submit.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0069R59 - EDI Claim Control Maintenance.- Auto Submit.     *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 03/24/10  PROGRAMMER: C:232 PROJECT: 139WNN         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  08/02/10  C:220    PROJECT: 139WNN / Priss# 41
     H*                              State file ID should be unique for
     H*                              each EDI format identifer.
     H*
     H*****************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0069f59  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     FWMI1P     uf a e             disk
     F                                     rename(pi101:dbrcdf)
      *
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMI1L     UF A E           K DISK
     FSTCTL1    IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
      *
     F*  GENERIC TABLE FILE
     FWTGTL     IF   E           K DISK
     F*  STATE MASTER
     FSTSTL3    IF   E           K DISK
      *
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'EDI Claim Control Maint.-
     d                                      - Auto Submit'
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
     c                   parm                    @@jrst
     c                   parm                    @@tsid
     c                   parm                    @@edif
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
     C     I1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    @@jrst
     C                   KFLD                    @@tsid
     C                   KFLD                    @@edif
     C**** Beg Add ****** 8/02/10 ************************************
     C*
     C     I1KEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    @@jrst
     C**** End Add ****** 8/02/10 ************************************

     C*  DARTS TABLE CODE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD

     C*  KEY LIST FOR THE SYSTEM-WIDE STATE CODE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY



    C*================================================================
    C*  *LIKE DEFN statements
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    CONAME        XXCONM
     C     *LIKE         DEFINE    CTRMCD        XXRMCD
     C     *LIKE         DEFINE    SCJRST        XXJRST
     C     *LIKE         DEFINE    I1TSID        XXTSID
     C     *LIKE         DEFINE    I1EDIF        XXEDIF
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     C     *LIKE         DEFINE    STKEY         XXKEY
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTTBID        QQTBID
     C     *LIKE         DEFINE    GTKEY         QQKEY
     C     *LIKE         DEFINE    Q1USER        XXUSER
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      *
     C     *LIKE         DEFINE    I1JRST        @@jrst
     C     *LIKE         DEFINE    I1TSID        @@tsid
     C     *LIKE         DEFINE    I1EDIF        @@edif
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
     C                   MOVEA     '000'         *IN(51)
     C                   MOVEA     '000'         *IN(61)
      *
      * Valid value of Automate submission is 'Y' or blanks
      *
     C                   IF        *IN90 <> '1'  AND SCAUTO <> *BLANKS
     C                   IF        SCAUTO <> 'Y'
     C                   SETON                                        516190
     C                   ENDIF
     C                   ENDIF
      *
      * Valid value of Print Hardcopy in Extract is 'Y' or blanks
      *
     C                   IF        *IN90 <> '1' AND  SCHCPY <> *BLANKS
     C                   IF        SCHCPY <> 'Y'
     C                   SETON                                        526290
     C                   ENDIF
     C                   ENDIF
      *
      * Valid value of Test Transmission is 'Y' or blanks
      *
     C                   IF        *IN90 <> '1' AND SCTSTF <> *BLANKS
     C                   IF        SCTSTF <> 'Y'
     C                   seton                                        536390
     C                   ENDIF
     C                   ENDIF
     C**** Beg Add ******08/02/10 ************************************
      * State file ID should be unique.
      *
     C                   IF        *IN90 <> '1' AND SCSFID <> *BLANKS
     C*
     C     I1KEY2        chain(n)  PI101                              60
     C*
     C                   dow       *in60 = *off
     C*
     C                   if        SCSFID = I1SFID
     C                             and SCSFID <> X1SFID
     C*
     C                   seton                                          7490
     C                   leave
     C                   endif                                                   State ID
     C*
     C     I1KEY2        reade(n)  PI101                                  60
     C                   enddo
     C*
     C                   ENDIF
     C**** End Add ****** 8/02/10 ************************************
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
     C
      *
     C**** Beg Chg ****** 8/02/10 ************************************
     C**** I1KEY         CHAIN     PI101                              60
     C     I1KEY         CHAIN(N)  PI101                              60
     C**** End Chg ****** 8/02/10 ************************************
      *
     C                   IF        *IN60 = '0'
      *
     C                   MOVEL     @@JRST        SCJRST
     C                   MOVEL     @@TSID        SCTSID
     C                   MOVEL     @@EDIF        SCEDIF
     C                   MOVEL     I1AUTO        SCAUTO
     C                   MOVEL     I1SFID        SCSFID
     C**** Beg Add ****** 8/02/10 ************************************
     C                   MOVEL     I1SFID        X1SFID            3
     C**** End Add ****** 8/02/10 ************************************
     C                   MOVEL     I1HCPY        SCHCPY
     C                   MOVEL     I1TSTF        SCTSTF
      *
     C                   ELSE
      *
     C                   MOVEL     @@JRST        SCJRST
     C                   MOVEL     @@TSID        SCTSID
     C                   MOVEL     @@EDIF        SCEDIF
     C                   MOVEL     *blanks       SCAUTO
     C                   MOVEL     *blanks       SCSFID
     C                   MOVEL     *blanks       SCHCPY
     C                   MOVEL     *blanks       SCTSTF
      *
     C                   ENDIF
      *
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   MOVEL     XXCO#         I1CO#
     C                   MOVEL     SCJRST        I1JRST
     C                   MOVEL     SCTSID        I1TSID
     C                   MOVEL     SCEDIF        I1EDIF
     C                   MOVEL     SCAUTO        I1AUTO
     C                   MOVEL     SCSFID        I1SFID
     C                   MOVEL     SCHCPY        I1HCPY
     C                   MOVEL     SCTSTF        I1TSTF
      *
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
     C**** Beg Add ****** 8/02/10 ************************************
     C                   seton                                        lr
     C**** End Add ****** 8/02/10 ************************************
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
      *
     C     Q1USER        CHAIN     PUP01
     C                   IF        %FOUND
     C                   EVAL      XXCO# = UPCO#
     C                   ENDIF
     C                   MOVEL     @@JRST        SCJRST
     C                   MOVEL     @@TSID        SCTSID
     C                   MOVEL     @@EDIF        SCEDIF
      *
     C                   MOVEL     *BLANKS       SCSFID
     C                   MOVEL     *BLANKS       SCHCPY
     C                   MOVEL     *BLANKS       SCTSTF
     C                   MOVEL     *BLANKS       SCAUTO
      *
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
      *
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
      *
     C     I1KEY         CHAIN     PI101                              99
     C*    @@RRN         CHAIN     DBRCDF                             99
      *
     C                   IF        *IN99 = *OFF
      *
     C                   EXSR      MVSCDB
      *
     C                   EVAL      I1USER = Q1USER
     C                   EVAL      I1PGM  = Q1PGM
     C                   TIME                    DSDTTM
      *
     C                   EVAL      SRDTI  = DSDATE
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         I1DT
     C                   EVAL      I1TM   = DSTIME
      *
     C                   UPDATE    PI101
     C                   MOVE      *ON           OUTFLG
     C                   ELSE
     C                   EXSR      WRITER
     C                   ENDIF
      *
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
      *
     C                   CLEAR                   DBRCDF
      *
     C                   EXSR      MVSCDB
      *
     C                   EVAL      I1USER = Q1USER
     C                   EVAL      I1IUSR = Q1USER
     C                   EVAL      I1PGM  = Q1PGM
     C                   EVAL      I1IPGM = Q1PGM
     C                   TIME                    DSDTTM
      *
     C                   EVAL      SRDTI  = DSDATE
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         I1DT
     C                   MOVEL     SRDTO         I1IDT
     C                   eval      I1TM   = DSTIME
     C                   eval      I1ITM  = DSTIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      WMI1P
     C                   MOVE      *ON           OUTFLG
      *
     C                   endsr
    C*================================================================
    C* KJDO - Processing for F10
    C*================================================================
     C     kjdo          begsr
      *
     C                   endsr
