     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H/TITLE W0070R32 - EDI R3 Additional Info
     H Bnddir('WTBNDDIR')
     H* ***************************************************************
     H* DESCRIPTION:  THIS PROGRAM ALLOWS THE USER TO ENTER ADDL      *
     H*               EDI R3 INFO FOR A CLAIM (BASED ON W0070R7)      *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H* CALLS TO:     NONE                                            *
     H* CREATED BY:   191                                             *
     H* PROJECT:      WC1584               10/19/07                   *
     H* ***************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 10/21/10     C:246      SUPPORT 74504 - Fix the pointer issue
     H*                         when calling W0070R1
     H*
     H* 09/28/10     C:180      PROJECT SY190
     H*                         Fix MCH3601 (Pointer not set for
     H*                         location referenced).
     H*
     H* 07/13/09     C:180      PROJECT WC1836 -  Add MC name.
     H*
     H* 04/29/09     C:180      SUPPORT 68059 - EP claims
     H*                         Changes done to W0070F32 only -
     H*                         F19 is now 'Employer Paid Info"
     H*
     A* 04/07/09   C:189   SUPPORT 66028 - Remove F19 functionlity and
     A*                                    adjust keys
     A*
     A* 03/21/09   C:189   SUPPORT 67718 - Need F14 Darts on screen
     A*
     H* 01/06/09     C:53       SUPPORT 66537
     H*                         correct edit for scsame
     h*
     H* 11/03/08     C:180      SUPPORT 65671
     H*                         Add prompt for Reduced Ben amt type.
     h*
     H* 10/30/08     C:180      SUPPORT 65671,65757 - Add EDI R3 fields:
     H*                         Employee ID other than SSN,
     H*                         Employee id type qualifier, and
     H*                         Reduced benefit amount type
     H*                                                               *
     H* ***************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  *IN09  -  INDICATES MODE (MAINT/INQ)                         *
     H*  *IN61-69  Position Cursor
     H*  *IN71-89  ERROR INDICATORS                                   *
     H* ***************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0070f32  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmcmp     uf a e             disk
     F                                     rename(pcm01:dbrcdf)
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMCML1    UF   E           K DISK
      *  GENERIC TABLE FILE
     FWTGTL     IF   E           K DISK
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'EDI Rel 3 Info'
     D comapi          c                   'WA001R'
     D #ofscr          c                   2
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
     D PRDS            S             40    DIM(2) CTDATA
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D*
     D                 DS
     D  XXSUSN                 1    150
     D  SCSUS1                 1     50
     D  SCSUS2                51    100
     D  SCSUS3               101    150
     D*
     D                 DS
     D  XXDNYN                 1    150
     D  SCDNN1                 1     50
     D  SCDNN2                51    100
     D  SCDNN3               101    150
      ******* BEGIN ADD ****** 03/21/09 *******************************
     D                 DS                  INZ
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
      ******* END   ADD ****** 03/21/09 *******************************
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
     C                   parm                    @@cont            1
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR PAYEE DEMOGRAPHICS FILE
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    CMACDT        XXDT
     C     *LIKE         DEFINE    CMDNLT        XXDNLT
     C     *LIKE         DEFINE    CMACDT        SVDATE                         CURRENT DATE
     C     *LIKE         DEFINE    CMACTM        SVTIME                         CURRENT TIME
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
      ******* BEGIN ADD ****** 03/21/09 *******************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      ******* END   ADD ****** 03/21/09 *******************************
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
     C                   eval      *in90 = *ON
     C                   movea     '000000000000'*in(71)
     C                   setoff                                       8251
     c***** begin add ****** 07/13/09 **************************************
     C                   setoff                                       47
     c***** end  add ****** 07/13/09 **************************************
     C*
      *  If another user has updated this claim, send an error message.
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N99SVDATE        IFNE      CMDT
     C     SVTIME        ORNE      CMTIME
      *
     C                   CALL      'WA017R'
     C                   PARM                    CMUSER
     C                   PARM                    ##RTCD
      *
     C                   LEAVESR
     C                   ENDIF
      *
     C* Date notified of LT
     C**** Edit for valid Date
     C     SCDNLT        IFNE      0
     C                   Z-ADD     SCDNLT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCDNLT
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN71
     C                   LEAVESR
     C                   ENDIF                                                  SRERR='1'
      *
      * Date notified of LT should not be less than accident date
     C                   Z-ADD     SCDNLT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXDNLT
      * Accident date loaded from CMACDT
     C     XXDNLT        IFLT      XXACDT
     C                   MOVE      *ON           *IN82
     C                   LEAVESR
     C                   ENDIF                                                  XXDNLT<XXACDT
     C                   ENDIF                                                  SCDNLT<>0
     C*
     C* Date employer notified
     C     SCDODD        IFNE      0
     C                   MOVE      SCDODD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDODD
     C                   IF        SRERR = '1'
     C                   SETON                                        72
     C                   LEAVESR
     C                   ENDIF
     C                   ENDIF                                                  SCDODD<>0
     C*
     C* Initial Date of lost time
     C     SCIDLT        IFNE      0
     C                   MOVE      SCIDLT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCIDLT
     C                   IF        SRERR = '1'
     C                   SETON                                        73
     C                   LEAVESR
     C                   ENDIF
     C                   ENDIF                                                  SCIDLT<>0
     C*
     C* Current Date last day worked
     C     SCCDLW        IFNE      0
     C                   MOVE      SCCDLW        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDLW
     C                   IF        SRERR = '1'
     C                   SETON                                        74
     C                   LEAVESR
     C                   ENDIF                                                  SRERR='1'
     C                   ENDIF                                                  SCCDLW<>0
     C*
     C* Current Date disability began
     C     SCCDDB        IFNE      0
     C                   MOVE      SCCDDB        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDDB
     C                   IF        SRERR = '1'
     C                   SETON                                        52
     C                   LEAVESR
     C                   ENDIF                                                  SRERR='1'
     C                   ENDIF                                                  SCCDLW<>0
     C*
     C* Release SS# Authorization must be Y or N
     C     SCSSRL        IFNE      'Y'
     C     SCSSRL        ANDNE     'N'
     C     SCSSRL        ANDNE     ' '
     C                   SETON                                        75
     C                   LEAVESR
     C                   ENDIF
      *
      *  Release MR Authorisation
     C     SCAURL        IFNE      *BLANK
     C     SCAURL        ANDNE     'Y'
     C     SCAURL        ANDNE     'N'
     C                   SETON                                        76
     C                   LEAVESR
     C                   ENDIF                                                  SCAURL<>' '
      *
      *  Death result of injury
     C     SCIDTH        IFNE      *BLANK
     C     SCIDTH        ANDNE     'Y'
     C     SCIDTH        ANDNE     'N'
     C     SCIDTH        ANDNE     'U'
     C                   SETON                                        77
     C                   LEAVESR
     C                   ENDIF                                                  SCIDTH<>' '
     C*
     C* Tax filing Status
     C                   IF        SCETFS <> ' '
     C                   MOVEL(P)  'ETFS'        XXTBID
     C                   MOVEL(P)  SCETFS        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        78
     C                   LEAVESR
     C                   ENDIF                                                  *IN99 = '1'
     C                   ENDIF                                                  SCETFS
     C*
     C* Non-Consecutive Period code
     C                   IF        SCNCPC <> ' '
     C                   MOVEL(P)  'NCPD'        XXTBID
     C                   MOVEL(P)  SCNCPC        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        79
     C                   LEAVESR
     C                   ENDIF                                                  *IN99 = '1'
     C                   ENDIF                                                  SCNCPC
     C* Return to work with same employer
     C                   IF        SCSAME <> 'Y' AND SCSAME <> ' '
     C*** BEG ADD *** 01/06/09 ************************************
     C                                AND SCSAME <> 'N'
     C*** END ADD *** 01/06/09 ************************************
     C                   SETON                                        80
     C                   LEAVESR
     C                   ENDIF                                                  SCSAME
     C*
     C* MCO FEIN
     C                   IF        SCMCOF <> 0
     C                   EVAL      XXFEIN = SCMCOF
     C                   EVAL      XXMOD = 0
     C     PDKY          CHAIN     PPD01                              60
     C                   IF        NOT %FOUND
      *********** BEGIN CHANGE ****** 07/13/09 ****************************
     C***                SETON                                        81
     C***                LEAVESR
     C                   ELSE
     C                   MOVEL(P)  PDNAME        SCMCNM
      *********** END CHANGE ******** 07/13/09 ****************************
     C                   ENDIF                                                  %FOUND
      *********** beg add    ******** 07/13/09 ****************************
     C                   if        scmcnm = *blanks
     C                   SETON                                        47
     c                   LEAVESR
     c                   endif
     C                   ELSE
     C                   if        scmcnm > *blanks
     C                   SETON                                        81
     c                   LEAVESR
     c                   endif
      *********** End add    ******** 07/13/09 ****************************
     C                   ENDIF                                                  SCMCOF<>0
     C*
     C* Permanent impairment min. payment
     C                   IF        SCPIMP <> 'Y' AND SCPIMP <> ' '
     C                   SETON                                        51
     C                   LEAVESR
     C                   ENDIF                                                  SCPIMP
     C*
      ******** BEG ADD ****** 10/30/08 *************************************
     C* Employee id type qualifier
     C                   IF        (SCEITQ > *BLANK) or
     C                             (SCEITQ = *BLANK and SCEID > *BLANKS)
     C                   MOVEL(P)  'EIDQ'        XXTBID
     C                   MOVEL(P)  SCEITQ        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        43
     C                   LEAVESR
     C                   ENDIF                                                  *IN99 = '1'
     C                   IF        SCEID = *BLANKS
     C                   SETON                                        42
     C                   LEAVESR
     C                   ENDIF
     C                   ENDIF
      *
      ******** END ADD ****** 10/30/08 *************************************
      *
      ******** BEG ADD ****** 11/03/08 *************************************
     C* Reduced Benefit amount type
     C                   IF        (SCRBAC > *BLANK)
     C                   MOVEL(P)  'RBAC'        XXTBID
     C                   MOVEL(P)  SCRBAC        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        46
     C                   LEAVESR
     C                   ENDIF                                                  *IN99 = '1'
     C                   ENDIF
      ******** END ADD ****** 11/03/08 *************************************
     C                   EVAL      *in90 = *OFF
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   eval      *in90 = *ON
     C                   movea     '0000000'     *in(83)
      *  If another user has updated this claim, send an error message.
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N99SVDATE        IFNE      CMDT
     C     SVTIME        ORNE      CMTIME
      *
     C                   CALL      'WA017R'
     C                   PARM                    CMUSER
     C                   PARM                    ##RTCD
      *
     C                   LEAVESR
     C                   ENDIF                                                  SVDATE <> CMDT
      *
     C* Full denial effective date
     C     SCDYEF        IFNE      0
     C                   MOVE      SCDYEF        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDYEF
     C                   IF        SRERR = '1'
     C                   SETON                                        83
     C                   LEAVESR
     C                   ENDIF                                                  SRERR='1'
     C                   ENDIF                                                  SCDYEF <> 0
      *
     C* Full denial recission date
     C     SCDRES        IFNE      0
     C                   MOVE      SCDRES        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDRES
     C                   IF        SRERR = '1'
     C                   SETON                                        84
     C                   LEAVESR
     C                   ENDIF                                                  SRERR='1'
     C                   ENDIF                                                  SCDYEF <> 0
     C*
     C* DENIAL CODE 1
     C*
     C     SCDNY1        IFNE      *BLANKS
     C                   MOVEL(P)  'DENY'        XXTBID
     C                   MOVEL(P)  SCDNY1        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        85
     C                   LEAVESR
     C                   ENDIF                                                  *IN99 = '1'
     C                   ENDIF                                                  SCDNY1 <> ' '
     C*
     C* DENIAL CODE 2
     C*
     C     SCDNY2        IFNE      *BLANKS
     C                   MOVEL(P)  'DENY'        XXTBID
     C                   MOVEL(P)  SCDNY2        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        86
     C                   LEAVESR
     C                   ENDIF                                                  *IN99 = '1'
     C                   ENDIF                                                  SCDNY2 <> ' '
     C*
     C* DENIAL CODE 3
     C*
     C     SCDNY3        IFNE      *BLANKS
     C                   MOVEL(P)  'DENY'        XXTBID
     C                   MOVEL(P)  SCDNY3        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        87
     C                   LEAVESR
     C                   ENDIF                                                  *IN99 = '1'
     C                   ENDIF                                                  SCDNY3 <> ' '
     C*
     C* DENIAL CODE 4
     C*
     C     SCDNY4        IFNE      *BLANKS
     C                   MOVEL(P)  'DENY'        XXTBID
     C                   MOVEL(P)  SCDNY4        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        8586
     C                   LEAVESR
     C                   ENDIF                                                  *IN99 = '1'
     C                   ENDIF                                                  SCDNY4 <> ' '
     C*
     C* DENIAL CODE 5
     C*
     C     SCDNY5        IFNE      *BLANKS
     C                   MOVEL(P)  'DENY'        XXTBID
     C                   MOVEL(P)  SCDNY5        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        8587
     C                   LEAVESR
     C                   ENDIF                                                  *IN99 = '1'
     C                   ENDIF                                                  SCDNY5 <> ' '
     C*
     C* Partial denial reason code
     C*
     C     SCPDNY        IFNE      *BLANKS
     C                   MOVEL(P)  'PDCD'        XXTBID
     C                   MOVEL(P)  SCPDNY        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        88
     C                   LEAVESR
     C                   ENDIF                                                  *IN99 = '1'
     C                   ENDIF                                                  SCDNY5 <> ' '
     C*
     C* Suspension Effective date
     C     SCSUEF        IFNE      0
     C                   MOVE      SCSUEF        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSUEF
     C                   IF        SRERR = '1'
     C                   SETON                                        89
     C                   LEAVESR
     C                   ENDIF                                                  SRERR='1'
     C                   ENDIF                                                  SCSUEF<>0
      *
     C                   EVAL      *in90 = *OFF
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
     C******* BEGIN ADD ****** 03/21/09 *******************************
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14 = DARTS
     C*===============================================================
     C*
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      CMFND         QCFND
     C                   MOVE      CMFYR         QCFYR
     C                   MOVE      CMCASE        QCCASE
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0070R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C*
     C******* END  ADD ****** 03/21/09 *******************************
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   eval      scjuro = cmjuro
     C                   z-add     cmdnlt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         scdnlt
     C                   z-add     cmdodd        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         scdodd
     C                   z-add     cmidlt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         scidlt
     C                   eval      sccdlw = cmcdlw
     C                   z-add     cmcdlw        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         sccdlw
     C                   eval      sccddb = cmcddb
     C                   z-add     cmcddb        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         sccddb
     C                   eval      scaurl = cmaurl
     C                   eval      scidth = cmidth
     C                   eval      scssrl = cmssrl
     C                   eval      scdfbe = cmdfbe
     C                   eval      scenex = cmenex
     C                   eval      scncpc = cmncpc
     C                   eval      scsame = cmsame
     C                   eval      scpimp = cmpimp
     C                   eval      xxsusn = cmsusn
     C                   eval      xxdnyn = cmdnyn
     C                   move      cmmcof        scmcof
      **** Begin add ***** 07/13/09 **************************************
     C                   MOVE      cmmcnm        scmcnm
      **** End add ******* 07/13/09 **************************************
     C                   eval      sciif  = cmiif
     C                   eval      scetfs = cmetfs
     C                   Z-ADD     CMTIME        SVTIME
     C                   Z-ADD     CMDT          SVDATE

     C                   z-add     cmdyef        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         scdyef
     C                   z-add     cmdres        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         scdres
     C                   eval      scdny1 = cmdny1
     C                   eval      scdny2 = cmdny2
     C                   eval      scdny3 = cmdny3
     C                   eval      scdny4 = cmdny4
     C                   eval      scdny5 = cmdny5
     C                   eval      scpdny = cmpdny
     C                   z-add     cmsuef        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         scsuef

      ******** BEG ADD ****** 10/30/08 *************************************
     C                   eval      sceid = cmeid
     C                   eval      sceitq = cmeitq
     C                   eval      scrbac = cmrbac
      ******** END ADD ****** 10/30/08 *************************************
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   eval      cmjuro = scjuro
     C                   z-add     scdnlt        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         cmdnlt
     C                   z-add     scdodd        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         cmdodd
     C                   z-add     scidlt        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         cmidlt
     C                   z-add     sccdlw        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         cmcdlw
     C                   z-add     sccddb        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         cmcddb
     C                   eval      cmaurl = scaurl
     C                   eval      cmssrl = scssrl
     C                   eval      cmidth = scidth
     C                   eval      cmdfbe = scdfbe
     C                   eval      cmenex = scenex
     C                   eval      cmncpc = scncpc
     C                   eval      cmsame = scsame
     C                   eval      cmpimp = scpimp
     C                   move      scmcof        cmmcof
      ***** Beg add ****  07/13/09 ****************************************
     C                   move      scmcnm        cmmcnm
      ***** End add ****  07/13/09 ****************************************
     C                   eval      cmiif  = sciif
     C                   eval      cmetfs = scetfs
     c
     C                   z-add     scdyef        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         cmdyef
     C                   z-add     scdres        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         cmdres
     C                   eval      cmdny1 = scdny1
     C                   eval      cmdny2 = scdny2
     C                   eval      cmdny3 = scdny3
     C                   eval      cmdny4 = scdny4
     C                   eval      cmdny5 = scdny5
     C                   eval      cmpdny = scpdny
     C                   z-add     scsuef        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         cmsuef
     C                   eval      cmsusn = xxsusn
     C                   eval      cmdnyn = xxdnyn
     c                   time                    dsdttm
     C                   z-add     dsdate        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         cmdt
     c                   z-add     dstime        cmtime
     c                   movel     q1user        cmuser
     c                   movel     q1pgm         cmpgm

      ******** BEG ADD ****** 10/30/08 *************************************
     c                   eval      cmeid = sceid
     c                   eval      cmeitq = sceitq
     c                   eval      cmrbac = scrbac
      ******** END ADD ****** 10/30/08 *************************************
     c
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
     C     *INKJ         CASEQ     *ON           KJDO                     90
      *** BEGIN DEL *** 04/07/09 ***********************************************
     C***********     *INKT         CASEQ     *ON           KTDO
      *** END   DEL *** 04/07/09 ***********************************************
      *** BEGIN ADD *** 04/29/09 ***********************************************
     C     *INKT         CASEQ     *ON           KTDO
      *** END ADD ***** 04/29/09 ***********************************************
     C     *INKU         CASEQ     *ON           KUDO
     C     *INKV         CASEQ     *ON           KVDO
     C     *INKW         CASEQ     *ON           KWDO
      *** BEGIN ADD *** 03/21/09 ***********************************************
     C     *INKN         CASEQ     *ON           KNDO                     90
      *** END   ADD *** 03/21/09 ***********************************************
     C                   ENDCS
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
      *  CHECK THE MODE.  A=ADD, C=CHANGE, D=DISPLAY
      *
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
      *
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'C'           @@MODE
     C                   MOVEL     MODEA         SCMODE
     C                   END
      *
     C     *IN09         IFEQ      *ON
     C                   MOVEL(P)  MODED         SCMODE
     C                   END
      *
     C                   TIME                    DSDTTM
     C*
     C*  RETRIEVE THE USER PARAMETER, COMPANY AND FUND RECORDS
     C*
     C                   Z-ADD     0             XXCO#
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     UPCO#         XXCO#
     C                   ENDIF                                                  *IN60 = 0
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   MOVE      CMFND         XXFND
     C                   MOVE      CMFYR         XXFYR
     C                   MOVE      CMCASE        XXCASE
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     XXFND         SCFND
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C*  RETRIEVE THE CLAIMANT INFORMATION
     C*
      *  FYR-CASE
     C                   MOVE(P)   XXFYR         XAFYR             3
     C                   MOVE(P)   XXCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCL
     C     SCYRCL        CAT       XACASE:0      SCYRCL
     C*
      *  CLAIMANT SS#
     C     CMSS#         IFNE      *BLANK
      *****  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCSS#
     C*****SCSS#         CAT       SS2:0         SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       SS3:0         SCSS#
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-Free
      *****  End Chg  ***  06/09/20  **********************
     C                   ENDIF                                                  CMSS# <> *BLANKS
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    CMEMP#
     C                   PARM                    CMDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
     C                   ENDIF                                                  *IN60='0'
     C*
     C                   MOVEL     CMACDT        XXACDT
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL     CMACL#        SCSCL#
     C*
     C*  RETRIEVE THE CLAIMANT NAME
     C*
     C     CDKY          CHAIN     PCD01                              60
     C                   MOVE      *BLANKS       SCCNAM
     C                   MOVEL     CDLSN         SCCNAM
     C     CDLSN         IFNE      *BLANKS
     C     SCCNAM        CAT       ',':0         SCCNAM
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C                   ELSE
     C                   MOVEL     CDFSN         SCCNAM
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C                   ENDIF                                                  CDLSN <> ' '
     C* OTHER SCREEN FIELDS
     C                   EVAL      SCSAME = CMSAME
     C                   EVAL      SCPIMP = CMPIMP
     C* Change date to CYMD format before writing to file
     C                   MOVE      CMIDLT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCIDLT
     C*
     C                   MOVE      CMDODD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDODD
     C*
     C                   MOVE      CMCDLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDLW
     C                   MOVE      CMCDDB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDDB
     C*
     C                   EVAL      SCNCPC = CMNCPC
     C                   EVAL      SCIIF  = CMIIF
     C                   EVAL      SCETFS = CMETFS
     C                   EVAL      SCENEX = CMENEX
     C                   EVAL      SCDFBE = CMDFBE
     C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      wmcmp
     C                   endsr
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=TABLE SEARCH
     C*
     C     kjdo          begsr
     C                   MOVEL     SCPRDS        ##PRDS           40
     C                   MOVE      *BLANK        ##RTCD            1
     C                   MOVE      *BLANK        ##CALL            1
     C*
     C                   SELECT
     C*   Tax Filling Status Search
     C     HFLD          WHENEQ    'SCETFS'
     C                   CALL      'W2493R'
     C                   PARM                    ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM      'ETFS'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       ##RTCD
     C*
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCETFS
     C                   MOVE      *ON           *IN64                          POS CUSR
     C                   ELSE
     C                   MOVE      *ON           *IN63
     C                   ENDIF
      *
     C*   Non-Consecutive period search
     C     HFLD          WHENEQ    'SCNCPC'
     C                   CALL      'W2493R'
     C                   PARM                    ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM      'NCPD'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       ##RTCD
     C*
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCNCPC
     C                   MOVE      *ON           *IN65                          POS CUSR
     C                   ELSE
     C                   MOVE      *ON           *IN64
     C                   ENDIF
      *
     C*   MCO FEIN Search
     C     HFLD          WHENEQ    'SCMCOF'
     C                   MOVEL     SCMCOF        ##FEIN
     C                   MOVE      XXMOD         ##MOD                          *ZEROS MOVED TO XXMO
     C                   MOVE      '2'           ##CALL
     C                   CALL      'W0030R'
     C                   PARM                    ##CALL
     C                   PARM                    ##FEIN            9
     C                   PARM                    ##MOD             3
     C                   PARM                    ##RTCD
      ****** BEGIN ADD ***** 07/13/09 *******************************
     C                   PARM                    ##NAME           40
      ****** END ADD ******* 07/13/09 *******************************
      *
     C     ##RTCD        IFEQ      '*'
     C                   MOVE      ##FEIN        SCMCOF
      ****** BEGIN ADD ***** 07/13/09 *******************************
     C                   MOVE      ##NAME        SCMCNM
      ****** END ADD ******* 07/13/09 *******************************
     C                   SETON                                        62
     C                   ELSE
     C                   SETON                                        61
     C                   ENDIF
      *
     C     HFLD          WHENEQ    'SCDNY1'
     C                   CALL      'W2493R'
     C                   PARM                    ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM      'DENY'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       ##RTCD
     C*
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCDNY1
     C                   MOVE      *ON           *IN64                          POS CUSR
     C                   ELSE
     C                   MOVE      *ON           *IN63
     C                   ENDIF
      *
     C     HFLD          WHENEQ    'SCDNY2'
     C                   CALL      'W2493R'
     C                   PARM                    ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM      'DENY'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       ##RTCD
     C*
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCDNY2
     C                   MOVE      *ON           *IN65                          POS CUSR
     C                   ELSE
     C                   MOVE      *ON           *IN64
     C                   ENDIF
      *
     C     HFLD          WHENEQ    'SCDNY3'
     C                   CALL      'W2493R'
     C                   PARM                    ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM      'DENY'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       ##RTCD
     C*
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCDNY3
     C                   MOVE      *ON           *IN66                          POS CUSR
     C                   ELSE
     C                   MOVE      *ON           *IN65
     C                   ENDIF
      *
     C     HFLD          WHENEQ    'SCDNY4'
     C                   CALL      'W2493R'
     C                   PARM                    ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM      'DENY'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       ##RTCD
     C*
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCDNY4
     C                   MOVE      *ON           *IN67                          POS CUSR
     C                   ELSE
     C                   MOVE      *ON           *IN66
     C                   ENDIF
      *
     C     HFLD          WHENEQ    'SCDNY5'
     C                   CALL      'W2493R'
     C                   PARM                    ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM      'DENY'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       ##RTCD
     C*
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCDNY5
     C                   MOVE      *ON           *IN68                          POS CUSR
     C                   ELSE
     C                   MOVE      *ON           *IN67
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCPDNY'
     C                   CALL      'W2493R'
     C                   PARM                    ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM      'PDCD'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       ##RTCD
     C*
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCPDNY
     C                   MOVE      *ON           *IN69                          POS CUSR
     C                   ELSE
     C                   MOVE      *ON           *IN68
     C                   ENDIF
      *
      ******** BEG ADD ****** 10/30/08 *************************************
     C     HFLD          WHENEQ    'SCEITQ'
     C                   CALL      'W2493R'
     C                   PARM                    ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM      'EIDQ'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       ##RTCD
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCEITQ
     C                   MOVE      *ON           *IN44
     C                   ENDIF
      ******** END ADD ****** 10/30/08 *************************************
      ******** BEG ADD ****** 11/03/08 *************************************
     C     HFLD          WHENEQ    'SCRBAC'
     C                   CALL      'W2493R'
     C                   PARM                    ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM      'RBAC'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       ##RTCD
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCRBAC
     C                   MOVE      *ON           *IN45
     C                   ENDIF
      ******** END ADD ****** 11/03/08 *************************************
     C                   OTHER
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR                                                  KJDO
      ******** BEGIN DEL ****** 04/07/09 *************************************
     C*===============================================================
     C* KTDO - SUBROUTINE FOR F19
     C*===============================================================
     C*
     C**** BEGIN CHANGE ****** 04/29/09 ********************************
     C*********F19=Weekly Amounts
     C* F19=Employer-Paid Info
     C**** END CHANGE ******** 04/29/09 ********************************
     C*
     C     ktdo          begsr
     C*
     C                   MOVE      XXFND         #@FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      XXCASE        ##CASE
     C*
     C                   CALL      'W0070R3'
     C                   PARM      *BLANKS       ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM                    #@FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      *BLANKS       ##RTCD            1
      ****** BEGIN ADD  ****  09/28/10  ************************************
     C                   PARM                    @@MODE
      ****** END  ADD ******  09/28/10  ************************************
     C*
     C                   IF        ##RTCD='A'
     C                   EVAL      *INKC='1'
     C                   ENDIF                                                  ##RTCD='A'
     C                   IF        ##RTCD='B'
     C                   EVAL      *INKL='1'
     C                   ENDIF                                                  ##RTCD='B'
     C*
     C                   ENDSR                                                  KTDO
     C*===============================================================
     C* KUDO - SUBROUTINE FOR F20
     C*===============================================================
     C*
     C*  F20=Reduced Earnings
     C*
     C     kudo          begsr
     C*
     C                   MOVE      XXFND         #@FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      XXCASE        ##CASE
     C*
     C                   CALL      'W0070R1'
     C                   PARM      *BLANKS       ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM                    #@FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      *BLANKS       ##RTCD            1
      ******** BEG ADD ****** 10/21/10 *************************************
     C                   PARM                    @@MODE
      ******** END ADD ****** 10/21/10 *************************************
     C*
     C                   IF        ##RTCD='A'
     C                   EVAL      *INKC='1'
     C                   ENDIF                                                  ##RTCD='A'
     C                   IF        ##RTCD='B'
     C                   EVAL      *INKL='1'
     C                   ENDIF                                                  ##RTCD='B'
     C*
     C                   ENDSR                                                  KTDO
     C*===============================================================
     C* KVDO - SUBROUTINE FOR F21
     C*===============================================================
     C*
     C*  F21=Concurrent Employer
     C*
     C     kvdo          begsr
     C*
     C                   MOVE      XXFND         #@FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      XXCASE        ##CASE
     C*
     C                   CALL      'W0070R1A'
     C                   PARM      *BLANKS       ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM                    #@FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      *BLANKS       ##RTCD            1
      ******** BEG ADD ****** 10/21/10 *************************************
     C                   PARM                    @@MODE
      ******** END ADD ****** 10/21/10 *************************************
     C*
     C                   IF        ##RTCD='A'
     C                   EVAL      *INKC='1'
     C                   ENDIF                                                  ##RTCD='A'
     C                   IF        ##RTCD='B'
     C                   EVAL      *INKL='1'
     C                   ENDIF                                                  ##RTCD='B'
     C*
     C                   ENDSR                                                  KTDO
     C*===============================================================
     C* KWDO - SUBROUTINE FOR F22
     C*===============================================================
     C*
     C*  F22=Witness Information
     C*
     C     kwdo          begsr
     C*
     C                   MOVE      XXFND         #@FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      XXCASE        ##CASE
     C*
     C                   CALL      'W0070RA'
     C                   PARM      *BLANKS       ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM                    #@FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C                   IF        ##RTCD='A'
     C                   EVAL      *INKC='1'
     C                   ENDIF                                                  ##RTCD='A'
     C                   IF        ##RTCD='B'
     C                   EVAL      *INKL='1'
     C                   ENDIF                                                  ##RTCD='B'
     C*
     C                   ENDSR                                                  KTDO
** PGM DESC
  EDI R3 ADDITIONAL INFO MAINTENANCE
    EDI R3 ADDITIONAL INFO ENQUIRY
