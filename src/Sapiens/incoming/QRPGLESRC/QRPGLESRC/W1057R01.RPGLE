     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *===============================================================*
      *===============================================================*
     H/TITLE W1057R01 - FIND % ERRORS IN THE OWNER/OFFICER FILE
      *===============================================================*
      * DESCRIPTION:  This program will find and display any %        *
      *               errors for this employer.                       *
      *                                                               *
      * CALLED FROM:                                                  *
      *   W1057R   - Work With Owners/Officers                        *
      *   W1057R02 - Owner/Officer Information                        *
      * CALLS TO:                                                     *
      *   NONE                                                        *
      *                                                               *
      * CREATED: 05/20/97  PROGRAMMER: C:33   PROJECT: 60AND          *
      * REWRITE: 10/21/99  PROGRAMMER: C:113  PROJECT: CM027          *
      *===============================================================*
      *  REVISIONS:                                                   *
      *                                                               *
      *  09/19/12  C:65  Project:  WC2116                             *
      *                  Return error status for regular mode of      *
      *                  program to allow for recognition of error.   *
      *                                                               *
      *                                                               *
      *===============================================================*
      *  INDICATOR USAGE:                                             *
      *                                                               *
      *===============================================================*
      *
      ** Display File
     FW1057F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      ** Location File
     FWMALP     IF   E             DISK
      *
      ** Employer Master File (Logical)
     FWMEML     IF   E           K DISK
      *
      ** Employer Master File (Physical)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
      *
      ** Owner/Officer File
     FWMOOL3    IF   E           K DISK
      *=========================================================================
      *  Array & Table definitions
      *=========================================================================
      *=========================================================================
      *  Data Structures & constants definitions
      *=========================================================================
      *
      ** Midterm Endorsement Information data structure

     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
      *
      ** Program Information data structure
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      ** Effective Dates
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)                               Effective dates
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
      *
      ** Cancelation Dates
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               Cancelation dates
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *=========================================================================
      *  PLIST Definitions
      *=========================================================================
      *
      ** Entry parameters

     C     *ENTRY        PLIST
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@ELFL            1
     C                   PARM                    @@ERR             1
     C                   PARM                    @@EFDT            7 0
     C                   PARM                    @@EXDT            7 0
     C                   PARM                    @@PRCT            5 2
     C                   PARM                    @@RRN             9 0
      *
      ** Date Check/Conversion parameters
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *=========================================================================
      *  KLIST Definitions
      *=========================================================================
      *
      *  Employer Master file
     C     EMKY          KLIST
     C                   KFLD                    ALCO#
     C                   KFLD                    ALFND
     C                   KFLD                    ALEMP#
     C                   KFLD                    ALDIV
      *
      *  Owner/Officer file
     C     OOKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXELID
      *=========================================================================
      *  *LIKE DEFN definitions
      *=========================================================================
     C     *LIKE         DEFINE    OOEXDT        XXCXDT
     C     *LIKE         DEFINE    OOEFDT        XXEFDT
     C     *LIKE         DEFINE    OOEXDT        SVEXDT
     C     *LIKE         DEFINE    OOEFDT        SVEFDT
     C     *LIKE         DEFINE    OOOFPC        SVOFPC
     C     *LIKE         DEFINE    OOOFPC        TTLPCT
     C     *LIKE         DEFINE    OOELID        XXELID
      *=========================================================================
      *  Main
      *=========================================================================
     C                   CLEAR                   XXELID
      *
     C     @@ELFL        IFEQ      *BLANKS
      *
     C     @@RRN2        CHAIN     WMEMP                              99
      *
     C                   MOVE      *OFF          *IN41
      *
     C                   ELSE
      *
     C     @@RRN2        CHAIN     WMALP                              99
      *
     C     @@ELFL        IFEQ      'E'
     C                   MOVEL     ALELID        XXELID
     C                   ENDIF                                                  @@ELFL=E
      *
     C                   MOVE      *ON           *IN41
      *
      ** Get the Employer Master record
     C     EMKY          CHAIN     PEM01                              99
      *
     C                   ENDIF                                                  @@ELFL=*BLANKS
      *
      ** Get the most recent effective date for this employer
     C                   Z-ADD     1             I1                3 0
      *
     C     *ZERO         LOOKUP    EF(I1)                                 61
      *
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61=*ON
      *
     C                   Z-ADD     EF(I1)        XXEFDT
      *
      ** Get midterm/demand information
     C                   MOVE      @@ERR         XXMID             1
     C                   MOVE      *BLANKS       @@ERR
      *
     C     XXMID         IFEQ      'M'
     C     XXMID         OREQ      'D'
      *
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
      *
     C                   Z-ADD     *ZEROS        XXEFDT
     C                   Z-ADD     LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
      *
     C                   ENDIF                                                  XXMID = 'M'
      *
      ** Add all the active Owner/Officer %'s.
     C                   EXSR      CHKPCT
      *
      ** If % is over 100 or less than 99 display warning.
     C                   MOVE      *OFF          ERR
      *
     C     TTLPCT        IFGT      *ZERO
      *
     C     TTLPCT        IFLT      99
     C     TTLPCT        ORGT      100
      *
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
      *
     C                   Z-ADD     TTLPCT        SCTLPC
      *
     C     XXMID         IFEQ      'M'
     C                   EXFMT     WDWMSG2
     C                   ELSE
     C                   EXFMT     WDWMSG
     C                   ENDIF                                                  XXMID = 'M'
      *
     C                   SELECT
      *
     C     *INKC         WHENEQ    *ON
     C                   MOVE      'C'           ERR
      *
     C     *INKL         WHENEQ    *ON
     C                   MOVE      'L'           ERR
      *
     C                   OTHER
     C                   MOVE      *ON           ERR               1
      *
     C                   ENDSL
      *
     C                   ENDIF                                                   99>TTLPCT>100
      *
     C                   ENDIF                                                   TTLPCT>*ZERO
      *
      ** Pass the error code back.
     C     XXMID         IFEQ      'B'
     C*** begin add ***  09/19/12  **********************************************
     C     XXMID         OREQ      *blank
     C*** end add *****  09/19/12  **********************************************
     C                   MOVE      ERR           @@ERR
     C                   ENDIF                                                   XXMID != 'M'
      *
      ** If no error and called on demand, display OK message.
     C     ERR           IFEQ      *OFF
     C     XXMID         ANDEQ     'D'
     C     ERR           OREQ      *OFF
     C     XXMID         ANDEQ     'B'
     C                   EXFMT     WDWMSG1
     C                   ENDIF                                                   ERR=*OFF
      *
     C                   MOVE      *ON           *INLR
      *=========================================================================
      *  CHKPCT - Subroutine to check the %
      *=========================================================================
     C     CHKPCT        BEGSR
      *
      ** Get all the Owner/Officer records and add the % for the active ones
     C     OOKY          CHAIN     POO01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     OOEFDT        IFEQ      *ZERO
     C                   Z-ADD     XXEFDT        OOEFDT
     C                   ENDIF                                                  OOEFDT=*ZERO
      *
     C                   SELECT
     C     XXMID         WHENEQ    'M'
     C     XXMID         OREQ      'D'
      *
     C     OOEFDT        IFLE      XXEFDT
      *
     C     OOEXDT        IFGT      XXEFDT
     C     OOEXDT        OREQ      *ZEROS
     C                   ADD       OOOFPC        TTLPCT
     C                   ENDIF                                                  OOEXDT
      *
     C                   ENDIF                                                  OOEFDT
      *
     C                   OTHER
     C     OOEXDT        IFEQ      *ZEROS
     C                   ADD       OOOFPC        TTLPCT
     C                   ENDIF                                                  OOEXDT = 0
      *
     C                   ENDSL
      *
     C     OOKY          READE     POO01                                  60
      *
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   ENDSR                                                  CHKPCT
