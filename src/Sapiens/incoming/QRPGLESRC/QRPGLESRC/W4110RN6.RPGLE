     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
     H/TITLE W4110RN6 - FORM 63
      *===============================================================*
      *  DESCRIPTION:  This program will allow a user enter the       *
      *                additional information required for Form 26A.  *
      *                                                               *
      *  Called from:                                                 *
      *                                                               *
      *  Calls to:                                                    *
      *    NONE                                                       *
      *                                                               *
      *  Created: 12/19/08   C:09   Project: 81WCN                    *
      *===============================================================*
      *  REVISIONS:                                                   *
      *                                                               *
      *                                                               *
      *===============================================================*
      *  INDICATOR USAGE:                                             *
      *                                                               *
      *  09 - INQUIRY                                                 *
      * N09 - UPDATE                                                  *
      *  90 - GENERAL ERROR INDICATOR                                 *
      *  91 - GENERAL POSITION CURSOR                                 *
      *  92 - GENERAL F KEY PRESSED                                   *
      *  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
      *                                                               *
      *  SCR01:                                                       *
      *  ERROR:                                                       *
      *                                                               *
      *===============================================================*
      ** Display file
     FW4110FN6  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      ** Claim Department Letter workfiles
     FWDCLP     UF   E             DISK
     F                                     RENAME(PCL01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWDC2L     O    E             DISK
     FWDCLL     UF   E           K DISK
     F                                     INFDS(DBCLP)
      ** Claim Master File
     FWMCML1    IF   E           K DISK
      ** Department Code File
     FWMDEL     IF   E           K DISK
      ** Dept/Doc Id File
     FWMDOL     IF   E           K DISK
      ** Fund (Group) Master File
     FWMFNL     IF   E           K DISK
      ** Claim Demographics File
     FWMCDL     IF   E           K DISK
      *================================================================
      *  Table & Array Definitions
      *================================================================
      /COPY SCOPYMT01D
     D WK              S              3  0 DIM(3)                               WEEKS
     D DY              S              3  0 DIM(3)                               DAYS
      *================================================================
      *  Data Structure and Constant Definitions
      *================================================================
     D                 DS
     D  CMSS#                  1      9
     D  X1SS1                  1      3
     D  X1SS2                  4      5
     D  X1SS3                  6      9
      *
     D***PY SCOPYMT01I
      *
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBCLP           DS
     D  CL2RRN               397    400B 0
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('GENERATE CLAIM DEPA-
     D                                     RTMENT LETTERS')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(2)
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      ** Parms breakout for *ENTRY field @@PRMS
     D                 DS
     D  PARMS                  1    136
     D  XXCO#                  1      3  0
     D  XXFND                  4      6  0
     D  XXREQ#                 7     11  0
     D  XXDPCD                12     16
     D  XXDCID                17     28
     D  XXDESC                29     56
     D  XXFLDR                57    136
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@PRMS          136
     C                   PARM                    @@DEPT            2
     C                   PARM                    @@RTCD            1
      ** Normal Work With entry Parms
     C     WWPARM        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
      ** Company file/fund (group) retrieve program parameter list
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *================================================================
      *  KLIST definitions
      *================================================================
      ** Letter workfile
     C     CLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXREQ#
     C                   KFLD                    XXDPCD
     C                   KFLD                    XXFLDR
     C                   KFLD                    XXDCID
      ** Claim Master file
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      ** Company Master file
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      ** Department Master file
     C     DEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    XXDPCD
      ** Dept/Doc ID Master file
     C     DOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    XXDPCD
      ** Employer Master file
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      ** Group Master file
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      ** Group Auxiliary file
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      ** Custom Group ID file - SMH1P
     C     H1KY          KLIST
     C                   KFLD                    XXKEY            40
     C                   KFLD                    XXCSID            2
     C                   KFLD                    XXRFMT            1
      ** User Parameter file
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      ** Work History file
     C     WHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      ** Claimant Demographics file
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
      ** Table file - WTABL
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY2           10
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    CLFCLM        XXCASE
     C     *LIKE         DEFINE    CLFYR         XXFYR
     C     *LIKE         DEFINE    DEDEPT        XXDEPT
     C     *LIKE         DEFINE    CLSS#         XXSS#
     C     *LIKE         DEFINE    CLDIV         XXDIV
     C     *LIKE         DEFINE    CLEMP#        XXEMP#
      *
     C/COPY SCOPYMT01C
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
     C                   ENDSR
      *
      *
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR01)
      *================================================================
     C     ERRCK2        BEGSR
      *
     C  N90SCSEL1        IFEQ      *BLANKS
     C     SCSEL2        ANDEQ     *BLANKS
     C                   SETON                                        21  90
     C                   ENDIF
     C*
     C  N90SCSEL1        IFNE      *BLANKS
     C     SCSEL2        ANDNE     *BLANKS
     C                   SETON                                        22  90
     C                   ENDIF
     C*
     C  N90SCSEL1        IFNE      *BLANKS
     C     SCNTCD        IFEQ      *ZEROS
     C     SCFCDT        OREQ      *ZEROS
     C                   SETON                                        23  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCSEL1        IFEQ      *BLANKS
     C     SCNTCD        IFNE      *ZEROS
     C     SCFCDT        ORNE      *ZEROS
     C                   SETON                                        24  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCSEL1        IFNE      *BLANKS
     C     SCNTCD        IFNE      *ZEROS
     C                   Z-ADD     SCNTCD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *ON
     C                   SETON                                        25  90
     C                   ELSE
     C                   MOVEL     SRDTO         SCNTCD
     C                   ENDIF                                                  SRERR = *ON
     C                   ENDIF
     C  N90SCFCDT        IFNE      *ZEROS
     C                   Z-ADD     SCFCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *ON
     C                   SETON                                        26  90
     C                   ELSE
     C                   MOVEL     SRDTO         SCFCDT
     C                   ENDIF                                                  SRERR = *ON
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCSEL2        IFNE      *BLANKS
     C     SCWNDT        IFEQ      *ZEROS
     C                   SETON                                        27  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCSEL2        IFEQ      *BLANKS
     C     SCWNDT        IFNE      *ZEROS
     C                   SETON                                        28  90
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCSEL2        IFNE      *BLANKS
     C     SCWNDT        IFNE      *ZEROS
     C                   Z-ADD     SCWNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *ON
     C                   SETON                                        29  90
     C                   ELSE
     C                   MOVEL     SRDTO         SCWNDT
     C                   ENDIF                                                  SRERR = *ON
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      *================================================================
      * ERRCK3 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK3        BEGSR
      *
     C                   ENDSR
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
      *
     C                   ENDSR
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
      ** Screen 1
      *
     C                   MOVE      *BLANKS       SCMODE            1
     C                   MOVEL     SCCBP1        CL60A3
     C                   MOVEL     SCCBP2        CL60A4
     C                   MOVEL     SC60A5        CL60A5
     C                   MOVEL     SC60A6        CL60A6
     C                   MOVEL     SCSEL1        CL1A4
     C                   MOVE      SCNTCD        CLDT1
     C                   MOVE      SCFCDT        CLDT2
     C                   MOVEL     SCSEL2        CL1A5
     C                   MOVEL     SCWNDT        CLDT3
      *
     C                   ENDSR
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
      *
      ** This is so the CL will not submit a job if we cancel out
     C     *INKL         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR01'
     C     CLKY          DELETE    PCL01                              59
     C                   MOVE      'B'           @@RTCD
     C                   ENDIF
      *
     C                   MOVE      *ON           *INLR
      *
     C                   ENDSR
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C                   MOVE      'A'           @@MODE
     C                   MOVE      @@PRMS        PARMS
      *
      ** Read the Claim Department Letter Work File (WDCLP) to
      ** get the relative record number for the Work With process
     C     CLKY          CHAIN     PCL01                              60
      *
     C                   Z-ADD     CLFYR         XXFYR
     C                   Z-ADD     CLFCLM        XXCASE
     C                   Z-ADD     CLEMP#        XXEMP#
     C                   Z-ADD     CLDIV         XXDIV
      *
     C                   Z-ADD     XXFND         SCFND
     C                   Z-ADD     XXFYR         SCFYR
      *
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
      *
      ** Load department fields
     C                   MOVE      @@DEPT        XXDEPT
     C                   MOVE      XXDPCD        SCDPCD
      *
     C     DEKY          CHAIN     PDE01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      DEDPNM        SCDPNM
     C                   ELSE
     C                   MOVE      *BLANKS       SCDPNM
     C                   ENDIF
      *
      ** Load document fields
     C                   MOVE      XXDCID        SCDCID
      *
     C     DOKY          CHAIN     PDO01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DODESC        SCDESC
     C                   ELSE
     C                   MOVE      *BLANKS       SCDESC
     C                   ENDIF
      *
      ** Load claimant name
     C     CMKY          CHAIN     PCM01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     CMLSN         CAT       ', ':0        SCCLNM
     C     SCCLNM        CAT(P)    CMFSN:1       SCCLNM
     C     SCCLNM        CAT(P)    CMMID:1       SCCLNM
     C                   ELSE
     C                   MOVE      *BLANKS       SCCLNM
     C                   ENDIF
      *
     C                   Z-ADD     CMCASE        SCFCL2
      *
      ** Get claimant demographic record
     C     CDKEY         CHAIN     PCD01                              60
      *
     C*
     C                   ENDSR
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
      *
     C                   ENDSR
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
      *
     C                   EXSR      MVSCDB
      *
     C     CLKY          CHAIN     PCL01                              60
      *
     C*
      ** Write the records
     C                   UPDATE    PCL01
     C                   WRITE     PC201
      *
     C                   FEOD      WDCLP
      *
      ** Turn on outflag to avoid additional data entry screens
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDSR
