     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W2268R02 - CAMPUS LOCATION DETAIL INFOMATION
     H*****************************************************************
     H*  CALLED FROM: W2268R - CAMPUS LOCATION DETAIL                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 07/16/03  PROGRAMMER: C:136 PROJECT: 25WNL          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  01/04/10 C:190 PROJECT: 25WRE                                *
     H*                 Added pgm  to current release.                *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FW2268F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWDCYP     UF A E             DISK
     F                                     RENAME(PCY01:DBRCDF)
     F*
     FWDCYL     IF   E           K DISK
     F*
     F*  Campus Location Master
     FWMCLP     IF   E             DISK
     F*
     F*  Fund master file
     FWMFNL     IF   E           K DISK
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
      *
      *  Employer Policy year
     FWDELL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Campus Location Deta-
     D                                     il')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     D*
     D*  Data structure for DARTS
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C*
     C*  Note: @@RRN is the RRN of WDCYP
     C*        @@RRN2 is the RRN of WMCLP
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
      *
      *  Employer Policy Year
      *
     C     ELKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLEMP#
     C                   KFLD                    CLDIV
     C                   KFLD                    SCFYR
      *
      *  Group Master
      *
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C*
      *  Employer Master
      *
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  CAampus Location Detail File
     C*
     C     CYKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCCMPS
     C                   KFLD                    SCFYR
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    EMEMP#        QQEMP#
     C     *LIKE         DEFINE    EMDIV         QQDIV
     C     *LIKE         DEFINE    EMNAME        QQNAME
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C*
     C/COPY SCOPYMT01C
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  Validate policy year
     C  N90SCFYR         IFLE      0
     C                   SETON                                        72  90
     C                   ENDIF
      *
      *  Entered policy year must exist for employer.
      *
     C  N90*IN40         IFEQ      *ON
     C     ELKY          CHAIN     PEL01                              60
     C   60              SETON                                        75  90
     C                   ENDIF
      *
     C*
     C*  Validate nmber of employers
     C  N90SCEMPY        IFLE      0
     C                   SETON                                        73  90
     C                   ENDIF
     C*
     C*  If in add mode and a record already exists display error
     C  N90*IN40         IFEQ      *ON
     C*
     C     CYKY          CHAIN(N)  PCY01                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        74  90
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN40=*ON
     C*
     C*  Check to be sure record has not been updated since entry
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90CYDT          IFGT      SAVEDT
     C     CYDT          OREQ      SAVEDT
     C     CYTIME        ANDGT     SAVETM
     C                   SETON                                        71  90
     C                   ENDIF                                                  LDDT IFGT SAVEDT
     C*
     C                   ENDSR                                                  ERRCK1
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14=DARTS
     C*===============================================================
     C     KNDO          BEGSR
     C*
     C*  Establish key
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
     C*
     C*  Call DARTS entry program
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     CYFYR         SCFYR
     C                   Z-ADD     CYEMPY        SCEMPY
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCFYR         CYFYR
     C                   Z-ADD     SCEMPY        CYEMPY
     C                   Z-ADD     ##CO#         CYCO#
     C                   Z-ADD     SCFND         CYFND
     C                   Z-ADD     SCEMP#        CYEMP#
     C                   Z-ADD     SCDIV         CYDIV
     C                   MOVE      SCCMPS        CYCMPS
     C                   MOVE      'A'           CYSTAT
     C*
     C                   ENDSR                                                  MVSCDB
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKN         CASEQ     *ON           KNDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      *
     C*                    SETON                     LR
      *
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C     @@MODE        COMP      'A'                                    40    ADD MODE
     C*
     C*  Chain to WMALP for header info
     C     @@RRN2        CHAIN     WMCLP                              60
     C*
     C*     Get fund info
     C                   Z-ADD     CLFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C                   Z-ADD     CLEMP#        SCEMP#
     C                   Z-ADD     CLDIV         SCDIV
     C*
     C*     Get employer info
     C     EMKY          CHAIN     PEM01                              60
     C*
     C*    Compress emp#/div/name
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        QQEMP#
     C                   PARM      EMDIV         QQDIV
     C                   PARM      EMNAME        QQNAME
     C                   PARM                    QQEDN            80
     C*
     C                   MOVEL     QQEDN         SCEDN
     C*
     C*     Fill other fields
     C                   MOVE      CLCMPS        SCCMPS
     C                   MOVEL     CLDESC        SCDESC
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR                                                  PREOTH
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        CYUSER
     C                   MOVEL     Q1PGM         CYPGM
     C                   Z-ADD     DSTIME        CYTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CYDT
     C*
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C*
     C                   ENDSR                                                  UPDATR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        CYUSER
     C                   MOVEL     Q1PGM         CYPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CYDT
     C                   MOVEL     DSTIME        CYTIME
     C*
     C                   MOVE      CYUSER        CYIUSR
     C                   MOVE      CYPGM         CYIPGM
     C                   MOVE      CYDT          CYIDT
     C                   MOVE      CYTIME        CYITM
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      WDCYP
     C                   ENDSR                                                  WRITER
