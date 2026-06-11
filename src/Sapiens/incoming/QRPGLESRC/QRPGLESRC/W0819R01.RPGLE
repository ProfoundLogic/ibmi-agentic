     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0819R01 - CLT139 - MISCELLANEOUS ENDORSE - PROMPT
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  This program will prompt for EMP/DIV/FYR in    *
     H*                order to allow direct menu access:             *
     H*                  WORK WITH MISCELLANEOUS ENDORSEMENTS         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    SRMISCI - CLT139 - WORK WITH MISCELLANEOUS ENDORSEMENTS    *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/13/09 C:61 PROJECT 139WLR                                  *
     H*               Modified from W0815R01.                         *
     H*                                                               *
     H*****************************************************************
      *
      *
     FW0819F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMEMP     UF A E             DISK
     F                                     RENAME(PEM01:DBRCDF)
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDSE)
     FWDELL     IF   E           K DISK
     F                                     INFDS(DBFDSY)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
      *
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26  0
      *
     D*
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D DBFDSY          DS
     D  DBRRNY               397    400B 0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Miscellaneous Endor-
     D                                     sements')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*  VALIDATE EMPLOYER/DIVISION
     C*
     C  N90EMKY          CHAIN     PEM01                              60
     C  N90
     CAN 60              SETON                                        71  90
     C*
     C*  VALIDATE YEAR
     C*
     C  N90ELKY          CHAIN     PEL01                              60
     C  N90
     CAN 60              SETON                                        72  90
      *
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   MOVE      *ZERO         LDEND#
     C                   MOVE      SCFYR         LDFYR
     C                   OUT       *DTAARA
      *
     C     *IN90         IFEQ      *OFF
      *
     C                   CALL      'SRMISCI'                            99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      *BLANK        ##CALL
     C                   PARM                    ##RRN
     C                   PARM      DBRRNY        ##RRN2
     C                   PARM      Q1PGM         ##CPGM           10            CALLING PGM
     C*
     C                   SELECT
      *
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN90
      *
     C     *INKC         WHENEQ    *OFF
     C                   CLEAR                   SCEMP#
     C                   CLEAR                   SCDIV
     C                   CLEAR                   SCFYR
     C                   MOVE      *ON           *IN90
      *
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C*
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C*    WORK WITH EMPLOYERS
     C*
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C     HFLD          OREQ      'SCFYR'
     C     SCEMP#        ANDEQ     *ZEROS
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFYR'
     C                   MOVE      'Y'           ##CALL
     C                   OTHER
     C                   MOVE      '2'           ##CALL
     C                   ENDSL
     C*
     C                   CALL      'W0050R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DBRRNF        ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     WMEMP                              60
     C*
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFYR'
     C     ##RRN2        CHAIN     WDELP                              60
     C                   Z-ADD     ELFYR         SCFYR
     C                   OTHER
     C                   MOVE      *ON           *IN21
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFYR'
     C                   MOVE      *ON           *IN21
     C                   OTHER
     C                   MOVE      *ON           *IN20
     C                   ENDSL
     C*
     C                   ENDIF                                                  ##RRN<>*ZEROS
     C*_______________________________________________________________
     C*
     C*    WORK WITH EMPLOYER POLICY YEARS
     C*
     C     HFLD          WHENEQ    'SCFYR'
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   CALL      'W0051R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      DBRRNE        ##RRN2
     C                   PARM      'W'           ##QTE
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     WDELP                              60
     C                   Z-ADD     ELFYR         SCFYR
     C                   ELSE
     C                   MOVE      *ON           *IN21
     C                   ENDIF                                                  @@RRN<>*ZEROS
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* KMDO - SUBROUTINE FOR F13
     C*===============================================================
     C*
     C*  F13=CHANGE USER PARMS
     C*
     C     KMDO          BEGSR
     C*
     C                   CALL      'W0006R'                             99
     C*
     C                   EXSR      GETCOF
     C*
     C*  GET GROUP RRN
     C*
     C                   CLEAR                   DBRRNF
     C     FNKY          CHAIN(N)  PFN01                              60
     C                   Z-ADD     ##FND         SCFND
     C                   MOVEL(P)  ##FNAM        SCNAME
     C*
     C                   ENDSR                                                  KMDO
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C                   ENDSR                                                  MVSCDB
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKM         CASEQ     *ON           KMDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN09
     C                   MOVE      *ZERO         @@RRN             9 0
     C*
     C*  IN ORDER TO KEEP FROM EXECUTING WRITE/UPDATE SUBROUTINE @@MODE
     C*  WILL BE LOADED TO A HOLD FIELD AND CHANGED TO 'X'
     C*
     C                   MOVE      'X'           @@MODE            1
     C*
     C*  GET GROUP RRN
     C*
     C                   CLEAR                   DBRRNF
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     ##FND         SCFND
     C                   MOVEL(P)  ##FNAM        SCNAME
     C*
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR                                                  PREOTH
     C*
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMEMP
     C*
     C                   ENDSR                                                  WRITER
