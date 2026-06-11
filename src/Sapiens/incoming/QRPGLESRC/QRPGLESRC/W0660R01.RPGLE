     Hcopyright('(c) 2025 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0660R01 - PAYROLLS - PROMPT
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  This program will prompt for EMP/DIV/FYR in    *
     H*                order to allow direct menu access to one of:   *
     H*                  WW S/R PAYROLLS                              *
     H*                  WW AUDITED PAYROLLS                          *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W0660R -  WORK WITH S/R PAYROLLS                           *
     H*    W0660R2 - WORK WITH AUDITED PAYROLLS                       *
     H*                                                               *
     H*  CREATED: 02/02/98  PROGRAMMER: C:33  PROJECT: WC577          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/08/25 C:300 Project:W2996                                  *
     H*                Call new screen for S/R Payroll Entry          *
     H*                                                               *
     H* 05/04/05 C:33  Seq#: WA071/46875                              *
     H*                Release the Emp Mst file when using for search.*
     H*                                                               *
     H* 09/16/99 C:61  PROJECT WC577                                  *
     H*              - Change SCR01 fields to be required.            *
     H*                                                               *
     H* 08/05/99 C:61  PROJECT WC926                                  *
     H*              - Add parm to indicate if program used to call   *
     H*                W0660R2 - AUDITED PAYROLLS.                    *
     H*              - Load correct screen name based on              *
     H*                existence/value of new parm.                   *
     H*                                                               *
     H* 11/12/98  C:78 PROJECT: WC872                                 *
     H*                CHANGE SCREEN, PROMPT FOR FUND YEAR FIRST,     *
     H*                EMPLOYER # SECOND.                             *
     H*                                                               *
     H* PLEASE ADD TO TOP                                             *
     H*****************************************************************
      *
      *
     FW0660F01  CF   E             WORKSTN
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
      ***** BEG DEL ******* 08/05/99 **********************************
     D************  'S/R Payrolls'        C         OPTNM
      ***** END DEL ******* 08/05/99 **********************************
      ***** BEG ADD ******* 08/05/99 **********************************
     D OPTNM           C                   CONST('S/R Payrolls        ')
     D OPTNM2          C                   CONST('Audited Payrolls    ')
      ***** END ADD ******* 08/05/99 **********************************
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
      ***** BEG ADD ******* 08/05/99 **********************************
     C                   PARM                    @@SROA            1            S/R or Audited
      *
      *   @@SROA
      *      'A'  = Audited payrolls
      *      ELSE = S/R payrolls
      *
      *
      ***** END ADD ******* 08/05/99 **********************************
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
     C*
     C     *IN90         IFEQ      *OFF
      *
      ***** BEG ADD ******* 08/05/99 **********************************
      *
      *   Audited Payrolls
      *
     C     Q1PRMS        IFGT      3
     C     @@SROA        ANDEQ     'A'
      *
     C                   CALL      'W0660R2'                            99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      *BLANK        ##CALL
     C                   PARM      SVMODE        ##MODE            1
     C                   PARM      DBRRNY        ##RRN                          WDELP
      *
     C                   ELSE
      *
      *   S/R Payrolls
      *
      ***** END ADD ******* 08/05/99 **********************************
     C                   CALL      'W0660R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      SVMODE        ##CALL
     C                   PARM                    ##RRN
     C                   PARM      DBRRNY        ##RRN2
      ***** BEG ADD ******* 08/05/99 **********************************
      *
     C                   ENDIF                                                  Q1PRMS
      *
      ***** END ADD ******* 08/05/99 **********************************
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
     C***  Begin Chg  ***  05/04/05  **************************************
     C********** ##RRN     CHAINWMEMP                60
     C     ##RRN         CHAIN(N)  WMEMP                              60
     C***  End   Chg  ***  05/04/05  **************************************
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
     C     @@MODE        COMP      ' '                                0909
     C                   Z-ADD     *ZEROS        @@RRN             9 0
     C*
     C*  IN ORDER TO KEEP FROM EXECUTING WRITE/UPDATE SUBROUTINE @@MODE
     C*  WILL BE LOADED TO A HOLD FIELD AND CHANGED TO 'X'
     C*
     C                   MOVE      @@MODE        SVMODE            1
     C                   MOVE      'X'           @@MODE
     C*
     C*  GET GROUP RRN
     C*
     C                   CLEAR                   DBRRNF
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     ##FND         SCFND
     C                   MOVEL(P)  ##FNAM        SCNAME
     C*
      ***** BEG ADD ******* 08/05/99 **********************************
      *
      *  Determine if called for S/R or Audited Payrolls
      *
     C     Q1PRMS        IFGT      3
     C     @@SROA        ANDEQ     'A'
      *
     C                   MOVEL(P)  OPTNM2        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
      ***** Beg Add ******* 01/08/25 **********************************
     C                   ELSE
      * Call new screen for S/R Payroll Entry
     C                   CALL      'WTSRPAYROL'
     C                   RETURN
      ***** End Add ******* 01/08/25 **********************************
     C                   ENDIF
      *
      ***** END ADD ******* 08/05/99 **********************************
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
