     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0170R02 - CANCELLATION CONFIRMATION INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 09/21/98  PROGRAMMER: C:87  PROJECT: WC762          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/03/07 C:33  Seq#: WA099/44610                              *
     H*                Release record lock                            *
     H*                                                               *
     H* 06/14/02 C:61  PROJECT WC577                                  *
     H*                Display correct CX Reason Desc.                *
     H*                                                               *
     H* 05/20/02 C:61  SUPPORT WA045 32367                            *
     H*                Display missing S/R month if there is one.     *
     H*                                                               *
     H*  05/09/01  C:136 PROJECT: WC762                               *
     H*                  Pgm wasn't allowing user to clear confir-    *
     H*                  mation dates on W0170R SCR01.                *
     H*                                                               *
     H*  10/21/99  C:87  PROJECT: WC577                               *
     H*                  Use WTCRP instead of WTABL to get info for   *
     H*                  the cancellation reason.                     *
     H*                                                               *
     H*  08/19/99  C:87  PROJECT: SUPP WA091 #19271                   *
     H*                  Pgm  was not putting correct date in for     *
     H*                  confirmation when processing other divs.     *
     H*                                                               *
     H*****************************************************************
     H*
     FW0170F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWDCXP     UF A E             DISK
     F                                     RENAME(PCX01:DBRCDF)
     F*
     F***  Begin Chg  ***  01/03/07  **************************************
     F**********WDCXL6  UF  E           K        DISK
     FWDCXL6    IF   E           K DISK
     F                                     RENAME(PCX01:PCX06)
     F                                     INFDS(CXFDS6)
     F***  End   Chg  ***  01/03/07  **************************************
     F*
     F*  Cancellation confirmation control file
     FWMCFL     IF   E           K DISK
     F*
     F*  Group master file
     FWMFNL     IF   E           K DISK
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
     F*
     F*  Employer detail file
     FWDELL     IF   E           K DISK
     F*
     F***  BEGIN CHG  ***  10/21/99  **********************************
     F*  Table file
     F***WTABL   IF  E           K        DISK
     F*
     FWTCRL     IF   E           K DISK
     F*
     FWMRSL     IF   E           K DISK
     F***  END   CHG  ***  10/21/99  **********************************
      /COPY SCOPYMT01D
     D*
     D***  Begin Add  ***  01/03/07  **************************************
     D CXFDS6          DS
     D  CXRRN6               397    400B 0
     D***  End   Add  ***  01/03/07  **************************************
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Cancellation Confirm-
     D                                     ations')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
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
     C*   Cancellation detail key list
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCXDT
     C                   KFLD                    KKEMP#
     C*
     C*   Cancellation confirmation control key list
     C     CFKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*   Group master key list
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*   Employer master key list
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*   Employer detail key list
     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C***  BEGIN CHG  ***  10/21/99  **********************************
     C*   Table file key list
     C********** TBKY      KLIST
     C**********           KFLD           KKCO#
     C**********           KFLD           KKKEY
     C*
     C     CRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKST
     C                   KFLD                    KKKEY
     C                   KFLD                    KKEFDT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKST
     C                   KFLD                    KKKEY
     C*
     C     RSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKRTCD
     C***  END   CHG  ***  10/21/99  **********************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CXCO#         KKCO#
     C     *LIKE         DEFINE    CXFND         KKFND
     C     *LIKE         DEFINE    CXCXDT        KKCXDT
     C     *LIKE         DEFINE    CXEMP#        KKEMP#
     C     *LIKE         DEFINE    CXDIV         SAVDIV
     C     *LIKE         DEFINE    CXCDAT        SVCDAT
     C     *LIKE         DEFINE    CXCFDT        SVCFDT
     C     *LIKE         DEFINE    ELDIV         KKDIV
     C     *LIKE         DEFINE    ELFYR         KKFYR
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    CRKEY         KKKEY
     C***  BEGIN ADD  ***  10/21/99  **********************************
     C     *LIKE         DEFINE    RSRTCD        KKRTCD
     C     *LIKE         DEFINE    RSJRST        KKST
     C     *LIKE         DEFINE    CREFDT        KKEFDT
     C***  END   ADD  ***  10/21/99  **********************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* ASNMNT - Assigns values to maintenance fields for write or update
     C*================================================================
     C     ASNMNT        BEGSR
     C*
     C                   MOVEL     Q1USER        CXUSER
     C                   MOVEL     Q1PGM         CXPGM
     C                   Z-ADD     DSTIME        CXTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXDT
     C*
     C                   ENDSR                                                  ASNMNT
     C*================================================================
     C* CHKDIV - Check and update Canx Dtl recs that have the same
     C*          Canx Eff date and Canx Issue Date
     C*================================================================
     C     CHKDIV        BEGSR
     C*
     C                   Z-ADD     CXDIV         SAVDIV
     C                   Z-ADD     CXCDAT        SVCDAT
     C***  BEGIN ADD  ***  08/19/99  **********************************
     C                   Z-ADD     CXCFDT        SVCFDT
     C***  END   ADD  ***  08/19/99  **********************************
     C*
     C                   Z-ADD     CXCXDT        KKCXDT
     C                   Z-ADD     CXEMP#        KKEMP#
     C*
     C***  Begin Chg  ***  01/03/07  **************************************
     C********** CXKY      CHAINPCX01                60
     C     CXKY          CHAIN(N)  PCX06                              60
     C***  End   Chg  ***  01/03/07  **************************************
     C*
     C     *IN60         DOWEQ     *OFF
     C     CXDIV         IFNE      SAVDIV
     C*
     C     CXCDAT        IFEQ      SVCDAT
     C***  Begin Add  ***  01/03/07  **************************************
     C     CXRRN6        CHAIN     DBRCDF                             60
     C***  End   Add  ***  01/03/07  **************************************
     C                   Z-ADD     SVCFDT        CXCFDT
     C                   EXSR      ASNMNT
     C***  Begin Chg  ***  01/03/07  **************************************
     C**********           UPDATPCX01
     C                   UPDATE    DBRCDF
     C***  End   Chg  ***  01/03/07  **************************************
     C                   ENDIF                                                  CXCDAT=SVCDAT
     C*
     C                   ENDIF                                                  CXDIV<>SAVDIV
     C*
     C***  Begin Chg  ***  01/03/07  **************************************
     C********** CXKY      READEPCX01                    60
     C     CXKY          READE(N)  PCX06                                  60
     C***  End   Chg  ***  01/03/07  **************************************
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHKDIV
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*   Check that a valid date was entered
     C                   Z-ADD     SCCFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *ON
     C                   SETON                                        71  90
     C                   ELSE
     C                   MOVEL     SRDTO         SCCFDT
     C                   ENDIF
     C*
     C*   Check that 'confirmation date' is not less than 'canx eff date'
     C                   Z-ADD     SCCFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCFDT            7 0
     C*
     C  N90XXCFDT        IFLT      CXCXDT
     C                   SETON                                        72  90
     C                   ENDIF
     C*
     C*  Check that fields have not been updated since program entered
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90CXDT          IFGT      SAVEDT
     C     CXDT          OREQ      SAVEDT
     C     CXTIME        ANDGT     SAVETM
     C                   SETON                                        78  90
     C                   ENDIF                                                  AUDT IFGT SAVEDT
     C*
     C                   ENDSR                                                  ERRCK1
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     CXFND         SCFND
     C*
     C*    Get group name
     C                   Z-ADD     CXCO#         KKCO#
     C                   Z-ADD     CXFND         KKFND
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCFNNM
     C*
     C                   Z-ADD     CXEMP#        SCEMP#
     C                   Z-ADD     CXDIV         SCDIV
     C                   Z-ADD     CXFYR         SCFYR
     C*
     C*    Get employer name
     C                   Z-ADD     CXEMP#        KKEMP#
     C                   Z-ADD     CXDIV         KKDIV
     C     EMKY          CHAIN     PEM01                              60
     C*
     C*    Compress Emp#/Div/Name
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
     C*
     C*    Get policy effective date and expiration date
     C                   Z-ADD     CXFYR         KKFYR
     C     ELKY          CHAIN     PEL01                              60
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPFDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPTDT
     C*
     C*  Format cancel date for output
     C                   Z-ADD     CXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C*
     C*  If Confirm date is zero then default to cancel date
     C                   Z-ADD     CXCFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCFDT
     C*
     C     *IN09         IFEQ      *OFF
     C     CXCFDT        ANDEQ     *ZERO
     C                   Z-ADD     SCCXDT        SCCFDT
     C                   ENDIF                                                  *IN09=*OFF
     C*
     C                   MOVE      CXRSN         SCRSN
     C                   MOVEL     CXCOM1        SCCOM1
     C                   MOVEL     CXCOM2        SCCOM2
     C                   Z-ADD     CXAMTD        SCAMTD
      ***** BEG ADD ******* 05/20/02 **********************************
      *
      *  Display missing S/R month if there is one
      *
     C     CXRPMY        IFNE      0
     C                   MOVE      *OFF          *IN50
     C                   Z-ADD     CXRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCRPMY
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN50
     C                   Z-ADD     0             SCRPMY
     C                   ENDIF                                                  CXRPMY<>0
      *
      ***** END ADD ******* 05/20/02 **********************************
     C                   Z-ADD     CXRFEE        SCRFEE
     C*
     C*  Get cancellation reason description
     C                   MOVEL     CXRSN         KKKEY
     C                   MOVEL     *BLANKS       SCRSNM
     C*
     C***  BEGIN CHG  ***  10/21/99  **********************************
     C********** TBKY      CHAINPCR01                60
     C*
     C*  Get the jurisdiction state
     C                   MOVE      *BLANKS       KKST
     C*
     C                   MOVE      ELRTCD        KKRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        KKST
     C                   ENDIF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C*   Get the cancellation reason description
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
     C***  END   CHG  ***  10/21/99  **********************************
      ***** BEG ADD ******* 06/14/02 **********************************
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
      ***** END ADD ******* 06/14/02 **********************************
     C  N60              MOVEL(P)  CRDESC        SCRSNM
     C*
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCCFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXCFDT
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C*
     C     @@MODE        CASEQ     'S'           UPDATR
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHERR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C     @@MODE        IFEQ      'S'
     C*
     C***  Begin Chg  ***  01/03/07  **************************************
     C********** @@RRN     CHAINDBRCDF               60
     C     @@RRN         CHAIN(N)  DBRCDF                             60
     C***  End   Chg  ***  01/03/07  **************************************
     C                   Z-ADD     CXCO#         KKCO#
     C                   Z-ADD     CXFND         KKFND
     C*
     C*  If the confirmation date cannot be different from the canx
     C*  effective date (CFNECX <> 'Y') then update automatically
     C     CFKY          CHAIN     PCF01                              60
     C     *IN60         IFEQ      *OFF
     C*** BEG DEL ******** 05/09/01 **************************************
     C***********CFNECX    ANDNE'Y'
     C****END DEL ******** 05/09/01 **************************************
     C*** BEG ADD ******** 05/09/01 **************************************
     C     CXCFDT        IFNE      0
     C***  Begin Add  ***  01/03/07  **************************************
     C     @@RRN         CHAIN     DBRCDF                             60
     C***  End   Add  ***  01/03/07  **************************************
     C                   Z-ADD     0             CXCFDT
     C                   EXSR      ASNMNT
     C                   UPDATE    DBRCDF
     C                   EXSR      CHKDIV
     C                   RETURN
     C                   ELSE
     C     CFNECX        IFNE      'Y'
     C*** END ADD ******** 05/09/01 **************************************
     C***  Begin Add  ***  01/03/07  **************************************
     C     @@RRN         CHAIN     DBRCDF                             60
     C***  End   Add  ***  01/03/07  **************************************
     C                   Z-ADD     CXCXDT        CXCFDT
     C                   EXSR      ASNMNT
     C                   UPDATE    DBRCDF
     C                   EXSR      CHKDIV
     C                   RETURN
     C*** BEG ADD ******** 05/09/01 **************************************
     C                   ENDIF                                                  CFNECX <> Y
     C                   ENDIF                                                  CXCFDT <> O
     C*** END ADD ******** 05/09/01 **************************************
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  @@MODE='S'
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C     @@MODE        CASEQ     'S'           PRECHG
     C                   ENDCS
     C*
     C                   ENDSR                                                  PREOTH
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   EXSR      ASNMNT
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   EXSR      CHKDIV
     C*
E01  C                   ENDIF
     C                   ENDSR                                                  UPDATR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      WDCXP
     C                   ENDSR                                                  WRITER
