     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* W0162R02 - Cancellation Detail Maintenance
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 07/25/05 C:61  PROJECT 133WBV
     H*                Display "Number of CX" for employer.
     H*
     H* 07/07/05 C: 58 PROJECT: WC1205
     H*              - Access activity status code from employer
     H*                detail WDELP rather than master WMEMP.
     H*                                                               *
     H* 05/02/03 C:61  PROJECT WC1108                                 *
     H*              - Change header from "Minimum balance" to        *
     H*                "% of Premium".                                *
     H*                                                               *
     H* 04/26/02 C:61  PROJECT WC577                                  *
     H*                Correct CX reason code validation.             *
     H*                                                               *
     H* 01/30/02 C:61  SUPPORT WA107 31022                            *
     H*                OPT4 = Delete, delete all wrkfil records for   *
     H*                employer (all divisons).                       *
     H*                                                               *
     H*  12/02/99 C:90 PROJECT: WC912                                 *
     H*                ADD 'BALANCE TYPE' (CCEAFL) TO HEADER.         *
     H*                                                               *
     H*  11/29/99 C:118 PROJECT: WC577                                *
     H*                 Load Group number and name into header.       *
     H*                                                               *
     H*  10/07/99 C:87 PROJECT: WC894                                 *
     H*                In screen hdgs chg 'Balance date' to 'Billings *
     H*                through date' and put 'Payments through dt'    *
     H*                immediately below.  Adjust other headings in   *
     H*                order to accomodate.                           *
     H*                                                               *
     H*  09/02/99 C:61 PROJECT WC577                                  *
     H*                Add Activity Status Description to header      *
     H*                info.                                          *
     H*                                                               *
     H*  06/22/99 C:61 PROJECT 48WQJ                                  *
     H*                Add previous fund number to header info.       *
     H*                                                               *
     H*  11/10/98 C:87  PROJECT: WC852                                *
     H*                 Change the call to W2196R (Cancellation rsn   *
     H*                 code search) to pass the jurisdiction state.  *
     H*                                                               *
     H*  09/18/98 C:61  PROJECT 48WLC                                 *
     H*                 Display old Emp#/Div from Employer Cross      *
     H*                 Reference (WZEXP).                            *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H* GENERAL                                                       *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  63       DISPLAY OLD EMPLOYER NUMBER                         *
     H*  64       DISPLAY ACTIVITY STATUS DESCRIPTION                 *
     H*                                                               *
     H*****************************************************************
      *
      *
     FW0162F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  Physical file definition (for maintaining record)
     FWXCNP     UF   E             DISK
     F                                     RENAME(PCN01:DBRCDF)
      ***** BEG ADD ******* 01/30/02 **********************************
     FWXCNL4    UF   E           K DISK
      ***** END ADD ******* 01/30/02 **********************************
     FWXCCL     IF   E           K DISK
     FWDELL     IF   E           K DISK
      ***** BEG ADD ******* 09/18/98 **********************************
      *
      *  Employer Cross Reference
     FWZEXL     IF   E           K DISK
     F                                     RENAME(PEX01:PEX02)
      ***** END ADD ******* 09/18/98 **********************************
     FWMBXL1    IF   E           K DISK
     F***  BEGIN CHG  ***  11/10/98  **********************************
     F***WTABL   IF  E           K        DISK
     F*********** PCC01                             KIGNORE
     F*********** PRS01                             KIGNORE
     FWTCRL     IF   E           K DISK
     F*
     FWMRSL     IF   E           K DISK
     F***  END   ADD  ***  11/10/98  **********************************
      ***** BEG ADD ******* 09/02/99 **********************************
      *
     F***** BEG DEL ******* 07/07/05 **********************************
      * Employer Master
     F***********WMEML   IF  E           K        DISK
     F***** END DEL ******* 07/07/05 **********************************
      *
      * Activity Status Table
     FWTAVL     IF   E           K DISK
      ***** END ADD ******* 09/02/99 **********************************
     F***** BEGIN ADD **** 11/29/99 ***********************************
     FWMFNL     IF   E           K DISK
     F***** END   ADD **** 11/29/99 ***********************************
      *
     D***PY SCOPYMT01I
      *
      *================================================================
      *  /COPY required constants
      *================================================================
      /COPY SCOPYMT01D
     D OPTNM           C                   CONST('Cancellation Batch D-
     D                                     etail')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      * KEY LIST FOR THE CANCELLATION BATCH CONTROL FILE
      *
     C     CCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBAT#
      *
     C***  BEGIN CHG  ***  11/10/98  **********************************
      * KEY LIST FOR THE CANCELLATION REASON CODE TABLE FILE
      **********
     C********** TBKY      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXKEY
     C*
     C     CRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKST
     C                   KFLD                    XXKEY
     C                   KFLD                    KKEFDT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKST
     C                   KFLD                    XXKEY
     C***  BEGIN CHG  ***  11/10/98  **********************************
      *
      * KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      ***** BEG ADD ******* 09/18/98 **********************************
      *
      *  Employer Cross Reference
      *
     C     EXKY          KLIST
     C                   KFLD                    CNCO#
     C                   KFLD                    CNFND
     C                   KFLD                    CNEMP#
     C                   KFLD                    CNDIV
      ***** END ADD ******* 09/18/98 **********************************
      *
      * KEY LIST FOR THE BILLING SCHEDULE CODE AUXILIARY FILE
      *
     C     BXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBSCD
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C*  Key list for Rate Set Code file
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKRTCD
     C***  END   ADD  ***  11/10/98  **********************************
      ***** BEG ADD ******* 09/02/99 **********************************
      *
      *  Employer Master
      *
     C     EMKY          KLIST
     C                   KFLD                    CNCO#
     C                   KFLD                    CNFND
     C                   KFLD                    CNEMP#
     C                   KFLD                    CNDIV
      *
      *  Activity Status
      *
     C     AVKY          KLIST
     C                   KFLD                    CNCO#
     C                   KFLD                    KKKEY
      *
      ***** END ADD ******* 09/02/99 **********************************
     C***** BEGIN ADD **** 11/29/99 ***********************************
     C*
     C*  Group Key List
     C*
     C     FNKY          KLIST
     C                   KFLD                    CNCO#
     C                   KFLD                    CNFND
     C*
     C***** END   ADD **** 11/29/99 ***********************************
      ***** BEG ADD ******* 01/30/02 **********************************
      *
      *  Work file - Mass CX
      *
     C     CNKY          KLIST
     C                   KFLD                    X1CO#
     C                   KFLD                    X1FND
     C                   KFLD                    X1BAT#
     C                   KFLD                    X1EMP#
      *
      ***** END ADD ******* 01/30/02 **********************************
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    CCBAT#        XXBAT#
     C     *LIKE         DEFINE    CCCO#         XXCO#
     C     *LIKE         DEFINE    CCFND         XXFND
     C     *LIKE         DEFINE    CRKEY         XXKEY
     C     *LIKE         DEFINE    BXBSCD        XXBSCD
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     *LIKE         DEFINE    RSRTCD        KKRTCD
     C     *LIKE         DEFINE    ELPFDT        KKEFDT
     C     *LIKE         DEFINE    CRST          KKST
     C***  END   ADD  ***  11/10/98  **********************************
      ***** BEG ADD ******* 09/02/99 **********************************
     C     *LIKE         DEFINE    AVKEY         KKKEY
      ***** END ADD ******* 09/02/99 **********************************
      ***** BEG ADD ******* 01/30/02 **********************************
     C     *LIKE         DEFINE    CNCO#         X1CO#
     C     *LIKE         DEFINE    CNFND         X1FND
     C     *LIKE         DEFINE    CNBAT#        X1BAT#
     C     *LIKE         DEFINE    CNEMP#        X1EMP#
      ***** END ADD ******* 01/30/02 **********************************
      *
     C/COPY SCOPYMT01C
      *
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
      * ERRCK1 - Error checking for screen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
      * SELECT FOR CX FLAG MUST BE 'Y' OR 'N'
      *
     C     SCSELC        IFNE      'Y'
     C     SCSELC        ANDNE     'N'
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCSELC IFNE
      *
      * CANCELLATION REASON CODE MUST BE VALID
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C*  Get employer jurisdiction state
     C                   Z-ADD     CNEMP#        XXEMP#
     C                   Z-ADD     CNDIV         XXDIV
     C                   Z-ADD     CNFYR         XXFYR
     C     ELKY          CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        KKRTCD
     C                   Z-ADD     ELPFDT        KKEFDT
     C     RSKY          CHAIN     PRS01                              60
     C  N60              MOVE      RSJRST        KKST
     C***  END   ADD  ***  11/10/98  **********************************
      *
     C  N90              MOVEL(P)  SCRSN         XXKEY
     C*
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C**N90***** TBKY      CHAINPCR01                61
     C*
     C  N90CRKY          SETGT     PCR01
     C  N90CRKY1         READPE    PCR01                                  61
     C***  END   CHG  ***  11/10/98  **********************************
      ***** BEG ADD ******* 04/26/02 **********************************
      *
     C  N90*IN61         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  61
     C                   ENDDO
      *
      ***** END ADD ******* 04/26/02 **********************************
      *
     C  N90SCRSN         IFEQ      *BLANK
     C     *IN61         OREQ      *ON
     C     CRSTAT        OREQ      'I'
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCRSN IFEQ
      *
      * REINSTATEMENT FEE CANNOT BE LESS THAN ZERO
      *
     C  N90SCRFEE        IFLT      0
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCRFEE IFNE
      *
      *  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N90CNDT          IFGT      SAVEDT
     C     CNDT          OREQ      SAVEDT
     C     CNTIME        ANDGT     SAVETM
      *
     C                   SETON                                        93  90
      *
     C                   ENDIF                                                  CNDT IFGT
      *
     C                   ENDSR                                                  ERRCK1
      *================================================================
      * ERRCK2 - Error checking for screen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
      *================================================================
      * ERRCK3 - Error checking for screen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
      *  F10=SEARCH
      *
     C     KJDO          BEGSR
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C*  Get employer jurisdiction state
     C                   Z-ADD     CNEMP#        XXEMP#
     C                   Z-ADD     CNDIV         XXDIV
     C                   Z-ADD     CNFYR         XXFYR
     C     ELKY          CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        KKRTCD
     C     RSKY          CHAIN     PRS01                              60
     C  N60              MOVE      RSJRST        ##JRST
     C   60              MOVE      *BLANKS       ##JRST
     C***  END   ADD  ***  11/10/98  **********************************
      *
     C     HFLD          IFEQ      'SCRSN'
     C                   CALL      'W2196R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RSN            10
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   PARM                    ##JRST            2
     C***  END   ADD  ***  11/10/98  **********************************
     C                   ENDIF                                                  HFLD IFEQ
      *
     C     ##RSN         IFNE      *BLANKS
     C                   MOVEL     ##RSN         SCRSN
     C                   ENDIF                                                  ##RSN IFNE
      *
     C                   ENDSR                                                  KJDO
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVE      CNCO#         XXCO#
     C                   MOVE      CNFND         XXFND
     C                   MOVE      CNBAT#        XXBAT#
      *
     C     CCKY          CHAIN     PCC01                              60
      ***** BEG ADD ******* 05/02/03 **********************************
     C     CCADPP        COMP      'P'                                    43
      ***** END ADD ******* 05/02/03 **********************************
      *
     C                   MOVE      CCBAT#        SCBAT#
     C*** BEG ADD **** 02/18/03 ******************************************
     C     CCFREQ        COMP      *BLANKS                            4141
     C   41CCWEDT        COMP      0                                  4040
      *
     C     *IN41         IFEQ      *ON
     C                   MOVE      CCWEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWEDT
     C                   ENDIF
      *
     C   41              SELECT
     C     CCFREQ        WHENEQ    'B'
     C                   MOVEL     'Both'        SCFREQ
     C     CCFREQ        WHENEQ    'W'
     C                   MOVEL     'Weekly'      SCFREQ
     C     CCFREQ        WHENEQ    'M'
     C                   MOVEL     'Monthly'     SCFREQ
     C                   ENDSL
     C*** END ADD **** 02/18/03 ******************************************
     C***** BEGIN ADD **** 11/29/99 ***********************************
     C*
     C*  GET GROUP NAME FOR HEADER
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   MOVEL     CNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C***** END   ADD **** 11/29/99 ***********************************
      *
     C*** BEG CHG **** 02/18/03 ******************************************
     C********** CCRPMY    IFNE 0
     C  N41CCRPMY        IFNE      0
     C*** END CHG **** 02/18/03 ******************************************
      *
     C                   MOVE      CCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCRPMY
     C                   MOVE      *ON           *IN40
      *
     C                   ELSE
      *
      ***** BEG CHG ******* 05/02/03 **********************************
     C*************        MOVE CCMIN     SCMIN
     C                   Z-ADD     CCMIN         SCMIN
      ***** END CHG ******* 05/02/03 **********************************
     C                   MOVE      CCBLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCBLDT
     C***  BEGIN ADD  ***  10/07/99  **********************************
     C                   Z-ADD     CCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPTDT
     C***  END   ADD  ***  10/07/99  **********************************
     C                   MOVE      *OFF          *IN40
      *
     C                   ENDIF                                                  CCRPMY IFNE
      *
     C                   MOVE      CCETYP        SCETYP
      **** BEGIN ADD **** 12/02/99 ************************************
     C                   MOVE      CCEAFL        SCEAFL
      **** END   ADD **** 12/02/99 ************************************
     C                   MOVE      CCFRDT        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTMDYY
      *
     C                   MOVEL     SRDTO         SCFRDT
     C                   MOVE      CCTODT        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTMDYY
      *
     C                   MOVEL     SRDTO         SCTODT
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      CNEMP#        ##EMP#            9 0
     C                   PARM      CNDIV         ##DIV             5 0
     C                   PARM      CNENAM        ##NAME           40
     C     SCEDN         PARM                    ##EDN            80
      *
     C***** BEG DEL ******* 07/07/05 **********************************
      ***** BEG ADD ******* 09/02/99 **********************************
     C********************
     C******************** Get Activity Status Description for Header
     C********************
     C***********EMKY***** CHAINPEM01                64
     C**N64*************** MOVELEMASTS    KKKEY     P
     C**N64******AVKY***** CHAINPAV01                64
     C**N64*************** MOVELAVDESC    SCACTD    P
     C***64*************** MOVEL*BLANKS   SCACTD    P
     C********************
      ***** END ADD ******* 09/02/99 **********************************
     C***** END DEL ******* 07/07/05 **********************************
      *
      ***** BEG ADD ******* 09/18/98 **********************************
      *
      *  Get Employer Cross Reference info for header
      *
     C     EXKY          CHAIN     PEX02                              63
      ***** BEG ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOFND        SCOFND
      ***** END ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
      ***** END ADD ******* 09/18/98 **********************************
      *
     C                   MOVE      CNFYR         SCFYR
      ***** BEG ADD ******* 07/25/05 **********************************
     C                   MOVE      CNCNC#        SCCNC#
      ***** END ADD ******* 07/25/05 **********************************
     C                   MOVE      CNAMTD        SCAMTD
     C                   MOVE      *BLANK        SCTYPE
     C                   MOVE      CNFYR         XXFYR
     C                   MOVE      CNEMP#        XXEMP#
     C                   MOVE      CNDIV         XXDIV
      *
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C***** BEG ADD ******* 07/07/05 **********************************
     C*  Get Activity Status Description for Header
     C                   MOVEL(P)  *BLANKS       SCACTD
     C                   MOVEL(P)  *BLANKS       AVDESC
     C                   MOVEL(P)  ELASCD        KKKEY
     C     AVKY          CHAIN     PAV01                              64
     C                   MOVEL(P)  AVDESC        SCACTD
     C***** END ADD ******* 07/07/05 **********************************
      *
     C                   MOVEL     ELBSCD        XXBSCD
      *
     C     BXKY          CHAIN     PBX01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     BXTYPE        ANDEQ     'A'
      *
     C                   MOVE      BXTYPE        SCTYPE                         ANNUAL INSTALLMT PLA
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVE      CNADJ         SCADJ
     C                   MOVE      CNVIP         SCVIP
     C                   MOVE      CNSELC        SCSELC
     C                   MOVE      CNRECD        SCRSN
     C                   MOVE      CNRFEE        SCRFEE
     C                   MOVE      CNCOM1        SCCOM1
     C                   MOVE      CNCOM2        SCCOM2
      *
     C                   ENDSR                                                  MVDBSC
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
     C                   MOVE      SCSELC        CNSELC
     C                   MOVE      SCRSN         CNRECD
     C                   MOVE      SCRFEE        CNRFEE
     C                   MOVE      SCCOM1        CNCOM1
     C                   MOVE      SCCOM2        CNCOM2
     C                   MOVE      Q1USER        CNUSER
     C                   MOVE      Q1PGM         CNPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         CNDT
     C                   MOVEL     DSTIME        CNTIME
      *
     C                   ENDSR                                                  MVSCDB
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90
      *
     C                   ENDCS                                                  *INKJ CASEQ
      *
     C                   ENDSR                                                  OTHFKS
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
      ***** BEG ADD ******* 09/18/98 **********************************
     C                   SETOFF                                       63
      ***** END ADD ******* 09/18/98 **********************************
      ***** BEG ADD ******* 01/30/02 **********************************
      *
      * If called in Delete mode, delete all wrkfil records for
      * employer (all divisions).
      *
     C     @@MODE        IFEQ      'E'
     C                   Z-ADD     CNCO#         X1CO#
     C                   Z-ADD     CNFND         X1FND
     C                   Z-ADD     CNBAT#        X1BAT#
     C                   Z-ADD     CNEMP#        X1EMP#
      *
     C     *IN60         DOUEQ     *ON
     C     CNKY          DELETE    PCN01                              60
     C                   ENDDO
      *
     C                   ENDIF                                                  MODE = E
      *
      ***** END ADD ******* 01/30/02 **********************************
     C                   ENDSR
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR
      *
      *
      *
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
      *
     C     @@MODE        IFEQ      'S'
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     CNSELC        IFEQ      'Y'
     C                   MOVE      'N'           CNSELC
     C                   ELSE
     C                   MOVE      'Y'           CNSELC
     C                   ENDIF                                                  CNSELC IFEQ
      *
     C                   MOVE      Q1USER        CNUSER
     C                   MOVE      Q1PGM         CNPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         CNDT
     C                   MOVEL     DSTIME        CNTIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   RETURN
      *
     C                   ENDIF                                                  @@MODE IFEQ
      *
     C                   ENDSR                                                  PREOTH
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
     C                   ENDSR
