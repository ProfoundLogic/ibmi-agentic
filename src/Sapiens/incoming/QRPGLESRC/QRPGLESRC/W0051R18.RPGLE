     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H DFTACTGRP(*no) ACTGRP(*caller)
     H/define CONVERT
     H*****************************************************************
     H*/TITLE MIDTERM ENDORSEMENT - FEDERAL ID CHANGE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/20/13 C:287 Project: CI126                                 *
     H*                Add division to CIEMPCHG program call          *
     H*                                                               *
     H* 12/27/11 C:282 Project: WC2005                                *
     H*                Add API WA085R to change EFT payee name/FEIN   *
     H*                                                               *
     H* 05/23/08 C:33  Project: 132DKH                                *
     H*                For name and/or FEIN changes report them to    *
     H*                 CiCenter.                                     *
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*                                                               *
     H* 06/23/99 C:113 PROJECT: SUPPORT                               *
     H*                Load fields like name change.                  *
     H*                                                               *
     H* 05/11/99 C:113 PROJECT: CM027                                 *
     H*                If the employer has an additional location,    *
     H*                update WMALP with the new federal ID.          *
     H*                                                               *
     H* 11/23/98  C:35   PROJECT: SUPPORT WA090 15125                 *
     H*                  INITIALIZE POLICY EFFECTIVE DATE IN THE      *
     H*                  ENDORSEMENT RECORDS FROM THE EMPLOYER DETAIL *
     H*                  FILE.                                        *
     H*                                                               *
     H* 10/14/98  C:35   PROJECT: SUPPORT WA091 13352                 *
     H*                  INITIALIZE CARRIER NUMBER IN THE ENDORSEMENT *
     H*                  RECORDS FROM THE VALUE IN FUND AUXILIARY.    *
     H*                                                               *
     H*****************************************************************
     FW0051F18  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMEMP     UF   E             DISK
     F                                     RENAME(PEM01:DBRCDF)
     F***  Begin Add  ***  05/23/08  **************************************
     F                                     INFDS(DBFDS)
     F***  End   Add  ***  05/23/08  **************************************
     FWDELP     IF   E             DISK
     FWTMYP     IF   E             DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMDXL     IF   E           K DISK
     FWMEXL     IF   E           K DISK
     FWMENL3    IF A E           K DISK
     F**** BEGIN ADD **** 10/14/98 *********************************************
     FWDF2L     IF   E           K DISK
     F**** END   ADD **** 10/14/98 *********************************************
     F*
     F*** BEGIN ADD *** 05/11/99 **************************************
     FWMALL     UF   E           K DISK
     F*** END   ADD *** 05/11/99 **************************************
      /COPY SCOPYMT01D
     D*================================================================
     D*  Array and Table Definitions
     D*================================================================
     D*================================================================
     D*  Data Structure and Constant Definitions
     D*================================================================
     D***PY SCOPYMT01I
     D*
     D***  Begin Add  ***  05/23/08  **************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D***  End   Add  ***  05/23/08  **************************************
     D*
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D*
     D                 DS
     D  XXFEIN                 1      9
     D  XXFED2                 1      2
     D  XXFED7                 3      9
     D*
     D                 DS                  INZ
     D  ED                     1     35  0
     D                                     DIM(5)
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D*
     D                 DS                  INZ
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Employer Endorsement-
     D                                     s - Federal ID Chang-
     D                                     es')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D CMPRES          C                   CONST('WA005R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          PEM01 RRN
     C                   PARM                    @@RRN1            9 0          PEL01 RRN
     C                   PARM                    @@RRN2            9 0          PMY01 RRN
     C                   PARM                    @@EEDT            8 0          MMDDCCYY
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##FYR
     C     SCAGT#        PARM                    ##AGT#
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD
     C                   PARM                    ##PNAM           60
     C*
     C*  Compress the employer header information parameter list
     C*
     C     CMPRPL        PLIST
     C                   PARM      EMEMP#        ##EMP#
     C                   PARM      EMDIV         ##DIV
     C                   PARM      EMNAME        ##NAME           40
     C                   PARM                    ##EDN            80
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     ELKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C                   KFLD                    ##FYR
     C*
     C*  COMPANY MASTER
     C*
     C     COKY          KLIST
     C                   KFLD                    ##CO#
     C*
     C*  ENDORS/DARTS CONTROL DEFINITION
     C*
     C     DXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  ADMIN/DARTS CONTROL DEFINITION
     C*
     C     EXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  FUND MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
      **** BEGIN ADD **** 10/14/98 *********************************************
      *
      * KEY LIST FOR THE GROUP AUXILIARY FILE
      *
     C     F2KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
      **** END   ADD **** 10/14/98 *********************************************
     C*
     C*  ENDORSEMENT REQUEST WORKFILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C                   KFLD                    ##FYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
     C*** BEGIN ADD *** 05/11/99 **************************************
     C     ALKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C*** END   ADD *** 05/11/99 **************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    ELCO#         ##CO#
     C     *LIKE         DEFINE    ELFND         ##FND
     C     *LIKE         DEFINE    ELEMP#        ##EMP#
     C     *LIKE         DEFINE    ELDIV         ##DIV
     C     *LIKE         DEFINE    ELFYR         ##FYR
     C     *LIKE         DEFINE    SCAGT#        ##AGT#
     C     *LIKE         DEFINE    SCPROD        ##PROD
     C     *LIKE         DEFINE    EMFEIN        SVFEIN
     C*
     C/COPY SCOPYMT01C
     C*
     C***  Begin Add  ***  05/23/08  **************************************
     C*====================================================================
     C* Update CiCenter with changes
     C*====================================================================
     C     CICNTR        BEGSR
     C*
     C     *LIKE         DEFINE    DBRRN         ##RRN
     C     *LIKE         DEFINE    SVFEIN        ##OFID
     C     *LIKE         DEFINE    SVDBA         ##ODBA
     C*** Begin Add ** 08/20/13 ****************************
     C     *LIKE         DEFINE    EMDIV         SVDIV
     C     *LIKE         DEFINE    SVDIV         ##ODIV
     C*** End   Add ** 08/20/13 ****************************
     C*
     C                   CLEAR                   #2TYPE
     C*
     C                   SELECT
     C     SVFEIN        WHENNE    EMFEIN
     C                   MOVEL     'FEIN'        #2TYPE
     C                   ENDSL
     C*
     C     #2TYPE        IFNE      *BLANK
     C                   CALL      'CIEMPCHG'                           99
     C                   PARM      DBRRN         ##RRN
     C                   PARM                    #2TYPE           10
     C                   PARM      SVFEIN        ##OFID
     C                   PARM      SVDBA         ##ODBA
     C*** Begin Add ** 08/20/13 ****************************
     C                   PARM      SVDIV         ##ODIV
     C*** End   Add ** 08/20/13 ****************************
     C                   ENDIF
     C*
     C                   MOVE      EMFEIN        SVFEIN
     C*** Begin Add ** 08/20/13 ****************************
     C                   MOVE      EMDIV         SVDIV
     C*** End   Add ** 08/20/13 ****************************
     C*
     C                   ENDSR
     C***  End   Add  ***  05/23/08  **************************************
     C*
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
     C     EXFIFG        IFEQ      'Y'
     C     SCFEIN        ANDEQ     *ZEROS
     C                   SETON                                        71  90
     C                   ENDIF                                                  EXFIFG IFEQ 'Y'
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90EMDT          IFGT      SAVEDT
     C     EMDT          OREQ      SAVEDT
     C     EMTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
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
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10 (SEARCHES)
     C*===============================================================
     C     KJDO          BEGSR
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     EMFEIN        SCFEIN
     C                   MOVEL     EMFEIN        SVFEIN
     C*
     C***  Begin Add  ***  05/23/08  **************************************
     C     EMNAME        CAT(P)    EMNAM2        SVDBA           100
     C***  End   Add  ***  05/23/08  **************************************
     C*** Begin Add ** 08/20/13 ****************************
     C                   MOVE      EMDIV         SVDIV
     C*** End   Add ** 08/20/13 ****************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C     SCFEIN        IFNE      SVFEIN
     C                   MOVEL     SCFEIN        EMFEIN
     C                   MOVEL     SCFEIN        XXFEIN
     C     XXFED2        CAT(P)    '-':0         XXAF01           50
     C                   CAT       XXFED7:0      XXAF01
     C                   MOVEL     SVFEIN        XXFEIN
     C     XXFED2        CAT(P)    '-':0         XXB401           50
     C                   CAT       XXFED7:0      XXB401
     C                   EXSR      WRTPEN
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
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
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
     C                   MOVE      *ON           FIRST             1
     C                   MOVEL     'C'           @@MODE            1
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   MOVE      @@EEDT        SCEEDT
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C*  LOAD SCREEN HEADINGS
     C*
     C*  GET EMPLOYER DETAIL RECORD
     C*
     C     @@RRN1        CHAIN     PEL01                              60
     C*
     C                   MOVE      ELCO#         ##CO#
     C                   MOVE      ELFND         ##FND
     C                   MOVE      ELFND         SCFND
     C                   MOVE      ELFYR         ##FYR
     C                   MOVE      ELEMP#        ##EMP#
     C                   MOVE      ELDIV         ##DIV
     C*
     C                   MOVEL     *BLANK        ##CONM
     C     COKY          CHAIN     PCO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        ##CONM
     C                   ENDIF
     C*
     C     FNKY          CHAIN     PFN01                              99
      **** BEGIN ADD **** 10/14/98 *********************************************
     C     F2KY          CHAIN     PF201                              99
      **** END   ADD **** 10/14/98 *********************************************
     C                   MOVEL     FNNAME        ##FNAM
     C*
     C                   MOVEL     ##CONM        SCCONM
     C                   MOVEL     ##FNAM        SCNAME
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             60
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      CMPRES        CMPRPL
     C*
     C                   MOVEL     ##EDN         SCEDN
     C*
     C                   Z-ADD     1             I1                1 0
     C     *ZERO         LOOKUP    ED(I1)                                 60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     5             I1
     C                   ELSE
     C                   SUB       1             I1
     C                   ENDIF
     C*
     C     I1            IFEQ      *ZERO
     C     CX(I1)        OREQ      *ZERO
     C                   MOVE      *OFF          *IN58                          ACTIVE
     C                   ELSE
     C                   MOVE      *ON           *IN58                          CANCELLED
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   ENDIF
     C*
     C*  GET AGENT/PRODUCER INFORMATION
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C*  GET ENDORS/DARTS INFORMATION
     C*
     C     DXKY          CHAIN     PDX01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PDX01
     C                   ENDIF
     C*
     C*  GET ADMIN/DARTS INFORMATION
     C*
     C     EXKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PEX01
     C                   ENDIF
     C     EXFIFG        IFEQ      'Y'
     C                   MOVE      *ON           *IN51
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C*
     C                   MOVEL     DSTIME        EMTIME
     C*
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C*
     C*** BEGIN ADD ***  12/27/11  ****************************************
     C     EMFEIN        IFNE      SVFEIN
     C                   CALL      'WA085R'
     C                   PARM      'E'           XXTYPE            1
     C                   PARM      DBRRN         XXRRN             9 0
     C                   PARM      *BLANK        XXRTN             1
     C                   ENDIF
     C*** END ADD *****  12/27/11  ****************************************
     C***  Begin Add  ***  05/23/08  **************************************
     C                   EXSR      CICNTR
     C***  End   Add  ***  05/23/08  **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   ENDSR
     C*================================================================
     C* WRTPEN - Write data base record to Endorsement Print Workfile
     C*================================================================
     C     WRTPEN        BEGSR
     C*
     C     FIRST         IFEQ      *ON
     C                   MOVE      *OFF          FIRST
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C                   ENDIF
     C*
     C                   MOVE      ##CO#         ENCO#
     C                   MOVE      ##FND         ENFND
     C                   MOVE      ##EMP#        ENEMP#
     C                   MOVE      ##DIV         ENDIV
     C                   MOVE      ##FYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
     C                   Z-ADD     0             ENSEQ#
     C*
     C                   MOVE      @@EEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
     C*
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C*** BEGIN ADD ***  03/07/05  ************************************
     C*
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2
     C*
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2
     C*
     C*** END ADD *****  03/07/05  ************************************
     C                   MOVEL     EMCONT        ENCON1
     C                   MOVEL     EMATTN        ENATT1
     C                   MOVEL     EMCNT3        ENCN31
     C                   MOVE      SVFEIN        ENFEN1
     C                   MOVEL     EMTPOB        ENTOB1
     C                   MOVEL     EMMAD1        ENMA11
     C                   MOVEL     EMMAD2        ENMA21
     C                   MOVEL     EMMCTY        ENMCT1
     C                   MOVEL     EMMST         ENMST1
     C                   MOVEL     EMMZIP        ENMZP1
     C                   MOVEL     EMPAD1        ENPA11
     C                   MOVEL     EMPAD2        ENPA21
     C                   MOVEL     EMPCTY        ENPCT1
     C                   MOVEL     EMPST         ENPST1
     C                   MOVEL     EMPZIP        ENPZP1
     C*
      *** BEGIN ADD *** 06/23/99 ***********************************************
     C                   MOVEL     EMNAME        ENNAM2
     C                   MOVEL     EMFDNM        ENFDN2
     C*** BEGIN ADD ***  03/07/05  ************************************
     C*
     C                   MOVEL     EMNAME        ENANM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENANM1
     C                   MOVE      EMNAM2        ENANM2
     C*
     C                   MOVEL     EMFDNM        ENAFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENAFN1
     C                   MOVE      EMFDN2        ENAFN2
     C*
     C*** END ADD *****  03/07/05  ************************************
     C                   MOVEL     EMCONT        ENCON2
     C                   MOVEL     EMATTN        ENATT2
     C                   MOVEL     EMCNT3        ENCN32
     C                   MOVEL     EMTPOB        ENTOB2
      *** END   ADD *** 06/23/99 ***********************************************
     C                   MOVE      SCFEIN        ENFEN2
     C                   MOVE      'Y'           ENFIFL
     C                   MOVEL     DXEMFI        ENRMCD
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
     C*** BEGIN ADD *** 05/11/99 **************************************
     C     ALKY          CHAIN     PAL01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     ALELTP        IFEQ      'L'
     C*
     C                   MOVE      EMFEIN        ALFEIN
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALDT
     C*
     C                   MOVEL     DSTIME        ALTIME
     C                   MOVEL     Q1USER        ALUSER
     C                   MOVEL     Q1PGM         ALPGM
     C                   MOVEL     Q1USER        ALUSER
     C*
     C                   UPDATE    PAL01
     C*
     C                   ENDIF                                                  ALELTP = 'L'
     C*
     C     ALKY          READE     PAL01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C*** END   ADD *** 05/11/99 **************************************
     C*
     C     @@RRN2        CHAIN     PMY01                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
     C*
     C                   MOVEL     XXB401        ENBLIN
     C                   MOVEL     XXAF01        ENALIN
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
     C*
     C                   MOVEL     DSTIME        ENTIME
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
      **** BEGIN ADD **** 10/14/98 *********************************************
     C                   MOVE      F2CAR#        ENCAR1
      **** END   ADD **** 10/14/98 *********************************************
      **** BEGIN ADD **** 11/23/98 *********************************************
     C                   Z-ADD     ELPFDT        ENPFD1
      **** END   ADD **** 11/23/98 *********************************************
     C*
     C                   WRITE     PEN01
     C*
     C                   ENDSR
