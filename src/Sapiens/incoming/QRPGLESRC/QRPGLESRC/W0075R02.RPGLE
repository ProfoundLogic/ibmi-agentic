     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0075R02 - WORK WITH ENTITY/LOCATION DETAIL MAINTENANCE
     H*****************************************************************
     H*  CALLED FROM: W0076R - W W ENTITY/LOCATIONS DETAIL            *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 02/05/99  PROGRAMMER: C:87  PROJECT: WC878          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/10/07  C:33  Seq#: WA099/58336                             *
     H*                 Load SCBDES when returning from search.       *
     H*                                                               *
     H* 04/17/07 C:190 Project 71WGW
     H*                 Add new field 'max # of employees per shift'
     H*
     H* 02/01/2007 c:62 Project WA099 56863                           *
     H*                 Compile only.                                 *
     H*                                                               *
     H* 06/01/06 C:168 PROJECT 129WNX                                 *
     H*                Added handle for field Total Payroll           *
     H*                                                               *
     H* 10/05/04 C:132  PROJECT:  132WBF                              *
     H*                 Add new field for Type of Building code and   *
     H*                 edit check on ADD mode for existing code.     *
     H*                 ADD F10-Search on new Type of Building field. *
     H*                                                               *
     H* 03/21/03 C:61  PROJECT 129WAJ                                 *
     H*                ADD mode processing different if called from   *
     H*                Mass Audit option.                             *
     H*                                                               *
     H* 10/03/02 C:61  SUPPORT WA099 33653                            *
     H*                Do not allow entry of policy year for location *
     H*                if policy year does not exist for employer.    *
     H*                                                               *
     H*****************************************************************
     H*
     FW0075F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWDLDP     UF A E             DISK
     F                                     RENAME(PLD01:DBRCDF)
     F*
     FWDLDL     IF   E           K DISK
     F*
     F*  Entity/Location file
     FWMALP     IF   E             DISK
     F*
     F*  Fund master file
     FWMFNL     IF   E           K DISK
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
      ***** BEG ADD ******* 10/03/02 **********************************
      *
      *  Employer Policy year
     FWDELL     IF   E           K DISK
      ***** END ADD ******* 10/03/02 **********************************
      ***** BEG ADD ******* 03/21/03 **********************************
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)
      ***** END ADD ******* 03/21/03 **********************************
      ***** BEG ADD ******* 10/05/04 **********************************
      *
      *  Employer Policy year
     FWTTBL     IF   E           K DISK
      ***** END ADD ******* 10/05/04 **********************************
      *
      *
      *
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Entity/Location Deta-
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

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          wdldp
     C                   PARM                    @@RRN2            9 0          wmalp
      ***** BEG ADD ******* 03/21/03 **********************************
     C                   PARM                    @@RRN3            9 0          WDELP
      ***** END ADD ******* 03/21/03 **********************************
     C*
     C*  Note: @@RRN is the RRN of WDLDP
     C*        @@RRN2 is the RRN of WMALP
     C*        @@RRN3 is the RRN of WDELP (OPTIONAL)
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
      ***** BEG ADD ******* 10/03/02 **********************************
      *
      *  Employer Policy Year
      *
     C     ELKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ALFND
     C                   KFLD                    ALEMP#
     C                   KFLD                    ALDIV
     C                   KFLD                    SCFYR
      *
      ***** END ADD ******* 10/03/02 **********************************
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
     C*  Additional Entity/Location detail file
     C*
     C     LDKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCELID
     C                   KFLD                    SCFYR
     C*
      ***** BEG ADD ******* 10/05/04 **********************************
     C*
     C*  Type of Building Code file
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCTYPB
      ***** END ADD ******* 10/05/04 **********************************
     C*
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    EMEMP#        QQEMP#
     C     *LIKE         DEFINE    EMDIV         QQDIV
     C     *LIKE         DEFINE    EMNAME        QQNAME
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C*
     C/COPY SCOPYMT01C
     C*
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
      *
      *
      *
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  Validate policy year
     C  N90SCFYR         IFLE      0
     C                   SETON                                        72  90
     C                   ENDIF
      ***** BEG ADD ******* 10/03/02 **********************************
      *
      *  Entered policy year must exist for employer.
      *
     C  N90*IN40         IFEQ      *ON
     C     ELKY          CHAIN     PEL01                              60
     C   60              SETON                                        75  90
     C                   ENDIF
      *
      ***** END ADD ******* 10/03/02 **********************************
      ***** BEG ADD ******* 10/05/04 **********************************
      *
      *  Entered type of building code must NOT exist for ADD, and
      *  must exist on CHANGE if entered.
      *
     C  N90*IN40         IFEQ      *ON
     C     SCTYPB        ANDNE     *BLANKS
     C     TBKY          CHAIN     PTB01                              60
     C   60              SETON                                        79  90
     C                   ENDIF
      *
     C  N90*IN40         IFEQ      *OFF
     C     *IN09         ANDEQ     *OFF
     C     SCTYPB        ANDNE     *BLANKS
     C     TBKY          CHAIN     PTB01                              60
     C   60              SETON                                        79  90
     C                   ENDIF
      ***** END ADD ******* 10/05/04 **********************************
     C*
     C*  Validate nmber of employers
     C  N90SCEMPY        IFLE      0
     C                   SETON                                        73  90
     C                   ENDIF
     C*
     C*  If in add mode and a record already exists display error
     C  N90*IN40         IFEQ      *ON
     C*
     C     LDKY          CHAIN(N)  PLD01                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        74  90
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN40=*ON
      *
     C***** BEG ADD *******  04/17/07 *********************************
     C*  Check that maximum # of employees per shift > 0
     C*
     C  N90
     CAN 92SCM#EM        IFLT      0
     C                   SETON                                        93  90
     C                   ENDIF
     C*
     C*  If in add mode and a record already exists display error
     C  N90*IN40         IFEQ      *ON
     C*
     C     LDKY          CHAIN(N)  PLD01                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        98  90
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN40=*ON
     C***** END ADD *******  04/17/07 *********************************
     C*
      ***** BEG  ADD   06/01/06 ***************************************
     C*
     C*  Validate - Total Payroll
     C  N90SCTPYR        IFLT      0
     C                   SETON                                        78  90
     C                   ENDIF
      ***** END  ADD   06/01/06 ***************************************
     C*
     C*  Check to be sure record has not been updated since entry
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90LDDT          IFGT      SAVEDT
     C     LDDT          OREQ      SAVEDT
     C     LDTIME        ANDGT     SAVETM
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
      ***** BEG ADD ******* 10/05/04 **********************************
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
      ***** BEG ADD ******* 04/17/07 **********************************
     C     CLTID         IFEQ      '071'
     C                   MOVE      *ON           *IN92
     C                   ELSE
     C                   MOVE      *OFF          *IN92
     C                   ENDIF
      ***** END ADD ******* 04/17/07 **********************************
      *
     C     HFLD          IFEQ      'SCTYPB'
      *
     C                   MOVE      *BLANKS       @@KEY
      *
     C                   CALL      'W2275R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@KEY             3
     C     @@KEY         IFNE      *BLANKS
     C                   MOVEL     @@KEY         SCTYPB
     C***  Begin Add  ***  05/10/07  **************************************
     C                   CLEAR                   TBDESC
     C     TBKY          CHAIN     PTB01                              60
     C                   MOVEL(P)  TBDESC        SCBDES
     C***  End   Add  ***  05/10/07  **************************************
     C                   ELSE
     C                   MOVE      *ON           *IN31
      ***** BEG ADD ******* 04/17/07 **********************************
     C     CLTID         IFEQ      '071'
     C                   MOVE      *ON           *IN92
     C                   ELSE
     C                   MOVE      *OFF          *IN92
     C                   ENDIF
      ***** END ADD ******* 04/17/07 **********************************
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDSR
      ***** END ADD ******* 10/05/04 **********************************
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
     C                   Z-ADD     LDFYR         SCFYR
      ***** BEG ADD ******* 03/21/03 **********************************
      *
      * Initilize Policy Year entry if ADD mode and called from
      *  Mass Audit
      *
     C     Q1PRMS        IFGE      6
     C     @@MODE        ANDEQ     'A'
      *
     C     @@RRN3        IFNE      *ZERO
     C     @@RRN3        CHAIN     PEL02                              60
     C  N60              Z-ADD     ELFYR         SCFYR
     C  N60              SETON                                        76
     C                   ENDIF                                                  @@RRN3 NE 0
      *
     C                   ENDIF                                                  Q1PRMS GE 6
      *
      ***** END ADD ******* 03/21/03 **********************************
     C                   Z-ADD     LDEMPY        SCEMPY
      *
      ***** BEG ADD ******* 04/17/07 **********************************
     C                   Z-ADD     LDM#EM        SCM#EM
      ***** END ADD ******* 04/17/07 **********************************
      ***** BEG ADD ******* 06/01/06 **********************************
     C                   Z-ADD     LDTPYR        SCTPYR
      ***** END ADD ******* 06/01/06 **********************************
      ***** BEG ADD ***** 10/05/04 ***********************************
     C                   MOVE      LDTYPB        SCTYPB
      *
      * If not ADD & Type of Building Code not blank, load description
     C     *IN40         IFEQ      *OFF
     C     SCTYPB        ANDNE     *BLANKS
     C     TBKY          CHAIN     PTB01                              60
     C  N60              MOVEL(P)  TBDESC        SCBDES                         TYPE BLDG DESC
     C                   ELSE
     C                   MOVE      *BLANKS       SCBDES
     C                   ENDIF
      ***** END ADD ***** 10/05/04 ***********************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCFYR         LDFYR
     C                   Z-ADD     SCEMPY        LDEMPY
      ***** BEG ADD ******* 04/17/07 **********************************
     C                   Z-ADD     SCM#EM        LDM#EM
      ***** END ADD ******* 04/17/07 **********************************
      ***** BEG ADD ******* 06/01/06 **********************************
     C                   Z-ADD     SCTPYR        LDTPYR
      ***** END ADD ******* 06/01/06 **********************************
     C*
     C                   Z-ADD     ##CO#         LDCO#
     C                   Z-ADD     SCFND         LDFND
     C                   Z-ADD     SCEMP#        LDEMP#
     C                   Z-ADD     SCDIV         LDDIV
     C                   MOVE      SCELTP        LDELTP
     C                   MOVE      SCELID        LDELID
      ***** BEG ADD ***** 10/05/04 ***********************************
     C                   MOVE      SCTYPB        LDTYPB
      ***** END ADD ***** 10/05/04 ***********************************
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
      ***** BEG ADD ******* 10/05/04 **********************************
     C     *INKJ         CASEQ     *ON           KJDO                     90
      ***** END ADD ******* 10/05/04 **********************************
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   SETON                                        LR
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
     C     @@RRN2        CHAIN     WMALP                              60
     C*
     C*     Get fund info
     C                   Z-ADD     ALFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C                   Z-ADD     ALEMP#        SCEMP#
     C                   Z-ADD     ALDIV         SCDIV
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
     C                   MOVE      ALELTP        SCELTP
     C                   MOVE      ALELID        SCELID
     C                   Z-ADD     ALEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C*** BEG ADD **** 04/17/07 **********************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C     CLTID         IFEQ      '071'
     C                   MOVE      *ON           *IN92
     C                   ELSE
     C                   MOVE      *OFF          *IN92
     C                   ENDIF
     C*** END ADD **** 04/17/07 **********************************************
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
     C                   MOVEL     Q1USER        LDUSER
     C                   MOVEL     Q1PGM         LDPGM
     C                   Z-ADD     DSTIME        LDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDDT
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
     C                   MOVEL     Q1USER        LDUSER
     C                   MOVEL     Q1PGM         LDPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDDT
     C                   MOVEL     DSTIME        LDTIME
     C*
     C                   MOVE      LDUSER        LDIUSR
     C                   MOVE      LDPGM         LDIPGM
     C                   MOVE      LDDT          LDIDT
     C                   MOVE      LDTIME        LDITM
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      WDLDP
      ***** BEG ADD ******* 03/21/03 **********************************
      *
      *  Do not prompt for additional entries
      *
     C                   MOVE      *ON           OUTFLG
      *
      ***** END ADD ******* 03/21/03 **********************************
     C                   ENDSR                                                  WRITER
