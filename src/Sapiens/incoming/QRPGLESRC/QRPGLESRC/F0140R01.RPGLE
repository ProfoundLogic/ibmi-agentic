     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0140R01 - EMPLOYER MASTER INFORMATION  C:62 02/18/98        *
     H*                                                               *
     H*  CALLED FROM: F0140R                                          *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/19/00 C:62 SUPPORT: MC065 19768                           *
     H*                SWAP OUT USAGE OF STABL TO STSTL3, SO THAT UC  *
     H*                BECOMES INVALID                                *
     H*                                                               *
     H*  04/12/00 C:53 PROJECT: MC189                                 *
     H*                ADD PRE DELETE PROCESS TO DELETE SECONDARY     *
     H*                FILES AND INACTIVATE DARTS RECORDS             *
     H*                                                               *
     H*  12/01/98 C:94 PROJECT: MC145                                 *
     H*                ADDED OPTION 14=DARTS TO PROGRAM               *
     H*                                                               *
     H*  10/07/98 C:94 PROJECT: 87MFX                                 *
     H*                CHANGE F12=PREVIOUS TO F12=CANCEL              *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*  POSITION CURSORS IN KJDO:  30 - 34.                          *
     H*                                                               *
     H*  ERROR INDICATORS IN ERRCK1: 71 - 78.                         *
     H*                                                               *
     H*****************************************************************
     FF0140F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMEMP     UF A E             DISK
     F                                     RENAME(PEM01:DBRCDF)
     F                                     INFDS(DBFDS)
     FFMEML     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FSTSTP     IF   E             DISK
     F*** BEGIN CHG *** 06/19/00 **************************************
     F**********STABL   IF  E           K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END   CHG *** 06/19/00 **************************************
     F                                     RENAME(PST01:PST02)
     FFMFNP     IF   E             DISK
     FFMF1L     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Employer Master -
     D                                     Information')
     D COMAPI          C                   CONST('FA007R')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FMEMP
     C                   PARM                    @@RRN2            9 0          FMFNP
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C*  State code search parm list
     C*
     C     STCPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C* Network code search parm list.
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     SMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C     F1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EMCO#         KKCO#
     C     *LIKE         DEFINE    EMSUB#        KKSUB#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C     *LIKE         DEFINE    STKEY         KKKEY
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     EMSTAT        IFEQ      'I'
     C                   MOVE      'A'           EMSTAT
     C                   ELSE
     C                   MOVE      'I'           EMSTAT
     C                   ENDIF                                                  EMSTAT
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
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C*** BEG ADD *** 04/12/00 ******************************
     C                   CALL      'FA029R'
     C                   PARM      *BLANKS       @@RTCD            1
     C                   PARM      *ZEROS        @@CO#             3
     C                   PARM      *ZEROS        @@SUB#            5
     C                   PARM      *ZEROS        @@FND             3
     C                   PARM      *ZEROS        @@EMP#            9
     C                   PARM      *ZEROS        @@DIV             5
     C                   PARM                    @@RRN
     C*** END ADD *** 04/12/00 ******************************
     C*
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   MOVEA     '00000'       *IN(30)
     C*
     C* ONLY PROCESS THESE EDITS IF STANDALONE.
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C* EMPLOYER # MUST NOT EQUAL ZEROS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCEMP#        IFEQ      *ZEROS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCEMP#
     C*
     C* CHECK TO BE SURE GROUP NUMBER HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVE      SCEMP#        KKEMP#
     C                   MOVE      SCDIV         KKDIV
     C*
     C   40
     CANN90EMKY          CHAIN(N)  PEM01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* EMPLOYER NAME CANNOT BE BLANK
     C*
     C  N90SCNAME        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C* DOING BUSINESS AS CANNOT BE BLANK
     C*
     C  N90SCDBA         IFEQ      *BLANKS
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCDESC
     C*
     C* MAILING STATE CODE IF NOT BLANK MUST BE VALID
     C*
     C     SCMST         IFNE      *BLANKS
     C     *IN90         ANDEQ     *OFF
     C*
     C                   MOVEL     SCMST         KKKEY
     C*
     C     STKY          CHAIN     PST02                              99
     C*
     C   99              SETON                                        75  90
     C*
     C                   ENDIF
     C*
     C* PHYSICAL STATE CODE IF NOT BLANK MUST BE VALID
     C*
     C     SCPST         IFNE      *BLANKS
     C     *IN90         ANDEQ     *OFF
     C*
     C                   MOVEL     SCPST         KKKEY
     C*
     C     STKY          CHAIN     PST02                              99
     C*
     C   99              SETON                                        76  90
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN16
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90EMDT          IFGT      SAVEDT
     C     EMDT          OREQ      SAVEDT
     C     EMTIME        ANDGT     SAVETM
     C                   SETON                                        9089
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Searches.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   MOVEA     '00000'       *IN(30)
     C*
     C     HFLD          IFEQ      'SCMST'
     C     HFLD          OREQ      'SCPST'
     C*
     C                   CALL      'SY601R'      STCPL
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCMST'
     C                   SETON                                        30
     C     HFLD          WHENEQ    'SCPST'
     C                   SETON                                        32
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PST01                              99
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCMST'
     C                   MOVEL     STKEY         SCMST
     C                   SETON                                        31
     C     HFLD          WHENEQ    'SCPST'
     C                   MOVEL     STKEY         SCPST
     C                   SETON                                        33
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** BEG ADD *** 12/01/98 ***************************************
     C*================================================================
     C* KNDO - F14 (DARTS)
     C*================================================================
     C     KNDO          BEGSR
     C*
     C                   MOVE      ##CO#         #@CO#
     C                   MOVE      SCSUB#        #@SUB#
     C                   MOVE      SCFND         #@FND
     C                   MOVE      SCEMP#        #@EMP#
     C                   MOVE      SCDIV         #@DIV
     C*
     C                   CALL      'FA719R'
     C                   PARM      *BLANKS       #@RTCD            1
     C                   PARM      ##WCIN        #@WCIN            1
     C                   PARM      ##WCDK        #@WDCK            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@SUB#            5
     C                   PARM                    #@FND             3
     C                   PARM                    #@EMP#            9
     C                   PARM                    #@DIV             5
     C                   PARM      *BLANKS       #@KEY            40
     C*
     C                   ENDSR
     C*** END ADD *** 12/01/98 ***************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      FNFND         SCFND
     C                   MOVEL     FNNAME        SCGNAM
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVEL     SMNAME        SCSNAM
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C* ASSIGN VALUES TO KEY FIELDS
     C*
     C                   MOVE      EMEMP#        KKEMP#
     C                   MOVE      EMDIV         KKDIV
     C*
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C*
     C                   MOVE      EMNAME        SCNAME
     C                   MOVE      EMDBA         SCDBA
     C*
     C                   MOVEL     EMMAD1        SCMAD1
     C                   MOVEL     EMMAD2        SCMAD2
     C                   MOVEL     EMMCTY        SCMCTY
     C                   MOVEL     EMMST         SCMST
     C                   MOVEL     EMMZIP        SCMZPA
     C                   MOVE      EMMZIP        SCMZPB
     C*
     C                   MOVEL     EMPAD1        SCPAD1
     C                   MOVEL     EMPAD2        SCPAD2
     C                   MOVEL     EMPCTY        SCPCTY
     C                   MOVEL     EMPST         SCPST
     C                   MOVEL     EMPZIP        SCPZPA
     C                   MOVE      EMPZIP        SCPZPB
     C*
     C                   MOVE      EMAREA        SCAREA
     C                   MOVEL     EMPHN#        SCPHNA
     C                   MOVE      EMPHN#        SCPHNB
     C*
     C                   MOVE      EMFARA        SCFARA
     C                   MOVEL     EMFAX#        SCFAXA
     C                   MOVE      EMFAX#        SCFAXB
     C*
     C                   MOVE      EMFEIN        SCFEIN
     C                   MOVEL     EMNOB         SCNOB
     C*
     C                   MOVEL     EMSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C* SCR01 VARIABLES
     C*
     C                   MOVE      *ZEROS        SCEMP#
     C                   MOVE      *ZEROS        SCDIV
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVE      *BLANKS       SCDBA
     C                   MOVE      *BLANKS       SCMAD1
     C                   MOVE      *BLANKS       SCMAD2
     C                   MOVE      *BLANKS       SCMCTY
     C                   MOVE      *BLANKS       SCMST
     C                   MOVE      *ZEROS        SCMZPA
     C                   MOVE      *ZEROS        SCMZPB
     C                   MOVE      *BLANKS       SCPAD1
     C                   MOVE      *BLANKS       SCPAD2
     C                   MOVE      *BLANKS       SCPCTY
     C                   MOVE      *BLANKS       SCPST
     C                   MOVE      *ZEROS        SCPZPA
     C                   MOVE      *ZEROS        SCPZPB
     C                   MOVE      *ZEROS        SCAREA
     C                   MOVE      *ZEROS        SCPHNA
     C                   MOVE      *ZEROS        SCPHNB
     C                   MOVE      *ZEROS        SCFARA
     C                   MOVE      *ZEROS        SCFAXA
     C                   MOVE      *ZEROS        SCFAXB
     C                   MOVE      *ZEROS        SCFEIN
     C                   MOVEL     *BLANKS       SCNOB
     C                   MOVEL     'A'           SCSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C* SCR01 VARIABLES
     C*
     C                   MOVE      FNCO#         EMCO#
     C                   MOVE      SCSUB#        EMSUB#
     C                   MOVE      SCFND         EMFND
     C                   MOVE      SCEMP#        EMEMP#
     C                   MOVE      SCDIV         EMDIV
     C                   MOVEL     SCNAME        EMNAME
     C                   MOVEL     SCDBA         EMDBA
     C*
     C                   MOVEL     SCMAD1        EMMAD1
     C                   MOVEL     SCMAD2        EMMAD2
     C                   MOVEL     SCMCTY        EMMCTY
     C                   MOVEL     SCMST         EMMST
     C                   MOVEL     SCMZPA        EMMZIP
     C                   MOVE      SCMZPB        EMMZIP
     C*
     C                   MOVEL     SCPAD1        EMPAD1
     C                   MOVEL     SCPAD2        EMPAD2
     C                   MOVEL     SCPCTY        EMPCTY
     C                   MOVEL     SCPST         EMPST
     C                   MOVEL     SCPZPA        EMPZIP
     C                   MOVE      SCPZPB        EMPZIP
     C*
     C                   MOVE      SCAREA        EMAREA
     C                   MOVEL     SCPHNA        EMPHN#
     C                   MOVE      SCPHNB        EMPHN#
     C*
     C                   MOVE      SCFARA        EMFARA
     C                   MOVEL     SCFAXA        EMFAX#
     C                   MOVE      SCFAXB        EMFAX#
     C*
     C                   MOVE      SCFEIN        EMFEIN
     C                   MOVEL     SCNOB         EMNOB
     C*
     C                   MOVEL     SCSTAT        EMSTAT
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
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C*** BEG ADD *** 12/01/98 ****************************************
     C     *INKN         CASEQ     *ON           KNDO
     C*** END ADD *** 12/01/98 ****************************************
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C*** GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*
     C                   Z-ADD     1             #OFSCR            1 0
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C*** MOVE KEY FIELDS FROM FMFNP.
     C*
     C     @@RRN2        CHAIN     PFN01                              99
     C*
     C                   MOVE      FNCO#         KKCO#
     C                   MOVE      FNSUB#        KKSUB#
     C                   MOVE      FNFND         KKFND
     C*
     C     F1KY          CHAIN     PF101                              99
     C*
     C                   MOVE      FNCO#         #1CO#
     C                   MOVE      FNSUB#        #1SUB#
     C                   MOVE      FNFND         #1FND
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM      *BLANK        ##OUTS            1
     C*
     C     ##OUTS        IFEQ      'N'
     C*
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   ENDIF
     C*
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C*
     C                   MOVEL     DSTIME        EMTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMPGM
     C                   MOVEL     Q1PGM         EMIPGM
     C                   MOVEL     Q1USER        EMIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C*
     C                   MOVEL     SRDTO         EMIDT
     C*
     C                   MOVEL     DSTIME        EMTIME
     C                   MOVEL     DSTIME        EMITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FMFNP
     C*
     C                   ENDSR
