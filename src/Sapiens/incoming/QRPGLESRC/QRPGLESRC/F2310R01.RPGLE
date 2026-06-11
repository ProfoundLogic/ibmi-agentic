     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* Revisions:
     F*
     F* 09/17/10 C:257 PROJECT: MC345
     F*               Add SC92N2 for HCPCS used amount.
     H*
     H* 01/14/04 C:62 Project: 114MPI
     H*               1. Add call to SA001R.
     H*               1. Add Second Screen Processing.
     H*
     H* 03/02/01 C:79 Project: 114MBI
     H*               1. Add Field for Technical component(SC92N1)
     H*               2. Add Anesthesia flag(SC1A2)
     H*               3. Add Anesthesia conversion factor(SCCONF)
     H*
     H*****************************************************************
     FF2310F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMRCP     UF A E             DISK
     F                                     RENAME(PRC01:DBRCDF)
     FFMRCL     IF   E           K DISK
     FFMGZP     IF   E             DISK
     F*** BEGIN ADD *** 01/14/04 **************************************
     FFMR1L     UF A E           K DISK
     F*** END   ADD *** 01/14/04 **************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('R&C Code Inform-
     D                                     ation')
     D COMAPI          C                   CONST('FA007R')

     I*** BEGIN DEL *** 01/14/04 **************************************
     I**********    1                     C         #OFSCR
     I*** END   DEL *** 01/14/04 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Search parameter list.
     C*
     C     SCHPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     RCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*** BEGIN ADD *** 01/14/04 **************************************
     C*
     C     R1KY          KLIST
     C                   KFLD                    RCCO#
     C                   KFLD                    RCZIPA
     C                   KFLD                    RCCPT4
     C                   KFLD                    RCEFDT
     C*** END   ADD *** 01/14/04 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    RCCO#         ##CO#
     C     *LIKE         DEFINE    RCZIPA        KKZIPA
     C     *LIKE         DEFINE    RCCPT4        KKCPT4
     C     *LIKE         DEFINE    RCEFDT        KKEFDT
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
     C     RCSTAT        IFEQ      'I'
     C                   MOVE      'A'           RCSTAT
     C                   ELSE
     C                   MOVE      'I'           RCSTAT
     C                   ENDIF                                                  RCSTAT
     C*
     C                   MOVEL     Q1USER        RCUSER
     C                   MOVEL     Q1PGM         RCPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RCDT
     C*
     C                   MOVEL     DSTIME        RCTIME
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
     C* GEO ZIP AREA CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCZIPA        IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CPT-4 CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCCPT4        IFEQ      *BLANKS
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCKEY
     C*
     C* EFFECTIVE DATE MUST BE VALID (IF IN ADD MODE)
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C   40
     CANN90SRERR         IFEQ      '1'
     C*
     C                   SETON                                        73  90
     C*
     C                   ENDIF                                                  SRERR
     C*
     C* CHECK TO BE SURE BODY AREA CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCZIPA        KKZIPA
     C                   MOVEL     SCCPT4        KKCPT4
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C   40
     CANN90RCKY          CHAIN     PRC01                              99
     C   40
     CANN90
     CANN99              SETON                                        74  90
     C*
     C* DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDSC1        IFEQ      *BLANKS
     C                   SETON                                        75  90
     C                   ENDIF                                                  SCDESC
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        8490
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*
     C                   SETON                                        8390
     C*
     C                   ENDIF
     C*
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'
     C*
     C                   SETON                                        7890
     C*
     C                   ENDIF
     C*** BEG ADD *** 03/02/01 ****************************************
     C*  THE ANES FLAG MUST BE Y IF THERE IS AN ANESTHESIA AMOUNT
     C*
     C  N90SCANAM        IFNE      0
     C     SC1A2         ANDNE     'Y'
     C*
     C                   SETON                                        7690
     C*
     C                   ENDIF
     C*** END ADD *** 03/02/01 ****************************************
     C*
     C*  The Professional Component Flag is invalid
     C*
     C  N90SC1A1         IFNE      *BLANK
     C     SC1A1         ANDNE     'B'
     C     SC1A1         ANDNE     'I'
     C*
     C                   SETON                                        8290
     C*
     C                   ENDIF
     C*
     C*  THE SUPPLIES FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        8190
     C*
     C                   ENDIF
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90RCDT          IFGT      SAVEDT
     C     RCDT          OREQ      SAVEDT
     C     RCTIME        ANDGT     SAVETM
     C                   SETON                                        9085
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
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
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C* ZIP AREA SEARCH.
     C*
     C     HFLD          WHENEQ    'SCZIPA'
     C*
     C                   CALL      'F2405R'      SCHPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        8990
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGZ01                              99
     C*
     C                   MOVEL     GZZIPA        SCZIPA
     C                   SETON                                        8890
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     RCZIPA        SCZIPA
     C                   MOVEL     RCCPT4        SCCPT4
     C*
     C                   Z-ADD     RCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVEL     RCDSC1        SCDSC1
     C                   MOVEL     RCDSC2        SCDSC2
     C                   MOVE      RCMRA         SCMRA
     C                   MOVE      RCFOLU        SCFOLU
     C                   MOVE      RCANAM        SCANAM
     C                   MOVE      RCPROF        SCPROF
     C*** BEG ADD *** 03/02/01 ****************************************
     C                   MOVE      RC92N1        SC92N1
     C                   MOVE      RC1A2         SC1A2
     C                   MOVE      RCCONF        SCCONF
     C*** END ADD *** 03/02/01 ****************************************
     C*** BEGIN ADD *** 09/17/10 **************************************
     C                   MOVE      RC92N2        SC92N2
     C*** END   ADD *** 09/17/10 **************************************
     C                   MOVE      RC1A1         SC1A1                          ADD 03/21/94
     C                   MOVE      RCRPT         SCRPT
     C                   MOVE      RCSURG        SCSURG
     C                   MOVE      RCSUPP        SCSUPP
     C                   MOVE      RCFSR         SCFSR
     C                   MOVEL     RCSTAT        SCSTAT
     C*** BEGIN ADD *** 01/14/04 **************************************
     C     #OFSCR        IFEQ      2
     C     R1KY          CHAIN(N)  PR101                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     R1MR50        SCMR50
     C                   Z-ADD     R1MR60        SCMR60
     C                   Z-ADD     R1MR70        SCMR70
     C                   Z-ADD     R1MR75        SCMR75
     C                   Z-ADD     R1MR80        SCMR80
     C                   Z-ADD     R1MR85        SCMR85
     C                   Z-ADD     R1MR90        SCMR90
     C                   Z-ADD     R1MR95        SCMR95
     C*
     C                   Z-ADD     R1CV50        SCCV50
     C                   Z-ADD     R1CV60        SCCV60
     C                   Z-ADD     R1CV70        SCCV70
     C                   Z-ADD     R1CV75        SCCV75
     C                   Z-ADD     R1CV80        SCCV80
     C                   Z-ADD     R1CV85        SCCV85
     C                   Z-ADD     R1CV90        SCCV90
     C                   Z-ADD     R1CV95        SCCV95
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCMR50
     C                   Z-ADD     *ZEROS        SCMR60
     C                   Z-ADD     *ZEROS        SCMR70
     C                   Z-ADD     *ZEROS        SCMR75
     C                   Z-ADD     *ZEROS        SCMR80
     C                   Z-ADD     *ZEROS        SCMR85
     C                   Z-ADD     *ZEROS        SCMR90
     C                   Z-ADD     *ZEROS        SCMR95
     C*
     C                   Z-ADD     *ZEROS        SCCV50
     C                   Z-ADD     *ZEROS        SCCV60
     C                   Z-ADD     *ZEROS        SCCV70
     C                   Z-ADD     *ZEROS        SCCV75
     C                   Z-ADD     *ZEROS        SCCV80
     C                   Z-ADD     *ZEROS        SCCV85
     C                   Z-ADD     *ZEROS        SCCV90
     C                   Z-ADD     *ZEROS        SCCV95
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 01/14/04 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCZIPA
     C                   MOVE      *BLANKS       SCCPT4
     C                   MOVE      *ZEROS        SCEFDT
     C                   MOVE      *BLANK        SCDSC1
     C                   MOVE      *BLANK        SCDSC2
     C                   MOVE      *BLANK        SCMRA
     C                   MOVE      *BLANK        SCFOLU
     C                   MOVE      *BLANK        SCANAM
     C                   MOVE      *BLANK        SCPROF
     C*** BEG ADD *** 03/02/01 ****************************************
     C                   MOVE      *ZEROS        SC92N1
     C*** BEGIN ADD *** 09/17/10 **************************************
     C                   MOVE      *ZEROS        SC92N2
     C*** END   ADD *** 09/17/10 **************************************
     C                   MOVE      *BLANK        SC1A2
     C                   MOVE      *ZEROS        SCCONF
     C*** END ADD *** 03/02/01 ****************************************
     C                   MOVE      *BLANK        SC1A1
     C                   MOVE      *BLANK        SCRPT
     C                   MOVE      *BLANK        SCSURG
     C                   MOVE      *BLANK        SCSUPP
     C                   MOVE      *BLANKS       SCFSR                          ADD 04/28/94
     C                   MOVE      'A'           SCSTAT
     C*** BEGIN ADD *** 01/14/04 **************************************
     C     #OFSCR        IFEQ      2
     C                   Z-ADD     *ZEROS        SCMR50
     C                   Z-ADD     *ZEROS        SCMR60
     C                   Z-ADD     *ZEROS        SCMR70
     C                   Z-ADD     *ZEROS        SCMR75
     C                   Z-ADD     *ZEROS        SCMR80
     C                   Z-ADD     *ZEROS        SCMR85
     C                   Z-ADD     *ZEROS        SCMR90
     C                   Z-ADD     *ZEROS        SCMR95
     C*
     C                   Z-ADD     *ZEROS        SCCV50
     C                   Z-ADD     *ZEROS        SCCV60
     C                   Z-ADD     *ZEROS        SCCV70
     C                   Z-ADD     *ZEROS        SCCV75
     C                   Z-ADD     *ZEROS        SCCV80
     C                   Z-ADD     *ZEROS        SCCV85
     C                   Z-ADD     *ZEROS        SCCV90
     C                   Z-ADD     *ZEROS        SCCV95
     C                   ENDIF
     C*** END   ADD *** 01/14/04 **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         RCCO#
     C                   MOVEL     SCZIPA        RCZIPA
     C                   MOVEL     SCCPT4        RCCPT4
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           CALL 'SRCVTDAT'DTMDYY
     C*** END   DEL *** 12/02/97 **************************************
     C*
     C*** BEGIN CHG *** 12/02/97 **************************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** END   CHG *** 12/02/97 **************************************
     C                   MOVEL     SRDTO         RCEFDT
     C*
     C                   MOVEL     SCDSC1        RCDSC1
     C                   MOVEL     SCDSC2        RCDSC2
     C                   MOVE      SCMRA         RCMRA
     C                   MOVE      SCFOLU        RCFOLU
     C                   MOVE      SCANAM        RCANAM
     C                   MOVE      SCPROF        RCPROF
     C*** BEG ADD *** 03/02/01 ****************************************
     C                   MOVE      SC92N1        RC92N1
     C*** BEGIN ADD *** 09/17/10 **************************************
     C                   MOVE      SC92N2        RC92N2
     C*** END   ADD *** 09/17/10 **************************************
     C                   MOVE      SC1A2         RC1A2
     C                   MOVE      SCCONF        RCCONF
     C*** END ADD *** 03/02/01 ****************************************
     C                   MOVE      SC1A1         RC1A1                          ADD 03/21/94
     C                   MOVE      SCRPT         RCRPT
     C                   MOVE      SCSURG        RCSURG
     C                   MOVE      SCSUPP        RCSUPP
     C                   MOVE      SCFSR         RCFSR
     C                   MOVEL     SCSTAT        RCSTAT
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
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
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
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*** BEGIN ADD *** 01/14/04 **************************************
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
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C                   PARM      *BLANK        ##OS              1            OUTOME/SAVINGS
     C                   PARM      *BLANK        ##ARLS            1            Auto Release Flag
     C                   PARM      *BLANK        ##CCOD            1            Correct Coding Flag
     C                   PARM      *BLANK        ##CRVW            1            Code Review Flag
     C                   PARM      *BLANK        ##PROM            1            ProMatch Flag
     C                   PARM      *BLANK        ##MCRV            1            Medicare CR Flag
     C                   PARM      *BLANK        ##FREQ            1            Frequency Flag
     C                   PARM      *BLANK        ##CRAC            1            CR Auto Complete
     C                   PARM      *BLANK        ##PREO            1            CR Auto Complete
     C                   PARM      *BLANK        ##PARN            1            Par/Non-Par/Both
     C                   PARM      *BLANK        ##MGRI            1            Multi Group Restr
     C                   PARM      *BLANK        ##KCIF            1            Keep Claimant info
     C                   PARM      *BLANK        ##CURS            1            PBILLS entry cursor
     C                   PARM      *BLANK        ##PCIB            1            Prov Check in Batch
     C                   PARM      *BLANK        ##CLT#            3            Client Number
     C                   PARM      *BLANK        ##UCPF            1            UCR Percentile
     C*
     C                   Z-ADD     1             #OFSCR            3 0
     C     ##UCPF        IFEQ      'Y'
     C                   Z-ADD     2             #OFSCR
     C                   ENDIF
     C*** END   ADD *** 01/14/04 **************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
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
     C                   MOVEL     Q1USER        RCUSER
     C                   MOVEL     Q1PGM         RCPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RCDT
     C*
     C                   MOVEL     DSTIME        RCTIME
     C*
     C                   UPDATE    DBRCDF
     C*** BEGIN ADD *** 01/14/04 **************************************
     C     #OFSCR        IFEQ      2
     C                   EXSR      WUPR1P
     C                   ENDIF
     C*** END   ADD *** 01/14/04 **************************************
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
     C                   MOVEL     Q1USER        RCUSER
     C                   MOVEL     Q1PGM         RCPGM
     C                   MOVEL     Q1USER        RCIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RCDT
     C*
     C                   MOVEL     SRDTO         RCIDT
     C*
     C                   MOVEL     DSTIME        RCTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FMRCP
     C*** BEGIN ADD *** 01/14/04 **************************************
     C     #OFSCR        IFEQ      2
     C                   EXSR      WUPR1P
     C                   ENDIF
     C*** END   ADD *** 01/14/04 **************************************
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 01/14/04 **************************************
     C*================================================================
     C* WUPR1P - Write/Update FMR1P record and after processing
     C*================================================================
     C     WUPR1P        BEGSR
     C*
     C     R1KY          CHAIN     PR101                              99
     C*
     C                   Z-ADD     SCMR50        R1MR50
     C                   Z-ADD     SCMR60        R1MR60
     C                   Z-ADD     SCMR70        R1MR70
     C                   Z-ADD     SCMR75        R1MR75
     C                   Z-ADD     SCMR80        R1MR80
     C                   Z-ADD     SCMR85        R1MR85
     C                   Z-ADD     SCMR90        R1MR90
     C                   Z-ADD     SCMR95        R1MR95
     C*
     C                   Z-ADD     SCCV50        R1CV50
     C                   Z-ADD     SCCV60        R1CV60
     C                   Z-ADD     SCCV70        R1CV70
     C                   Z-ADD     SCCV75        R1CV75
     C                   Z-ADD     SCCV80        R1CV80
     C                   Z-ADD     SCCV85        R1CV85
     C                   Z-ADD     SCCV90        R1CV90
     C                   Z-ADD     SCCV95        R1CV95
     C*
     C                   MOVEL     RCSTAT        R1STAT
     C                   MOVEL     Q1USER        R1USER
     C                   MOVEL     Q1PGM         R1PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         R1DT
     C                   MOVEL     DSTIME        R1TIME
     C*
     C     *IN99         IFEQ      *OFF
     C                   UPDATE    PR101
     C                   ELSE
     C                   Z-ADD     RCCO#         R1CO#
     C                   MOVEL(P)  RCZIPA        R1ZIPA
     C                   MOVEL(P)  RCCPT4        R1PRCD
     C                   Z-ADD     RCEFDT        R1EFDT
     C                   WRITE     PR101
     C                   FEOD      FMR1L
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 01/14/04 **************************************
