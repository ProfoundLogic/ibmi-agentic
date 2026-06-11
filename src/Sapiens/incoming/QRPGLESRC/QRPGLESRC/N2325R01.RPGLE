     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  N2325R01 - NETWORK CONTRACTS INFORMATION                     *
     H*                                                               *
     H*  CALLED FROM: N2325R - WW NETWORK CONTRACTS                   *
     H*                                                               *
     H*  CALLS TO   : SRCVTDAT - DATE CONVERSION PROGRAM.             *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/29/23 C:079 Support: 146/111883                           *
     H*                 1) Added new Check Box "Use original          *
     H*                 transaction ID for recons" SCFL04.        .    *
     H*                                                               *
     H*  01/19/15 C:296 Project: 129MTU                               *
     H*                 1) Added new Check Box "Allow inactive        *
     H*                 providers- SCFL03". (Screen change)           *
     H*                                                               *
     H*  12/14/10 C:198 Support: 74444                                *
     H*                 Added processing for new field PAGR.          *
     H*                                                               *
     H*  02/24/10 C:227 PROJECT: 144MBT                               *
     H*                 PMI Medical Bill Upload                       *
     H*                                                               *
     H*  11/07/07 C:062 PROJECT: 25MPE                                *
     H*                 Add pend uploaded bills?                      *
     H*                                                               *
     H*  08/15/07 C:062 PROJECT: MC307                                *
     H*                 Add pend code field processing.               *
     H*                                                               *
     H*  10/25/05 C:163 PROJECT: 126MEA                               *
     H*                 BILL UPLOAD: UPLOAD USER SETUP                *
     H*                                                               *
     H*  06/19/00 C:62 SUPPORT: MC065 19768                           *
     H*                SWAP OUT USAGE OF STABL TO STSTL3, SO THAT UC  *
     H*                BECOMES INVALID                                *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FN2325F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FNMNWP     UF A E             DISK
     F                                     RENAME(PNW01:DBRCDF)
     FNMNWL     IF   E           K DISK
     F*** BEGIN CHG *** 06/19/00 **************************************
     F**********STABL   IF  E           K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END   CHG *** 06/19/00 **************************************
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
     F*** BEGIN ADD *** 10/25/05 **************************************
     FSMUPP     IF   E             DISK
     FSMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     F*** END   ADD *** 10/25/05 **************************************
     F*** BEGIN ADD *** 08/15/07 **************************************
     FFTBSL     IF   E           K DISK
     FFTBSP     IF   E             DISK
     F                                     RENAME(PBS01:PBS02)
     F*** END   ADD *** 08/15/07 **************************************
     F*** BEG ADD *** 02/24/10 ****************************************
     F*  Fee Schedule Claims Status Code File
     FFTCSP     IF   E             DISK
     F* TABLE FILE LOGICAL
     FFTABL     IF   E           K DISK
     F                                     RENAME(PCS01:PCS02)
     F*** END ADD *** 02/24/10 ****************************************
     F*** Beg Add *** 12/14/10 ****************************************
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*** End Add *** 12/14/10 ****************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Network Master -
     D                                     Maintenance')
     D COMAPI          C                   CONST('FA007R')
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
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Add/change/display program parameter list
     C*
     C     STSPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*** BEG ADD *** 02/24/10 ****************************************
     C*
     C** CLAIM STATUS CODE TABLE FILE PARM LIST
     C*
     C     CSPLT         PLIST
     C                   PARM      '2'           ##CALL
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C*
     C*** END ADD *** 02/24/10 ****************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*** BEGIN ADD *** 08/15/07 **************************************
     C*
     C* Bill Status Table
     C*
     C     BSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*** END   ADD *** 08/15/07 **************************************
     C*
     C     NWKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##NWCD
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY            10
     C*
     F*** BEGIN ADD *** 10/25/05 **************************************
     C     UPKY          KLIST
     C                   KFLD                    SC10A2
     C*
     F*** END   ADD *** 10/25/05 **************************************
     C*** BEG ADD *** 02/24/10 ****************************************
     C*
     C* Key List for Table File
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY1
     C*
     C*** END ADD *** 02/24/10 ****************************************
     C*** Beg Add *** 12/14/10 ****************************************
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** End Add *** 12/14/10 ****************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    NWCO#         ##CO#
     C     *LIKE         DEFINE    NWNWCD        ##NWCD
     C     *LIKE         DEFINE    NWST          ##ST
     C*** BEGIN ADD *** 08/15/07 **************************************
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C*** END   ADD *** 08/15/07 **************************************
     C*** BEG ADD *** 02/24/10 ****************************************
     C     *LIKE         DEFINE    CSKEY         KKKEY1
     C*** END ADD *** 02/24/10 ****************************************
     C*** Beg Add *** 12/14/10 ****************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C*** End Add *** 12/14/10 ****************************************
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
     C     NWSTAT        IFEQ      'I'
     C                   MOVE      'A'           NWSTAT
     C                   ELSE
     C                   MOVE      'I'           NWSTAT
     C                   ENDIF                                                  NWSTAT
     C*
     C                   MOVEL     Q1USER        NWUSER
     C                   MOVEL     Q1PGM         NWPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NWDT
     C*
     C                   MOVEL     DSTIME        NWTIME
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
     C*** BEG ADD *** 02/24/10 ****************************************
     C                   SETOFF                                       36
     C*** BEG ADD *** 02/24/10 ****************************************
     C*** Beg Add *** 12/14/10 ****************************************
     C                   SETOFF                                       3738
     C                   SETOFF                                       39
     C*** End Add *** 12/14/10 ****************************************
     C*
     C* NETWORK CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCNWCD        IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE NETWORK CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCNWCD        ##NWCD
     C*
     C   40
     CANN90NWKY          CHAIN     PNW01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* NAME CANNOT BE BLANK
     C*
     C  N90SCNAME        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCNAME
     C*
     C* STATE MUST BE VALID - IF ENTERED
     C*
     C                   MOVEL(P)  SCST          KKKEY
     C*
     C  N90STKY          CHAIN     PST01                              99
     C*
     C  N90*IN99         IFEQ      '1'
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF                                                  *IN99 EQ *ON
     C*
     C* THE EFF. DATE MUST BE VALID - IF ENTERED
     C*
     C  N90SCEFDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        7590
     C*
     C                   ENDIF                                                  SRERR = '1'    '
     C*
     C                   ENDIF                                                  SCEFDT NE 0
     C*
     C*** BEGIN ADD *** 10/25/05 **************************************
     C*
     C* THE UPLOAD USER ID MUST BE VALID - IF ENTERED
     C*
     C     *IN90         IFEQ      *OFF
     C     SC10A2        ANDNE     *BLANKS
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C   99              SETON                                        8090
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 10/25/05 **************************************
     C*** BEGIN ADD *** 11/07/07 **************************************
     C*
     C* Pend code must be valid if entered
     C*
     C     *IN90         IFEQ      *OFF
     C     SCFL01        ANDNE     'Y'
     C     SCFL01        ANDNE     'N'
     C     SCFL01        ANDNE     ' '
     C                   SETON                                        8990
     C                   ENDIF
     C*** END   ADD *** 11/07/07 **************************************
     C*** BEGIN ADD *** 08/15/07 **************************************
     C*
     C* Pend code must be valid if entered
     C*
     C     *IN90         IFEQ      *OFF
     C     SC10A3        ANDNE     *BLANKS
     C                   MOVEL(P)  'P'           KKTYPE                         PEND
     C                   MOVEL(P)  SC10A3        KKCODE
     C     BSKY          CHAIN     PBS01                              99
     C     *IN99         IFEQ      *ON
     C     *IN99         OREQ      *OFF
     C     BSSTAT        ANDEQ     'I'
     C                   SETON                                        9081
     C                   ENDIF
     C                   ENDIF                                                              P
     C*** END   ADD *** 08/15/07 **************************************
     C*** BEG ADD *** 02/24/10 **************************************
     C*
     C* Adjudication code must be valid if entered
     C*
     C  N90SCCSCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCCSCD        KKKEY1
     C     TBKY          CHAIN     PCS02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9036
     C                   ELSE
     C     CSSTAT        IFEQ      'I'
     C                   SETON                                        9036
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 02/24/10 **************************************
     C*** Beg Add *** 12/14/10 ****************************************
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1            WC INST
     C                   PARM                    @@MCIF            1            MC INST
     C                   PARM                    @@WCIN            1            WC INTERFACE
     C                   PARM                    @@AR              1            MC AR INST
     C                   PARM                    @@NTWK            1            NETWORK INST
     C                   PARM                    @@UR              1            PRECERT INST
     C                   PARM                    @@MED             1            MEDMGMT INST
     C                   PARM                    @@EDI             1            EDI INST
     C                   PARM                    @@WCDK            1            WC DARTS KEY
     C                   PARM                    @@RHAB            1            REHAB INST
     C                   PARM                    @@MSS             1            MS SUPPORT
     C                   PARM                    @@FSCH            1            FEE SCHEDULE
     C                   PARM                    @@PRVB            1            PROVIDER BILL
     C*
     C     @@EDI         IFEQ      'Y'
     C     SCPAGR        ANDEQ     ' '
     C                   SETON                                        9039
     C                   ENDIF
     C*
     C  N90SCPAGR        IFNE      *BLANKS
     C                   MOVEL(P)  'DFPA'        KKTBID
     C                   MOVEL(P)  SCPAGR        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9038
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPAGR NE *BLANKS
     C*** End Add *** 12/14/10 ****************************************
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90NWDT          IFGT      SAVEDT
     C     NWDT          OREQ      SAVEDT
     C     NWTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C*** Beg Add *** 01/19/15 **************************************
     C*
     C* Inactive provider code validation.
     C*
     C                   eval      indFL03 = *off
     C     *IN90         IFEQ      *OFF
     C     SCFL03        ANDNE     'Y'
     C     SCFL03        ANDNE     'N'
     C     SCFL03        ANDNE     ' '
     C                   SETON                                        90
     C                   eval      indFL03 = *on
     C                   ENDIF
     C*** End Add *** 01/19/15 **************************************
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
     C* KJDO - F10 SEARCHES
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SETOFF                                       33
     C*
     C                   SELECT
     C*
     C* STATE SEARCH
     C*
     C     HFLD          WHENEQ    'SCST'
     C*
     C                   CALL      'SY601R'      STSPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                          76
     C*
     C                   ELSE                                                   QQRTC EQ 'B'
     C*
     C     ##RRN         CHAIN     STSTP                              99
     C*
     C                   MOVEL     STKEY         SCST
     C                   SETON                                          77
     C*
     C                   ENDIF                                                  QQRTC EQ '*'
     C*
     C*** BEGIN ADD *** 10/25/05 **************************************
     C*
     C* USER ID SEARCH
     C*
     C     HFLD          WHENEQ    'SC10A2'
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      1             ##VIEW            1 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                          79
     C*
     C                   ELSE                                                   *INKL = *ON
     C*
     C     ##RRN         CHAIN     PUP01                              99
     C*
     C                   MOVEL     UPUSR         SC10A2
     C*** BEGIN ADD *** 08/15/07 **************************************
     C*** BEGIN CHG *** 11/07/07 **************************************
     C**********           SETON                     32
     C                   SETON                                        34
     C*** END   CHG *** 11/07/07 **************************************
     C*** END   ADD *** 08/15/07 **************************************
     C*
     C                   ENDIF                                                  *INKL = *ON
     C*
     C*** END   ADD *** 10/25/05 **************************************
     C*** BEGIN ADD *** 08/15/07 **************************************
     C*
     C* PEND CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SC10A3'
     C*
     C                   CALL      'F2435R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZERO         ##KC              1
     C                   PARM      *ZERO         ##KL              1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      'P'           ##TYPE            1
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                        32
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PBS02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  BSCODE        SC10A3
     C                   MOVEL(P)  BSDESC        SCPDSC
     C*** BEG ADD *** 02/24/10 ****************************************
     C                   SETON                                        35
     C*** END ADD *** 02/24/10 ****************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C*** END   ADD *** 08/15/07 **************************************
     C*** BEG ADD *** 02/24/10 ****************************************
     C*
     C** ADJUDICATION CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCCSCD'
     C*
     C                   CALL      'F2300R'      CSPLT
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        35        pos cursor
     C                   ENDIF                                                  ##KL IFEQ *ON
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PCS01                              99
     C                   MOVEL(P)  CSKEY         SCCSCD
     C                   MOVEL(P)  CSDESC        SCCSDS
     C*** Beg Add *** 12/14/10 ****************************************
     C                   SETON                                        37        pos cursor
     C*** End Add *** 12/14/10 ****************************************
     C                   ENDIF                                                  ##RRN IFNE ZEROS
     C*
     C*** END ADD *** 02/24/10 ****************************************
     C*
     C*** Beg Add *** 12/14/10 ****************************************
     C     HFLD          WHENEQ    'SCPAGR'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCPAGR        HLPAGR            1
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'DFPA'        ##TBID            4
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLPAGR        SCPAGR
     C                   SETON                                        9037
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9038
     C                   ELSE                                                   *IN99 = *ON
     C                   MOVEL     GTKEY         SCPAGR
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** End Add *** 12/14/10 ****************************************
     C                   OTHER
     C*
     C                   SETON                                        33
     C*
     C                   ENDSL
     C*
     C                   SETON                                          90
     C*
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     NWNWCD        SCNWCD
     C                   MOVEL     NWNAME        SCNAME
     C                   MOVEL     NWADR1        SCADR1
     C                   MOVEL     NWADR2        SCADR2
     C                   MOVEL     NWCTY         SCCTY
     C                   MOVEL     NWST          SCST
     C                   MOVEL     NWZIP         SCZIPA
     C                   MOVE      NWZIP         SCZIPB
     C                   MOVE      NWPARA        SCPARA
     C                   MOVE      NWPHN#        SCPHN#
     C                   MOVE      NWFARA        SCFARA
     C                   MOVE      NWFAX#        SCFAX#
     C*
     C                   Z-ADD     NWEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVEL     NWSTAT        SCSTAT
     C*** BEGIN ADD *** 10/25/05 **************************************
     C                   MOVEL     NW10A2        SC10A2
     C*** END   ADD *** 10/25/05 **************************************
     C*** BEGIN ADD *** 11/07/07 **************************************
     C                   MOVEL     NWFL01        SCFL01
     C*** END   ADD *** 11/07/07 **************************************
     C*
     C*** BEGIN ADD *** 08/15/07 **************************************
     C                   MOVEL(P)  NW10A3        SC10A3
     C                   MOVEL(P)  'P'           KKTYPE                         PEND
     C                   MOVEL(P)  NW10A3        KKCODE
     C     BSKY          CHAIN     PBS01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  BSDESC        SCPDSC
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCPDSC
     C                   ENDIF
     C*** END   ADD *** 08/15/07 **************************************
     C*
     C*** BEG ADD *** 02/24/10 ****************************************
     C                   MOVEL(P)  NW10A4        SCCSCD
     C                   MOVEL(P)  SCCSCD        KKKEY1
     C     TBKY          CHAIN     PCS02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CSDESC        SCCSDS
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCCSDS
     C                   ENDIF
     C*** END ADD *** 02/24/10 ****************************************
     C*** Beg Add *** 12/14/10 ****************************************
     C                   MOVEL     NWFL02        SCPAGR
     C*** End Add *** 12/14/10 ****************************************
     C*** Beg Add *** 01/19/15 ****************************************
     C                   MOVEL     NWFL03        SCFL03
     C*** End Add *** 01/19/15 ****************************************
        //*** Beg Add 06/29/23 ***************************************
        scFl04 = NWfl04;
        //*** End Add 06/29/23 ***************************************
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCNWCD
     C                   MOVEL     *BLANKS       SCNAME
     C                   MOVEL     *BLANKS       SCADR1
     C                   MOVEL     *BLANKS       SCADR2
     C                   MOVEL     *BLANKS       SCCTY
     C                   MOVEL     *BLANKS       SCST
     C                   MOVE      *ZEROS        SCZIPA
     C                   MOVE      *ZEROS        SCZIPB
     C                   MOVE      *ZEROS        SCPARA
     C                   MOVE      *ZEROS        SCPHN#
     C                   MOVE      *ZEROS        SCFARA
     C                   MOVE      *ZEROS        SCFAX#
     C                   MOVE      *ZEROS        SCEFDT
     C                   MOVEL     'A'           SCSTAT
     C*** BEGIN ADD *** 10/25/05 **************************************
     C                   MOVEL     *BLANKS       SC10A2
     C*** END   ADD *** 10/25/05 **************************************
     C*** BEGIN ADD *** 11/07/07 **************************************
     C                   MOVEL     *BLANKS       SCFL01
     C*** END   ADD *** 11/07/07 **************************************
     C*** BEGIN ADD *** 08/15/07 **************************************
     C                   MOVEL     *BLANKS       SC10A3
     C                   MOVEL     *BLANKS       SCPDSC
     C*** END   ADD *** 08/15/07 **************************************
     C*** BEG ADD *** 02/24/10 ****************************************
     C                   MOVEL     *BLANKS       SCPDSC
     C*** END ADD *** 02/24/10 ****************************************
     C*** Beg Add *** 12/14/10 ****************************************
     C                   MOVEL     *BLANKS       SCPAGR
     C*** End Add *** 12/14/10 ****************************************
        //*** Beg Add 06/29/23 ***************************************
        scFl04 = 'Y';
        //*** End Add 06/29/23 ***************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         NWCO#
     C                   MOVEL     SCNWCD        NWNWCD
     C                   MOVEL     SCNAME        NWNAME
     C                   MOVEL     SCADR1        NWADR1
     C                   MOVEL     SCADR2        NWADR2
     C                   MOVEL     SCCTY         NWCTY
     C                   MOVEL     SCST          NWST
     C                   MOVEL     SCZIPA        NWZIP
     C                   MOVE      SCZIPB        NWZIP
     C                   MOVE      SCPARA        NWPARA
     C                   MOVE      SCPHN#        NWPHN#
     C                   MOVE      SCFARA        NWFARA
     C                   MOVE      SCFAX#        NWFAX#
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NWEFDT
     C*
     C                   MOVEL     SCSTAT        NWSTAT
     C*** BEGIN ADD *** 10/25/05 **************************************
     C                   MOVEL     SC10A2        NW10A2
     C*** END   ADD *** 10/25/05 **************************************
     C*** BEGIN ADD *** 08/15/07 **************************************
     C                   MOVEL     SC10A3        NW10A3
     C*** END   ADD *** 08/15/07 **************************************
     C*** BEG ADD *** 02/24/10 ****************************************
     C                   MOVEL     SCCSCD        NW10A4
     C*** END ADD *** 02/24/10 ****************************************
     C*** Beg Add *** 12/14/10 ****************************************
     C                   MOVEL     SCPAGR        NWFL02
     C*** End Add *** 12/14/10 ****************************************
     C*** BEGIN ADD *** 11/07/07 **************************************
     C                   MOVEL     SCFL01        NWFL01
     C*** END   ADD *** 11/07/07 **************************************
     C*** Beg Add *** 01/19/15 ****************************************
     C                   MOVEL     SCFL03        NWFL03
     C*** End Add *** 01/19/15 ****************************************
        //*** Beg Add 06/29/23 ***************************************
        NWfl04 = scFl04;
        //*** End Add 06/29/23 ***************************************
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
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
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
     C                   MOVEL     Q1USER        NWUSER
     C                   MOVEL     Q1PGM         NWPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NWDT
     C*
     C                   MOVEL     DSTIME        NWTIME
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
     C                   MOVEL     Q1USER        NWUSER
     C                   MOVEL     Q1PGM         NWPGM
     C                   MOVEL     Q1USER        NWIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NWDT
     C*
     C                   MOVEL     SRDTO         NWNIDT
     C*
     C                   MOVEL     DSTIME        NWTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      NMNWP
     C*
     C                   ENDSR
