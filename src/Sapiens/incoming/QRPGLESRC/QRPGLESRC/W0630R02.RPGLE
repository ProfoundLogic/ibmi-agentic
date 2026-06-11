     H Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT

     H/TITLE W0630R02 - PAYROLL CLASS CODE INFORMATION
      *****************************************************************
      *                                                               *
      *  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
      *                OR ADD RECORDS IN THE PAYROLL CLASS CODE FILE. *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    W0630R02 - WORK WITH PAYROLL CLASS CODES                   *
      *                                                               *
      *  CALLS TO:                                                    *
      *                                                               *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 06/17/24 C:344 Project: WC2952                                *
      *                Expand scRT field in display file.             *
      *                                                               *
      * 08/05/20 C:65  Project: WC2756                                *
      *                Add flags for exclude from terrorism calcs and *
      *                exclude from claim assignment.                 *
      *                                                               *
      * 10/20/15 C:302 Project: WC2364                                *
      *                Add Non-Ratable class code flag.               *
      *                                                               *
      * 03/31/14 C:189 Support: 85226                                 *
      *                Changed the decription field not to convert    *
      *                the description to upper case                  *
      *                                                               *
      * 04/26/13 C:279 Redmine: 2823                                  *
      *                Default Contractor Class Checkbox to 'N'.      *
      *                                                               *
      * 03/12/10 C:235 PROJECT: 102WFC                                *
      *                ADDITION CLASSES FOR LOGGING CALC              *
      *                                                               *
      * 08/10/05 C:136 PROJECT: WC1136                                *
      *                ADDED LCM EXP DT FIELD                         *
      *                                                               *
      * 06/24/05 C:132 PROJECT: WC1136                                *
      *                Added field in search for Loss cost Multiplier *
      *                                                               *
      * 10/20/04 C:132 PROJECT: WC1136                                *
      *              - Add field to display the Loss Cost Multiplier, *
      *                WMPRP/PRLCM, Do edits in Add/Change mode.      *
      *              - Add Search on Loss Cost Multiplier             *
      *                                                               *
      * 04/26/04 C:136 PROJECT: WC1169                                *
      *                Add field: Occupational Disease Class (Y/ )    *
      *                                                               *
      *                                                               *
      * 10/13/03 C:61  PROJECT WC1017                                 *
      *              - Add field: "Seasonal class (Y/ )" PRSCCD.      *
      *              - Correct "Updated by other user" issue.         *
      *                                                               *
      * 03/18/03 C:61  PROJECT WC1106                                 *
      *                Add field for "Allow 'A' rating on class (Y/ )"*
      *                                                               *
      * 10/30/02 C:61  SUPPORT WA122 34682                            *
      *              - Related: SUPP/SEQ# WA107/21140                 *
      *                When setting up a new rate, find the latest    *
      *                ACTIVE record.                                 *
      *              - Change CallED programs to not use same parms   *
      *                as *ENTRY.                                     *
      *                                                               *
      * 10/05/01 C:65  PROJECT: WC1027                                *
      *                DO NOT SHOW THE POPULATION TABLE ON DISPLAY    *
      *                MODE WHEN THE CLASS ISN'T POPULATION RATED.    *
      *                                                               *
      * 10/01/01 C:136 PROJECT:WC577                                  *
      *                Add new entry:'Loss constant amount' (optional)*
      *                                                               *
      * 08/15/01 C:90  PROJECT: WC1035                                *
      *                Expand rate field from 5.2 to 7.2              *
      *                                                               *
      * 03/14/01 C:65  PROJECT: WC1027                                *
      *                ALLOW MINIMUM PREMIUM TO BE ZERO.              *
      *                ALLOW THE PREMIUM BASIS FLAG TO BE SET TO      *
      *                'O' FOR POPULATION AND CALL W0631R TO SET      *
      *                THE POPULATION TABLE.                          *
      *                                                               *
      * 10/24/00 C:90  PROJECT: SEQ#: WA107/24732                     *
      *                WHEN DOING F16 OR OPT16, FIND THE MOST RECENT  *
      *                ACTIVE PAYROLL CLASS.                          *
      *                                                               *
      * 03/27/00 C:90  PROJECT: SUPP/SEQ#: WA091/22346                *
      *                WHEN DOING F16 OR OPT16, THE RATE SET CODE AND *
      *                PAYROLL CLASS CODE MUST BE ACTIVE.             *
      *                                                               *
      * 03/05/00 C:118 PROJECT: WC906                                 *
      *                ADD A NEW OPTION 3=COPY                        *
      *                                                               *
      * 02/03/00 C:90  PROJECT: SUPP/SEQ#: WA107/21675                *
      *                FIND THE NEXT ACTIVE RECORD IF THE INITIAL     *
      *                READ IS NOT ACTIVE.                            *
      *                                                               *
      * 12/29/99 C:90  PROJECT: SUPP/SEQ# WA107/21140                 *
      *                When setting up a new rate, find the latest    *
      *                ACTIVE record.                                 *
      *                                                               *
      * 10/19/99 C:61  PROJECT WC944                                  *
      *              - Add optional entries for: Expiration date,     *
      *                Transfer class, Governing class exclusion.     *
      *                                                               *
      *  08/07/98  C:87  PROJECT: WC591                               *
      *                  Added Exposure Act and Rating Tier fields    *
      *                  and processing for WCSTAT.                   *
      *                                                               *
      *  11/03/97  C:33  PROJECT: WC577                               *
      *                  Only allow the F10 search to be called when  *
      *                  cursor is postioned on a valid field.        *
      *                                                               *
      *  PLEASE ADD TO TOP OF REVISIONS                               *
      *****************************************************************

     FW0630F02  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')

      *  Physical file definition (for maintaining record)
     FWMPRP     UF A E             DISK
     F                                     RENAME(PPR01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWTPOL     IF A E           K DISK
     FWMPRL4    IF   E           K DISK
      *  Logical for Opt03=Copy
     FWMPRL7    IF   E           K DISK
     F                                     RENAME(PPR01:PPR02)
     FWMRSL     IF   E           K DISK

      *      System Table ID File
     FWTGIL     IF   E           K DISK
      *      Sustem Table Value File
     FWTGTL     IF   E           K DISK

      *  LOSS COST MULTIPLIER
     FWDLML     IF   E           K DISK
      /COPY SCOPYMT01D

     D DBFDS           DS
     D  DBRRN                397    400B 0

      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Payroll Class Mast-
     D                                     er Information')
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
      *  Company file/fund (group) retrieve program parameter list
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *================================================================
      *  KLIST definitions
      *================================================================
     C     PRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##RTCD
     C                   KFLD                    ##CLAS
     C                   KFLD                    ##EFDT

     C     PRKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCRTCD
     C                   KFLD                    SCCLAS
     C                   KFLD                    ##EFDT

     C     PRKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCRTCD
     C                   KFLD                    SCCLAS

     C     PRKY3         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##RTCD
     C                   KFLD                    ##CLAS

      *  Key for Opt03=Copy

     C     PRKY4         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##RTCD
     C                   KFLD                    ##EFDT

     C     POKY          KLIST
     C                   KFLD                    PRCO#
     C                   KFLD                    PRRTCD
     C                   KFLD                    PRCLAS
     C                   KFLD                    PREFDT

     C     POKY1         KLIST
     C                   KFLD                    PRCO#
     C                   KFLD                    PRRTCD
     C                   KFLD                    PRCLAS
     C                   KFLD                    SVPODT

     C     RSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##RTCD
      *  SYSTEM TABLE ID FILE
     C     GIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
      *  SYSTEM TABLE VALUE FILE
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY            10
      *  LOSS COST MULTIPLIER
     C     LMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    @@KEY

     C     LMKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    @@KEY
     C                   KFLD                    ZZEFDT            7 0
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    PRCO#         ##CO#
     C     *LIKE         DEFINE    PRRTCD        ##RTCD
     C     *LIKE         DEFINE    PRRTCD        @@KEY
     C     *LIKE         DEFINE    PRCLAS        ##CLAS
     C     *LIKE         DEFINE    PREFDT        ##EFDT
     C     *LIKE         DEFINE    PREFDT        XXEFDT
     C     *LIKE         DEFINE    PREXDT        XXEXDT
     C     *LIKE         DEFINE    PRLCEX        XXLCEX
     C     *LIKE         DEFINE    SCRTCD        SVRTCD
     C     *LIKE         DEFINE    SCEFDT        SVEFDT
     C     *LIKE         DEFINE    SCPRDS        QQPRDS
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    PRCO#         SVCO#
     C     *LIKE         DEFINE    PRCLAS        SVCLAS
     C     *LIKE         DEFINE    PREFDT        SVEFD1
     C     *LIKE         DEFINE    PRDESC        SVDESC
     C     *LIKE         DEFINE    PRRT          SVRT
     C     *LIKE         DEFINE    PRMNPR        SVMNPR
     C     *LIKE         DEFINE    PRPCF         SVPCF
     C     *LIKE         DEFINE    PRSTAT        SVSTAT
     C     *LIKE         DEFINE    PRLHCD        SVLHCD
     C     *LIKE         DEFINE    PRCCFL        SVCCFL
     C     *LIKE         DEFINE    PRRTCD        SVRTC1
     C     *LIKE         DEFINE    PRHZGR        SVHZGR
     C     *LIKE         DEFINE    PRSIC         SVSIC
     C     *LIKE         DEFINE    PREACT        SVEACT
     C     *LIKE         DEFINE    PRTIER        SVTIER
     C     *LIKE         DEFINE    PREXDT        SVEXDT
     C     *LIKE         DEFINE    PRXFER        SVXFER
     C     *LIKE         DEFINE    PRGVEX        SVGVEX
     C     *LIKE         DEFINE    PRARFL        SVARFL
     C     *LIKE         DEFINE    PRSCCD        SVSCCD

     C                   MOVE      *BLANK        QQCALL            1

     C/COPY SCOPYMT01C

      *================================================================
      * CHGSTS - SUBROUTINE FOR CHANGING STATUS
      *================================================================

     C     CHGSTS        BEGSR

     C     @@RRN         CHAIN     DBRCDF                             99

     C     *IN99         IFEQ      *OFF

     C     PRSTAT        IFEQ      'I'
     C                   MOVE      'A'           PRSTAT
     C                   ELSE
     C                   MOVE      'I'           PRSTAT
     C                   ENDIF

     C                   MOVEL     Q1USER        PRUSER
     C                   MOVEL     Q1PGM         PRPGM

     C                   TIME                    DSDTTM

     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT

     C                   MOVEL     DSTIME        PRTIME

     C                   UPDATE    DBRCDF

     C                   ENDIF

     C                   ENDSR
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
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
     C                   MOVE      *BLANK        XXPOP

      * RATE SET CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)

     C   40
     CANN90SCRTCD        IFEQ      *BLANKS
     C                   SETON                                        70  90
     C                   ENDIF

      * RATE SET CODE MUST BE VALID

     C                   MOVEL     SCRTCD        ##RTCD

     C   40
     CANN90RSKY          CHAIN     PRS01                              99
     C   40
     CANN90
     CAN 99              SETON                                        71  90

      * CLASS CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)

     C   40
     CANN90SCCLAS        IFEQ      *ZEROS
     C                   SETON                                        72  90
     C                   ENDIF

      * EFFECTIVE DATE MUST BE VALID

     C     *IN40         IFEQ      *ON
     C     *IN90         ANDEQ     *OFF

     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT

     C     SRERR         IFEQ      '1'

     C                   SETON                                        73  90

     C                   ENDIF

     C                   ENDIF

      * CHECK TO BE SURE CLASS CODE HAS NOT BEEN USED PREVIOUSLY

     C                   MOVEL     SCRTCD        ##RTCD
     C                   MOVE      SCCLAS        ##CLAS

     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##EFDT

     C   40
     CANN90PRKY          CHAIN     PPR01                              99
     C   40
     CANN90
     CANN99              SETON                                        74  90

      * CLASS CODE DESCRIPTION CANNOT BE BLANK

     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        75  90
     C                   ENDIF

      *  MINIMUM PREMIUM, CANNOT BE *ZERO.

     C  N90SCMNPR        IFLT      *ZERO

     C                   SETON                                        76  90

     C                   ENDIF

      *  PREMIUM BASIS FLAG MUST BE P,C

     C  N90SCPCF         IFNE      'P'
     C     SCPCF         ANDNE     'C'
     C     SCPCF         ANDNE     'O'

     C                   SETON                                        77  90

     C                   ENDIF

      *  L & H DESIGNATION FLAG MUST BE Y,N

     C  N90SCLHCD        IFNE      'Y'
     C     SCLHCD        ANDNE     'N'

     C                   SETON                                        78  90

     C                   END

      *  CONTRACTORS CLASS MUST BE Y,N

     C  N90SCCCFL        IFNE      'Y'
     C     SCCCFL        ANDNE     'N'

     C                   SETON                                        79  90

     C                   END

      *  LOGGING CLASS FLAG MUST BE (Y/ ).

     C  N90SCLOGF        IFNE      *BLANK
     C     SCLOGF        ANDNE     'Y'

     C                   SETON                                        35  90
     C                   END

     C     *IN90         IFNE      *ON

      *  CHECK THAT EXPOSURE ACT IS VALID

     C     SCEACT        IFNE      *BLANK
      *       Must be in Table file
     C                   MOVEL(P)  SCEACT        XXKEY
     C                   MOVE(P)   'XA'          XXTBID
      *       First try state table
     C                   MOVEL     RSJRST        XXTBID
     C     GTKY          CHAIN     PGT01                              90
      *       If not found, try generic NCCI table
     C     *IN90         IFEQ      '1'
     C     RSJRST        ANDNE     *BLANK
     C                   MOVEL     '  XA'        XXTBID
     C     GTKY          CHAIN     PGT01                              90
     C                   ENDIF

     C     *IN90         IFEQ      '1'
     C     GTSTAT        OREQ      'I'
     C                   MOVE      '1'           *IN91
     C                   ENDIF

     C                   ENDIF

     C                   ENDIF

      *  CHECK THAT RATING TIER IS VALID

     C     *IN90         IFNE      *ON

     C     SCTIER        IFNE      *BLANK
      *       Must be in Table file
     C                   MOVEL(P)  SCTIER        XXKEY
     C                   MOVE(P)   'RT'          XXTBID
      *       First try state table
     C                   MOVEL     RSJRST        XXTBID
     C     GTKY          CHAIN     PGT01                              90
      *       If not found, try generic NCCI table
     C     *IN90         IFEQ      '1'
     C     RSJRST        ANDNE     *BLANK
     C                   MOVEL     '  RT'        XXTBID
     C     GTKY          CHAIN     PGT01                              90
     C                   ENDIF

     C     *IN90         IFEQ      '1'
     C     GTSTAT        OREQ      'I'
     C                   MOVE      '1'           *IN95
     C                   ENDIF

     C                   ENDIF

     C                   ENDIF

      *  Governing class exclusion flag must be (Y/ ).

     C  N90SCGVEX        IFNE      *BLANK
     C     SCGVEX        ANDNE     'Y'
     C                   SETON                                        81  90
     C                   ENDIF

      *  "Allow 'A' rating on this class (Y/ )?"

     C  N90SCARFL        IFNE      *BLANK
     C     SCARFL        ANDNE     'Y'
     C                   SETON                                        50  90
     C                   ENDIF

      *  "Seasonal class (Y/ )"

     C  N90SCSCCD        IFNE      *BLANK
     C     SCSCCD        ANDNE     'Y'
     C                   SETON                                        51  90
     C                   ENDIF

      *  "Occupation disease class (Y/ )"

     C  N90SCOCCD        IFNE      *BLANK
     C     SCOCCD        ANDNE     'Y'
     C                   SETON                                        52  90
     C                   ENDIF

      *  Expiration date must be valid and greater than effective date,
      *  if entered.

     C  N90SCEXDT        IFNE      *ZERO

     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT

     C     SRERR         IFEQ      '1'
     C                   SETON                                        82  90
     C                   ENDIF

     C     *IN90         IFEQ      *OFF

     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT

     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT

     C     XXEXDT        IFLT      XXEFDT
     C                   SETON                                        83  90
     C                   ENDIF

     C                   ENDIF

     C                   ENDIF

      *  Transfer class must be valid and effective as of expiration date,
      *  if entered.

     C  N90SCXFER        IFNE      *ZERO

     C                   MOVEL     SCRTCD        ##RTCD
     C                   Z-ADD     SCXFER        ##CLAS

     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##EFDT

     C     PRKY          SETGT     PPR01
     C     PRKY3         READPE    PPR01                                  90
     C   90              SETON                                        84

     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT

     C     PREXDT        IFNE      *ZERO
     C     PREXDT        ANDLE     XXEXDT
     C                   SETON                                        84  90
     C                   ENDIF

      *  Transfer class and must be different payroll class

     C     SCCLAS        IFEQ      SCXFER
     C                   SETON                                        85  90
     C                   ENDIF

     C                   ENDIF

     C  N90SVMODE        IFEQ      'R'
     C                   MOVE      *ON           *IN21
     C                   ENDIF
     C                   Z-ADD     0             SVPODT            7 0
     C     SCPCF         IFEQ      'O'
     C     *IN90         ANDEQ     *OFF
     C                   MOVE      'Y'           XXPOP             1
     C     *IN21         IFEQ      *ON
     C                   Z-ADD     PREFDT        SVPODT
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *BLANK        XXPOP
     C                   ENDIF
     C     *IN90         IFEQ      *OFF
     C     SCLOSS        ANDLT     0
     C                   SETON                                        8790
     C                   ENDIF

      * Checks for Loss Cost Multiplier
     C     *IN90         IFEQ      *OFF
     C     SCLCM         ANDLT     *ZEROS
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN30
     C                   ENDIF

     C  N90SCLCEX        IFNE      0
     C                   Z-ADD     SCLCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCLCEX
     C     SRERR         IFEQ      '1'
     C                   SETON                                        88  90
     C                   ENDIF
     C                   ENDIF

     C  N90SCLCEX        IFNE      0
     C                   Z-ADD     SCLCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXLCEX
     C     XXLCEX        IFLT      XXEFDT
     C                   SETON                                        89  90
     C                   ENDIF
     C                   ENDIF

      * WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN

      * CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY

     C     @@RRN         CHAIN     DBRCDF                             60

     C  N60
     CANN90PRDT          IFGT      SAVEDT
     C     PRDT          OREQ      SAVEDT
     C     PRTIME        ANDGT     SAVETM
     C                   SETON                                        9080
     C                   ENDIF

     C                   ENDSR
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR

      * WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN

     C                   ENDSR
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR

      * WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN

     C                   ENDSR
      *================================================================
      *  ERRCKW - Error checking for window 1 (WDW01)
      *================================================================
     C     ERRCKW        BEGSR

      *  RATE SET CODE MUST NOT EQUAL BLANKS

     C  N90SCRTCD        IFEQ      *BLANKS
     C                   SETON                                        70  90
     C                   ENDIF

      * RATE SET CODE MUST BE VALID

     C                   MOVEL     SCRTCD        ##RTCD

     C  N90RSKY          CHAIN     PRS01                              99
     C  N90
     CAN 99              SETON                                        71  90

      * RATE SET CODE MUST BE ACTIVE

     C   40
     CANN90RSSTAT        IFEQ      'I'
     C  N90
     CAN 99              SETON                                        75  90
     C                   ENDIF

      *  CLASS CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)

     C   40
     CANN90SCCLAS        IFEQ      *ZEROS
     C                   SETON                                        72  90
     C                   ENDIF

      *  EFFECTIVE DATE MUST BE VALID

     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT

     C     *IN90         IFEQ      *OFF

     C     SRERR         IFEQ      '1'

     C                   SETON                                        73  90

     C                   ENDIF

     C                   ENDIF

      * CHECK TO BE SURE CLASS CODE HAS NOT BEEN USED PREVIOUSLY

     C                   MOVEL     SCRTCD        ##RTCD
     C                   MOVE      SCCLAS        ##CLAS

     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##EFDT

     C  N90PRKY          CHAIN     PPR01                              99
     C  N90
     CANN99              SETON                                        74  90

      * PAYROLL CLASS CODE MUST BE ACTIVE

     C  N90PRKY1         SETGT     PPR01
     C  N90PRKY2         READPE    PPR01                                  99
     C  N90*IN99         DOWEQ     *OFF
     C     PRSTAT        ANDEQ     'I'
     C     PRKY2         READPE    PPR01                                  99
     C                   ENDDO
     C  N90
     CAN 99PRSTAT        IFEQ      'I'
     C  N90              SETON                                        76  90
     C                   ENDIF

      * WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN

     C                   ENDSR

      *================================================================
      *  ERCKW2- Error checking for window 2 (WDW02)
      *================================================================
     C     ERCKW2        BEGSR

      * NEW RATE SET CODE MUST NOT EQUAL BLANKS

     C  N90SCRTCN        IFEQ      *BLANKS
     C  N90              SETON                                        89  90
     C                   ENDIF

      * NEW RATE SET CODE MUST BE VALID

     C                   MOVEL     *BLANKS       ##RTCD
     C                   MOVEL     SCRTCN        ##RTCD

     C     *IN90         IFEQ      *OFF

     C     RSKY          CHAIN     PRS01                              99
     C   99              SETON                                        71  90
     C                   ENDIF

      * NEW EFFECTIVE DATE CAN NOT BE BLANK

     C  N90SCEFDN        IFEQ      *ZEROS
     C                   SETON                                        88  90
     C                   ENDIF

      * NEW EFFECTIVE DATE MUST BE VALID

     C     *IN90         IFEQ      *OFF

     C                   Z-ADD     SCEFDN        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDN

     C     SRERR         IFEQ      '1'
     C                   SETON                                        73  90
     C                   ENDIF

     C                   ENDIF

      * VERIFY THAT NO RECORDS EXIST FOR NEW RATE SET CODE AND DATE

     C     *IN90         IFEQ      *OFF

     C                   MOVEL     *BLANKS       ##EFDT
     C                   Z-ADD     SCEFDN        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##EFDT

     C                   ENDIF

     C     *IN90         IFEQ      *OFF
     C     PRKY4         CHAIN     PPR02                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        87  90
     C                   ENDIF

     C                   ENDIF

     C                   ENDSR

      *================================================================
      * KJEACT - Search subroutine for exposure act
      *================================================================
     C     KJEACT        BEGSR

     C     SCRTCD        IFNE      *BLANK
     C                   MOVEL     SCRTCD        ##RTCD
     C     RSKY          CHAIN     PRS01                              61
     C                   ELSE
     C                   MOVE      *ON           *IN61
     C                   ENDIF

     C     *IN61         IFEQ      *OFF
     C                   MOVE(P)   'XA'          XXTBID
     C                   MOVEL     RSJRST        XXTBID
     C                   Z-ADD     2             Y                 1 0
     C     GIKY          CHAIN     PGI01                              61
     C                   ENDIF

     C     *IN61         IFEQ      *ON
     C                   MOVEL(P)  '  XA'        XXTBID
     C                   Z-ADD     1             Y
     C                   ENDIF

     C                   MOVEL     PREACT        XXKEY

      *      Try again on generic table if not found in state table.
     C                   DO        Y
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD            1
     C     XXKEY         IFNE      *BLANK
     C                   LEAVE
     C                   ENDIF
     C                   MOVEL     '  XA'        XXTBID
     C                   ENDDO

     C                   MOVE      *ON           *IN92
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     XXKEY         SCEACT
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCENAM
     C                   MOVE      *ON           *IN93
     C                   MOVE      *OFF          *IN92
     C                   ELSE
     C                   MOVE      *ON           *IN92
     C                   MOVE      *BLANK        SCENAM
     C                   ENDIF
     C                   ENDIF

     C                   ENDSR

      *================================================================
      * KJTIER - Search subroutine for rating tier
      *================================================================
     C     KJTIER        BEGSR

     C     SCRTCD        IFNE      *BLANK
     C                   MOVEL     SCRTCD        ##RTCD
     C     RSKY          CHAIN     PRS01                              61
     C                   ELSE
     C                   MOVE      *ON           *IN61
     C                   ENDIF

     C     *IN61         IFEQ      *OFF
     C                   MOVE(P)   'RT'          XXTBID
     C                   MOVEL     RSJRST        XXTBID
     C                   Z-ADD     2             Y                 1 0
     C     GIKY          CHAIN     PGI01                              61
     C                   ENDIF

     C     *IN61         IFEQ      *ON
     C                   MOVEL(P)  '  RT'        XXTBID
     C                   Z-ADD     1             Y
     C                   ENDIF

     C                   MOVEL     PRTIER        XXKEY
      *      Try again on generic table if not found in state table.
     C                   DO        Y
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD            1
     C     XXKEY         IFNE      *BLANK
     C                   LEAVE
     C                   ENDIF
     C                   MOVEL     '  RT'        XXTBID
     C                   ENDDO

     C                   MOVE      *ON           *IN93
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     XXKEY         SCTIER
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCTNAM
     C                   MOVE      *ON           *IN86
     C                   MOVE      *OFF          *IN93
     C                   ELSE
     C                   MOVE      *ON           *IN93
     C                   MOVE      *BLANK        SCTNAM
     C                   ENDIF
     C                   ENDIF

     C                   ENDSR
      *================================================================
      * COPYSR - COPY FIELDS FROM SELECTED RECORD
      *================================================================
     C     COPYSR        BEGSR

     C                   MOVEL     *BLANKS       ##RTCD
     C                   MOVEL     *BLANKS       ##EFDT

     C                   MOVEL     SVRTC1        ##RTCD
     C                   MOVEL     SVEFD1        ##EFDT

     C     PRKY4         CHAIN     PPR02                              60
     C     *IN60         DOWEQ     *OFF

      *  Save fields for records with selected rate set codes and date

     C                   MOVEL     PRCO#         SVCO#
     C                   MOVEL     PRCLAS        SVCLAS

      *  New effective date

     C                   Z-ADD     SCEFDN        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SVEFD1

     C                   MOVEL     PRDESC        SVDESC
     C                   MOVEL     PRRT          SVRT
     C                   MOVEL     PRMNPR        SVMNPR
     C                   MOVEL     PRPCF         SVPCF
     C                   MOVEL     PRSTAT        SVSTAT
     C                   MOVEL     PRLHCD        SVLHCD
     C                   MOVEL     PRCCFL        SVCCFL

      *  New rate set code

     C                   MOVEL     SCRTCN        SVRTC1
     C                   MOVEL     PRHZGR        SVHZGR
     C                   MOVEL     PRSIC         SVSIC
     C                   MOVEL     PREACT        SVEACT
     C                   MOVEL     PRTIER        SVTIER
     C                   MOVEL     PREXDT        SVEXDT
     C                   MOVEL     PRXFER        SVXFER
     C                   MOVEL     PRGVEX        SVGVEX
     C                   MOVE      PRARFL        SVARFL
     C                   MOVE      PRSCCD        SVSCCD
     C     PRPCF         IFEQ      'O'
     C     POKY          CHAIN     PPO01                              60
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      SVRTC1        PORTCD
     C                   MOVE      SVEFD1        POEFDT
     C                   WRITE     PPO01
     C     POKY          READE     PPO01                                  60
     C                   ENDDO
     C                   ENDIF

      *  Clear record values

     C                   CLEAR                   PPR02

      *  Load saved values to new record

     C                   MOVEL     SVCO#         PRCO#
     C                   MOVEL     SVCLAS        PRCLAS
     C                   MOVEL     SVEFD1        PREFDT
     C                   MOVEL     SVDESC        PRDESC
     C                   MOVEL     SVRT          PRRT
     C                   MOVEL     SVMNPR        PRMNPR
     C                   MOVEL     SVPCF         PRPCF
     C                   MOVEL     SVSTAT        PRSTAT
     C                   MOVEL     Q1USER        PRUSER
     C                   MOVEL     Q1PGM         PRPGM

     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT

     C                   MOVEL     DSTIME        PRTIME

     C                   MOVEL     SVLHCD        PRLHCD
     C                   MOVEL     SVCCFL        PRCCFL
     C                   MOVEL     SVRTC1        PRRTCD
     C                   MOVEL     SVHZGR        PRHZGR
     C                   MOVEL     SVSIC         PRSIC
     C                   MOVEL     SVEACT        PREACT
     C                   MOVEL     SVTIER        PRTIER
     C                   MOVEL     SVEXDT        PREXDT
     C                   MOVEL     SVXFER        PRXFER
     C                   MOVEL     SVGVEX        PRGVEX
     C                   MOVE      SVARFL        PRARFL
     C                   MOVE      SVSCCD        PRSCCD

     C                   WRITE     DBRCDF

     C     PRKY4         READE     PPR02                                  60

     C                   ENDDO

     C                   RETURN

     C                   ENDSR
      *================================================================
      * MVDBSC - Move date base fileds to screen fields
      *================================================================
     C     MVDBSC        BEGSR

     C                   MOVEL     PRRTCD        SCRTCD
     C                   MOVEL     PRCLAS        SCCLAS
     C                   Z-ADD     PRLOSS        SCLOSS
     C                   Z-ADD     PREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT

     C     SVMODE        IFEQ      'R'

     C     @@MODE        IFEQ      'A'
     C                   MOVEL     SVRTCD        SCRTCD
     C                   CLEAR                   SCCLAS
     C                   Z-ADD     SVEFDT        SCEFDT
     C                   MOVE      *ON           *IN40
     C                   ELSE
     C  NKL              CLEAR                   SCEFDT
     C   KL              Z-ADD     SVEFDT        SCEFDT
     C                   ENDIF

     C     *IN90         DOUEQ     *OFF

     C                   EXFMT     WDW01

     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)

     C     *INKL         IFEQ      *ON
     C     *INKC         OREQ      *ON
     C                   RETURN
     C                   ELSE
     C                   EXSR      OTHFKS
     C                   EXSR      ERRCKW
     C                   ENDIF

     C                   ENDDO

     C                   CLEAR                   @@RRN
     C                   MOVEL     SCRTCD        SVRTCD
     C                   Z-ADD     SCEFDT        SVEFDT
     C                   MOVE      *ON           *IN21
     C                   MOVE      *OFF          *IN40

     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##EFDT

     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     PRSTAT        IFNE      'A'

     C     *IN60         DOWEQ     *OFF
     C     PRSTAT        ANDNE     'A'
     C     PRKY2         READPE    PPR01                                  60
     C                   ENDDO

     C                   ENDIF

     C     *IN60         IFEQ      *ON
     C                   CLEAR                   DBRCDF
     C                   MOVEL     'A'           PRSTAT
     C                   MOVE      *OFF          *IN21
     C                   ENDIF

     C                   ENDIF

     C                   MOVEL     PRDESC        SCDESC
     C   40              MOVEL     'A'           PRSTAT
     C                   MOVEL     PRSTAT        SCSTAT
     C                   MOVEL     PRRT          SCRT
     C                   MOVEL     PRMNPR        SCMNPR
     C                   MOVEL     PRPCF         SCPCF
     C                   If        PRLHCD = ' '
     C                   MOVEL     'N'           SCLHCD
     C                   Else
     C                   MOVEL     PRLHCD        SCLHCD
     C                   EndiF
     C                   If        PRCCFL = ' '
     C                   MOVEL     'N'           SCCCFL
     C                   Else
     C                   MOVEL     PRCCFL        SCCCFL
     C                   EndiF
     C                   MOVEL     PRHZGR        SCHZGR

     C                   MOVE      PREACT        SCEACT
     C                   MOVE      *BLANK        SCENAM

     C                   MOVEL     SCRTCD        ##RTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     RSJRST        XXTBID
     C                   ENDIF

     C                   MOVE      'XA'          XXTBID
     C                   MOVEL(P)  PREACT        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL(P)  '  XA'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C                   ENDIF
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCENAM
     C                   ENDIF

     C                   MOVE      PRTIER        SCTIER
     C                   MOVE      *BLANK        SCTNAM

     C                   MOVEL     SCRTCD        ##RTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     RSJRST        XXTBID
     C                   ENDIF

     C                   MOVE      'RT'          XXTBID
     C                   MOVEL(P)  PRTIER        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL(P)  '  RT'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C                   ENDIF
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCTNAM
     C                   ENDIF

     C                   MOVE      PRGVEX        SCGVEX
     C                   MOVE      PRARFL        SCARFL
     C                   MOVE      PRSCCD        SCSCCD
     C                   MOVE      PRLOGF        SCLOGF
     C                   MOVE      PROCCD        SCOCCD
     C                   MOVE      PRNRAT        SCNRAT
     C                   MOVE      PREXTER       SCEXTER
     C                   MOVE      PREXCLM       SCEXCLM

     C                   Z-ADD     PREXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT

     C                   Z-ADD     PRXFER        SCXFER

     C                   MOVE      PRLCM         SCLCM
     C                   Z-ADD     PRLCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCLCEX

     C                   ENDSR

      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR

     C                   Z-ADD     ##CO#         PRCO#
     C                   MOVEL     SCRTCD        PRRTCD
     C                   MOVE      SCCLAS        PRCLAS

     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PREFDT

     C                   MOVEL     SCDESC        PRDESC
     C                   MOVEL     SCSTAT        PRSTAT
     C                   MOVEL     SCRT          PRRT
     C                   MOVEL     SCMNPR        PRMNPR
     C                   MOVEL     SCPCF         PRPCF
     C                   MOVEL     SCLHCD        PRLHCD
     C                   MOVEL     SCCCFL        PRCCFL
     C                   MOVEL     SCHZGR        PRHZGR
     C                   MOVEL(P)  SCEACT        PREACT
     C                   MOVEL(P)  SCTIER        PRTIER
     C                   MOVE      SCGVEX        PRGVEX
     C                   MOVE      SCARFL        PRARFL
     C                   MOVE      SCSCCD        PRSCCD
     C                   MOVE      SCOCCD        PROCCD
     C                   MOVE      SCNRAT        PRNRAT
     C                   MOVE      SCEXTER       PREXTER
     C                   MOVE      SCEXCLM       PREXCLM
     C                   MOVE      SCLOGF        PRLOGF
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PREXDT
     C                   Z-ADD     SCXFER        PRXFER
     C                   Z-ADD     SCLOSS        PRLOSS

      * Add for Loss Cost Multiplier
     C                   MOVE      SCLCM         PRLCM
     C                   Z-ADD     SCLCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRLCEX

     C                   ENDSR
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR

     C     *INKJ         IFEQ      *ON

     C                   MOVE      *ON           *IN90
     C                   MOVEA     '00000'       *IN(70)

     C     HFLD          IFEQ      'SCRTCD'
     C     *IN40         ANDEQ     *ON

     C                   CALL      'W2471R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *OFF          ##KC              1
     C                   PARM      *OFF          ##KL              1
     C                   PARM      *BLANKS       @@KEY

     C     *INKC         IFEQ      *OFF

     C     *INKL         IFEQ      *ON
     C     @@KEY         OREQ      *BLANKS

     C                   SETON                                            68

     C                   ELSE

     C                   MOVEL     @@KEY         SCRTCD
     C                   SETON                                            69

     C                   ENDIF

     C                   ENDIF

     C                   SETON                                            90

     C                   ENDIF

     C     HFLD          IFEQ      'SCLCM'
     C                   CALL      'W2472R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *OFF          ##KC              1
     C                   PARM      *OFF          ##KL              1
     C                   PARM      RSJRST        ##ST              2
     C                   PARM      *BLANKS       @@KEY

     C     @@KEY         IFNE      *BLANKS
     C                   MOVE      *HIVAL        ZZEFDT
     C     LMKY2         SETGT     PLM01
     C     LMKY          READPE    PLM01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     LMLCM         SCLCM
     C                   SETON                                            69
     C                   SETON                                            90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF

     C     SCEFDT        IFNE      *ZERO
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C                   ENDIF

     C     HFLD          IFEQ      'SCEACT'
     C                   EXSR      KJEACT
     C                   ENDIF

     C     HFLD          IFEQ      'SCTIER'
     C                   EXSR      KJTIER
     C                   ENDIF

      *  Search on new rate set code field in WDW02

     C     HFLD          IFEQ      'SCRTCN'

     C                   CALL      'W2471R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *OFF          ##KC              1
     C                   PARM      *OFF          ##KL              1
     C                   PARM      *BLANKS       @@KEY

     C     *INKC         IFEQ      *OFF

     C     *INKL         IFEQ      *ON
     C     @@KEY         OREQ      *BLANKS
     C                   SETON                                            68

     C                   ELSE

     C                   MOVEL     @@KEY         SCRTCN
     C                   SETON                                            69
     C                   ENDIF

     C                   ENDIF
     C                   ENDIF

     C     HFLD          IFEQ      'SCEFDN'
     C                   MOVE      *ON           *IN61
     C                   ENDIF

     C                   ENDIF

     C                   ENDSR
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR

     C     SVMODE        IFEQ      'R'
     C     *INKL         ANDEQ     *ON

     C     @@MODE        IFEQ      'P'
     C                   MOVE      *OFF          *IN21
     C                   ENDIF

     C     *INKC         DOUEQ     *ON

     C                   EXSR      REFRSH

     C     *IN90         DOUEQ     *OFF

     C                   EXFMT     SCR01

     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)

     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     *INKE         CASEQ     *ON           KEDO
     C                   CAS                     OTHFKS
     C                   ENDCS

     C     *IN90         IFEQ      *OFF
     C     @@MODE        ANDNE     'D'
     C                   EXSR      ERRCK1
     C                   ENDIF

     C     *IN90         IFEQ      *OFF

     C     #OFSCR        IFEQ      1
     C                   EXSR      FINISH
     C                   ELSE
     C     @@MODE        CASEQ     'C'           UPDATR
     C     @@MODE        CASEQ     'D'           DSPLYR
     C                   CAS                     OTHERR
     C                   ENDCS
     C                   EXSR      XSCR02
     C                   ENDIF

     C                   ENDIF

     C                   ENDIF

     C                   ENDDO

     C                   ENDDO

     C                   ENDIF

     C     *IN09         IFEQ      *ON
     C     *INKC         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C     PRPCF         ANDEQ     'O'

     C                   CALL      'W0631R'                             99
     C                   PARM      *OFF          ##KC              1
     C                   PARM      *OFF          ##KL              1
     C                   PARM      '1'           ##CALL            1
     C                   PARM      @@RRN         ##RRN             9 0

     C                   ENDIF

     C                   ENDSR
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR

     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID
     C                   MOVEL     @@MODE        SVMODE            1
     C                   MOVE      *BLANK        XXPOP

     C     CLTID         IFEQ      '102'
     C                   SETON                                        32
     C                   ENDIF
     C     @@MODE        IFEQ      'R'

     C     @@RRN         IFEQ      *ZEROS
     C                   MOVEL     'A'           @@MODE
     C                   MOVE      *OFF          CPYFLG
     C                   ELSE
     C                   MOVEL     'P'           @@MODE
     C                   MOVE      *ON           CPYFLG
     C                   ENDIF

     C                   CLEAR                   SVRTCD
     C                   CLEAR                   SVEFDT
     C                   MOVE      *OFF          *IN21

     C                   ENDIF

     C     @@MODE        COMP      '1'                                    09
     C     @@MODE        COMP      '2'                                    09
     C     @@MODE        COMP      '3'                                    09
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40

     C                   MOVEL(P)  OPTNM         ##DATA

     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0

     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP

     C     SVMODE        IFEQ      'P'

     C                   MOVEL     *BLANKS       SCRTCN
     C                   MOVEL     *BLANKS       SCEFDN

     C                   EXSR      GETDTM

     C     @@RRN         CHAIN     DBRCDF                             60
     C     *IN60         IFEQ      *OFF

      *  MOVE PRRTCD AND PREFDT TO COPY WINDOW

     C                   MOVEL     PRRTCD        SCRTCD

     C                   Z-ADD     PREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT

     C                   MOVEL     *BLANKS       SVRTC1
     C                   MOVEL     *BLANKS       SVEFD1
     C                   MOVEL     PRRTCD        SVRTC1
     C                   MOVEL     PREFDT        SVEFD1

     C     *IN90         DOUEQ     *OFF

     C                   EXFMT     WDW02

     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)

     C     *INKL         IFEQ      *ON
     C     *INKC         OREQ      *ON
     C                   RETURN
     C                   ELSE
     C                   EXSR      OTHFKS
     C                   EXSR      ERCKW2
     C                   ENDIF

     C                   ENDDO

     C                   EXSR      COPYSR

     C                   ENDIF

     C                   ENDIF

     C                   ENDSR
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR

     C                   SELECT
     C     @@MODE        WHENEQ    'S'

     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL

     C                   ENDSR
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR

     C     @@RRN         CHAIN     DBRCDF                             99

B01  C     *IN99         IFEQ      *OFF

     C                   EXSR      MVSCDB

     C                   MOVE      *BLANKS       PRSIC
     C                   MOVEL     Q1USER        PRUSER
     C                   MOVEL     Q1PGM         PRPGM
     C                   TIME                    DSDTTM

     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT

     C                   MOVEL     DSTIME        PRTIME

     C                   UPDATE    DBRCDF
     C                   EXSR      GETDTM

     C                   ENDIF

     C     XXPOP         IFEQ      'Y'

     C                   CALL      'W0631R'                             99
     C     *INKC         PARM      *OFF          ##KC              1
     C     *INKL         PARM      *OFF          ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM      @@RRN         ##RRN             9 0

     C                   MOVE      *INKL         *IN90
     C     *INKC         IFEQ      *ON
     C                   RETURN
     C                   ENDIF

     C                   ENDIF

     C                   ENDSR
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR

     C                   CLEAR                   DBRCDF

     C                   EXSR      MVSCDB

     C                   MOVE      *BLANKS       PRSIC
     C                   MOVEL     Q1USER        PRUSER
     C                   MOVEL     Q1PGM         PRPGM
     C                   TIME                    DSDTTM

     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT

     C                   MOVEL     DSTIME        PRTIME

     C                   WRITE     DBRCDF
     C                   EXSR      GETDTM

     C                   FEOD      WMPRP
     C     SVPODT        IFNE      *ZERO
     C     POKY1         CHAIN     PPO01                              60
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     PREFDT        POEFDT
     C                   WRITE     PPO01
     C     POKY1         READE     PPO01                                  60
     C                   ENDDO
     C                   ENDIF

     C     XXPOP         IFEQ      'Y'

     C                   CALL      'W0631R'                             99
     C     *INKC         PARM      *OFF          ##KC              1
     C     *INKL         PARM      *OFF          ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM      DBRRN         ##RRN             9 0

     C                   MOVE      *INKL         *IN90
     C     *INKC         IFEQ      *ON
     C                   RETURN
     C                   ENDIF

     C                   ENDIF

     C                   ENDSR
