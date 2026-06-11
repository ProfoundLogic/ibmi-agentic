     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/20/12  C:190 Project : WC2126.                             *
     H*                 Add new prompt for "Cancel ID code override"  *
     H*                                                               *
     H* 08/17/11  C:190 PROJECT: WC2019                               *
     H*                 Add new fields                                *
     H*                 Number of days to consider policy new         *
     H*                 Cancellation notice days for new policies     *
     H*                 Mailing days for new policies                 *
     H*                                                               *
     F* 07/13/11  C:061 PROJECT: WC2037                               *
     F*                 Add new fields                                *
     F*                   Unearned premium surcharge percentage       *
     F*                   Unearned premium surcharge stat code        *
     H*                                                               *
     H* 08/15/07 C:61  Project: WC1446 PRISS 4                        *
     H*                Condition display of instruction text upon     *
     H*                Auto reinstatement flag. Change to display     *
     H*                file only.                                     *
     H*                                                               *
     H* 02/02/07 C:33  Project: WC1226   WA139/51893                  *
     H*                Percentage required if, Auto reinstatement flag*
     H*                 set.                                          *
     H*                                                               *
     H* 09/25/06 C:132 Project: WC1446                                *
     H*                Auto-Reinstatement/Cancellation Billings       *
     H*                Through Date Flag.                             *
     H*                                                               *
     H* 06/24/05 C:33  Project: 109WDZ                                *
     H*                {109BM}                                        *
     H*                Add new entries for Auto CX confirmation       *
     H*                                                               *
     H* 03/08/05 C:132 PROJECT: WC1226                                *
     H*              - Check for Auto Reinstatement system flag, and  *
     H*                if on, display additional fields on screen.    *
     H*              - Add edits on additional Auto reinstatement     *
     H*                fields if these fields appear.                 *
     H*                                                               *
     H* 11/01/01 C:61  SUPPORT WA112 29380                            *
     H*              - Validate NCCI CX code entered.                 *
     H*              - Correct searches.                              *
     H*                                                               *
     H* 11/15/00 C:61  PROJECT WC577                                  *
     H*              - Condition use of *IN90 for CHAIN in ERRCK1.    *
     H*                Otherwise allows input of blank records.
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
      *                                                               *
      * 05/27/99  C:87  PROJECT: WC577                                *
      *                 Recompiled pgmr due to chg in dspf.  Edit     *
      *                 code was added to 'Reinstatement fee' field.  *
      *                                                               *
      * 11/10/98  C:87  PROJECT: WC852                                *
      *                 Added fields to screen: State(rqrd), Effective*
      *                 date(rqrd), Canx notice days(rqrd), Additional*
      *                 days for mailing(opt), Reinstatement fee(opt),*
      *                 and Cert notification flag(opt).  Also, added *
      *                 appropriate processing.                       *
      *                                                               *
      * 04/16/98  C:35  PROJECT: WC577                                *
      *                 ADD NCCI CODE DESCRIPTION TO SCREEN (DISPLAY  *
      *                 FIELD ONLY).                                  *
      *                                                               *
      *****************************************************************
     FW2196F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTCRP     UF A E             DISK
     F                                     RENAME(PCR01:DBRCDF)
     F***  BEGIN CHG  ***  11/10/98  **********************************
     F***WTABL   IF  E           K        DISK
     FWTCRL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     F***  END   CHG  ***  11/10/98  **********************************
      ***** BEG ADD ******* 11/01/01 **********************************
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
      ***** END ADD ******* 11/01/01 **********************************
      *
      *
     FWTGTL     IF   E           K DISK
      *
      ***** BEG ADD ***** 03/08/05 ************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SP201)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SSY01)
     F********    SWC01                             KIGNORE
      ***** END ADD ***** 03/08/05 ************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Cancellation Reason-
     D                                      Master Information')
     D COMAPI          C                   CONST('WA001R')
     D***  Begin Chg  ***  06/24/05  **************************************
     D**********    1                     C         #OFSCR
     D #OFSCR          C                   CONST(2)

     I***  End   Chg  ***  06/24/05  **************************************

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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     CRKY          KLIST
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C*******************  KFLD           ##CO#
     C*******************  KFLD           ##KEY
     C*
     C                   KFLD                    ##CO#
     C                   KFLD                    KKST
     C                   KFLD                    ##KEY
     C                   KFLD                    KKEFDT
     C***  END   CHG  ***  11/10/98  **********************************
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C                   KFLD                    ##KEY
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C*  State code file key list
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C***  END   ADD  ***  11/10/98  **********************************
      *
      ***** BEG ADD ***** 03/08/05 ************************************
     C*  Auto Reinstate key list
     C     WCKY          KLIST
     C                   KFLD                    ##WCK
      ***** END ADD ***** 03/08/05 ************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CRCO#         ##CO#
     C     *LIKE         DEFINE    CRKEY         ##KEY
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    CRKEY         @@KEY
     C     *LIKE         DEFINE    GTTBID        @@ID
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     *LIKE         DEFINE    CRST          KKST
     C     *LIKE         DEFINE    CREFDT        KKEFDT
     C     *LIKE         DEFINE    CRKEY         KKKEY
     C***  END   ADD  ***  11/10/98  **********************************
      *
      ***** BEG ADD ***** 03/08/05 ************************************
     C     *LIKE         DEFINE    WCKEY         ##WCK
      ***** END ADD ***** 03/08/05 ************************************
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
     C     CRSTAT        IFEQ      'I'
     C                   MOVE      'A'           CRSTAT
     C                   ELSE
     C                   MOVE      'I'           CRSTAT
     C                   ENDIF                                                  CRSTAT
     C*
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C*
     C                   MOVEL     DSTIME        CRTIME
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
     C* CX REASON CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C*  Validate state code entered
     C   40
     CANN90SCST          IFEQ      *BLANKS
     C                   SETON                                        80  90
     C                   ELSE
     C                   MOVEL     SCST          KKKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        81  90
     C                   ENDIF
     C*
     C*  Validate effective date entered
     C   40
     CANN90SCEFDT        IFEQ      *ZERO
     C                   SETON                                        82  90
     C                   ELSE
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCEFDT
     C                   ELSE
     C                   SETON                                        83  90
     C                   ENDIF                                                  SCERR=*OFF
     C                   ENDIF                                                  SCEFDT=*ZERO
     C***  END   ADD  ***  11/10/98  **********************************
     C*
     C* CHECK TO BE SURE CX REASON CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         ##KEY
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   MOVE      SCST          KKST
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C***  END   ADD  ***  11/10/98  **********************************
     C*
     C   40
     CANN90CRKY          CHAIN     PCR01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* CX REASON CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDSC1        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
      ***** BEG DEL ******* 11/01/01 **********************************
     C*********
     C**NCCI*CODE IS REQUIRED AND MUST BE VALID
     C*********
     C*****N90*  SCNCCI    IFEQ *BLANK
     C*********            SETON                     7490
     C*********            END
     C*********
     C*********            MOVEL'CXCD'    ##TBID
     C*********            MOVELSCNCCI    ##KEY     P
     C*********
     C******BEG CHG ******* 11/15/00 **********************************
     C*********  GTKY      CHAINPGT01                7490
     C**N90****  GTKY      CHAINPGT01                7490
     C******END CHG ******* 11/15/00 **********************************
      ***** END DEL ******* 11/01/01 **********************************
      ***** BEG ADD ******* 11/01/01 **********************************
      *
      * NCCI Code must be valid
      *
     C  N90SCNCCI        IFEQ      *BLANK
     C                   SETON                                        7490
     C                   MOVEL(P)  *BLANKS       SCNDSC
      *
     C                   ELSE
      *
     C                   MOVEL     'CXCD'        ##TBID
     C                   MOVEL(P)  SCNCCI        ##KEY
     C     GTKY          CHAIN     PGT01                              74
     C   74              SETON                                        90
     C  N74              MOVEL(P)  GTDESC        SCNDSC
     C                   ENDIF
      *
      ***** END ADD ******* 11/01/01 **********************************
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C*  Validate cancellation notice days
     C  N90SCNOTC        IFLT      *ZERO
     C                   SETON                                        84  90
     C                   ENDIF
     C*
     C*  Additional days.. cannot be less than zero
     C  N90SCADDM        IFLT      *ZERO
     C                   SETON                                        85  90
     C                   ENDIF
      ***** BEG ADD ******* 08/17/11 **********************************
     C*  Number of days to consider policy new
     C  N90SCNDNW        IFNE      *ZERO
     C     SCNDNW        ANDLT     *ZERO
     C                   SETON                                        53  90
     C                   ENDIF
     C*
     C*  New policy cancellation days
     C  N90SCNDNW        IFNE      *ZERO
     C     SCNTNP        ANDEQ     *ZERO
     C     SCNDNW        OREQ      *ZERO
     C     SCNTNP        ANDNE     *ZERO
     C                   SETON                                        55  90
     C                   ENDIF
      *
     C*  If entered, value should be greater than zero
     C  N90SCNTNP        IFNE      *ZERO
     C     SCNTNP        ANDLT     *ZERO
     C                   SETON                                        54  90
     C                   ENDIF
     C*
     C*  New policy additional mailing days
     C  N90SCNDNW        IFEQ      *ZERO
     C     SCMDNP        ANDNE     *ZERO
     C                   SETON                                        57  90
     C                   ENDIF
     C*  If entered, value should be greater than zero
     C  N90SCMDNP        IFNE      *ZERO
     C     SCMDNP        ANDLT     *ZERO
     C                   SETON                                        56  90
     C                   ENDIF
      ***** END ADD ******* 08/17/11 **********************************
     C*
     C*  Reinstatement fee cannot be less than zero
     C  N90SCRFEE        IFLT      *ZERO
     C                   SETON                                        86  90
     C                   ENDIF
     C*
     C*  Check certified notification equals 'Y' or blank
      ***** BEG CHG ******* 11/15/00 **********************************
     C*********  SCCERT    IFNE 'Y'
     C  N90SCCERT        IFNE      'Y'
      ***** END CHG ******* 11/15/00 **********************************
     C     SCCERT        ANDNE     ' '
     C                   SETON                                        87  90
     C                   ENDIF
     C***  END   ADD  ***  11/10/98  **********************************
      *
      ****** BEG DEL ***** 09/25/06 ***********************************
      ******BEG*ADD ***** 03/08/05 ************************************
      **If*Auto*Reinstatement flag on do edits
     C********** *IN45     IFEQ *ON
      **********
      **If*Flag*<> "Y" and NOT blank, is an error
     C**N90***** SCAREI    IFNE *BLANKS
     C********** SCAREI    ANDNE'Y'
     C**********           SETON                     21  90
     C**********           ENDIF                           CRAREI <> ZEROS
      **********
      **If*Flag*= "Y", and days <> zeros, must have a percentage entered.
     C**N90***** SCAREI    IFEQ 'Y'
     C********** SCRDAY    ANDNE*ZEROS
     C********** SCPCNT    ANDLE*ZEROS
     C**********           SETON                     22  90
     C**********           ENDIF                           CRAREI <> ZEROS
      **********
      **If*Flag*<> "Y" and days <> zeros, is an error
     C**N90***** SCAREI    IFEQ *BLANKS
     C********** SCRDAY    ANDNE*ZEROS
     C**********           SETON                     23  90
     C**********           ENDIF                           CRAREI <> ZEROS
      **********
      **If*Flag*<> "Y" and Percentage <> zeros, is an error
     C**N90***** SCAREI    IFEQ *BLANKS
     C********** SCPCNT    ANDNE*ZEROS
     C**********           SETON                     24  90
     C**********           ENDIF                           CRAREI <> ZEROS
      **********
     C**********           ENDIF                           *IN45 = *ON
      ******END*ADD ***** 03/08/05 ************************************
      ****** END DEL ***** 09/25/06 ***********************************
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90CRDT          IFGT      SAVEDT
     C     CRDT          OREQ      SAVEDT
     C     CRTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
      *
      ****** BEG ADD ***** 09/25/06 ***********************************
      * If Auto Reinstatement flag on do edits
     C     *IN45         IFEQ      *ON
      *
      * If Flag <> "Y" and NOT blank, is an error
     C  N90SCAREI        IFNE      *BLANKS
     C     SCAREI        ANDNE     'Y'
     C                   SETON                                        21  90
     C                   ENDIF                                                  CRAREI <> ZEROS
      *
      * If Flag = "Y", and days <> zeros, must have a percentage entered.
     C  N90SCAREI        IFEQ      'Y'
     C***  Begin Add  ***  02/02/07  **************************************
     C********** SCRDAY    ANDNE*ZEROS
     C***  End   Add  ***  02/02/07  **************************************
     C     SCPCNT        ANDLE     *ZEROS
     C                   SETON                                        22  90
     C                   ENDIF                                                  CRAREI <> ZEROS
      *
      * If Flag <> "Y" and days <> zeros, is an error
     C  N90SCAREI        IFEQ      *BLANKS
     C     SCRDAY        ANDNE     *ZEROS
     C                   SETON                                        23  90
     C                   ENDIF                                                  CRAREI <> ZEROS
      *
      * If Flag <> "Y" and Percentage <> zeros, is an error
     C  N90SCAREI        IFEQ      *BLANKS
     C     SCPCNT        ANDNE     *ZEROS
     C                   SETON                                        24  90
     C                   ENDIF                                                  CRAREI <> ZEROS
      *
      * If Flag <> "I", "C", "E" or BLANK, is an error
     C  N90SCAREI        IFEQ      'Y'
     C     SCICE         ANDNE     'I'
     C     SCICE         ANDNE     'C'
     C     SCICE         ANDNE     'E'
     C                   SETON                                        25  90
     C                   ENDIF                                                  CRAREI <> ZEROS
      *
     C  N90SCAREI        IFEQ      'Y'
     C     SCICE         ANDEQ     *BLANK
     C                   SETON                                        26  90
     C                   ENDIF                                                  CRAREI <> ZEROS
      *
     C  N90SCAREI        IFEQ      *BLANK
     C     SCICE         ANDNE     *BLANK
     C                   SETON                                        27  90
     C                   ENDIF                                                  CRAREI <> ZEROS
      *
     C                   ENDIF                                                  *IN45 = *ON
      *
      ****** END ADD ***** 09/25/06 ***********************************
      *
     C***  Begin Add  ***  06/24/05  **************************************
      *
      * Auto CX Confirm Flag must be (Y/ )
      *
     C  N90SCACNF        IFNE      'Y'
     C     SCACNF        ANDNE     *BLANK
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCACNF
      *
      *  If reinstatement flag = 'Y', number of CX confirm grace period
      *  days must be greater than or equal to grace period for auto
      *  reinstatement.
      *
     C  N90SCAREI        IFEQ      'Y'
     C     SCACNF        ANDEQ     'Y'
     C     SCCDAY        ANDLT     SCRDAY
     C                   SETON                                        76  90
     C                   ENDIF                                                  SCCDAY
      *
      * Update audit tracking must be (Y/ )
      *
     C  N90SCATRK        IFNE      'Y'
     C     SCATRK        ANDNE     *BLANK
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCATRK
      *
      * Print certificate holders letter must be (Y/ ).
      *
     C  N90SCPCHL        IFNE      'Y'
     C     SCPCHL        ANDNE     *BLANK
     C                   SETON                                        75  90
     C                   ENDIF                                                  SCPCHL
      *
     C     SCPCHL        IFEQ      *BLANK
     C                   MOVE      'N'           SCPCHL
     C                   ENDIF                                                  SCPCHL
     C***  End   Add  ***  06/24/05  **************************************
     C***  BEG   ADD  ***  02/20/12  **************************************
     C  N90SCIDOV        IFNE      *BLANKS
     C                   MOVEL     'CXID'        ##TBID
     C                   MOVEL(P)  SCIDOV        ##KEY
     C     GTKY          CHAIN     PGT01                              69
     C   69              SETON                                        90
     C  N69              MOVEL(P)  GTDESC        SCOVDS
     C                   ENDIF                                                  SCIDOV
     C***  END   ADD  ***  02/20/12  **************************************
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
      ***** BEG ADD ******* 11/01/01 **********************************
      *
      *
      *
      *================================================================
      * KJDO - Searches
      *================================================================
     C     KJDO          BEGSR
      * zzz
     C                   SELECT
      *
      *  NCCI Reason Code
      *
     C     HFLD          WHENEQ    'SCST'
     C     *IN40         ANDEQ     *ON
     C                   CALL      'SY601R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                        51
      *
     C                   ELSE
      *
     C     ##RRN         CHAIN     PST02                              60
     C                   MOVEL     STKEY         SCST
     C                   SETON                                        52
     C                   ENDIF
      *
      *  NCCI Reason Code
      *
     C     HFLD          WHENEQ    'SCNCCI'
     C                   MOVEL     OPTNM         @@PRDS
     C                   MOVEL     'CXCD'        @@ID
     C                   MOVE      *BLANKS       @@KEY
     C                   MOVE      *BLANKS       @@RTC
     C                   CALL      'W2493R'
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
     C                   PARM                    @@ID
     C                   PARM                    @@KEY
     C                   PARM                    @@RTC             1
      *
     C     @@KEY         IFEQ      *BLANKS
     C                   SETON                                        75
      *
     C                   ELSE
      *
     C                   SETON                                        50
     C                   MOVEL     @@KEY         SCNCCI
     C                   MOVEL     'CXCD'        ##TBID
     C                   MOVEL(P)  SCNCCI        ##KEY
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCNDSC
     C                   ENDIF                                                  @@KEY
      *
      ***** BEG ADD ******* 02/20/12 **********************************
     C     HFLD          WHENEQ    'SCIDOV'
     C                   MOVEL     OPTNM         @@PRDS
     C                   CALL      'W2493R'
     C                   PARM                    @@PRDS           40
     C                   PARM      *BLANK        @@CALL            1
     C                   PARM      'CXID'        @@ID
     C                   PARM      *BLANKS       @@KEY
     C                   PARM      *BLANKS       @@RTC             1
      *
     C     @@KEY         IFNE      *BLANKS
     C                   SETON                                        68
     C                   MOVEL     @@KEY         SCIDOV
     C                   MOVEL     'CXID'        ##TBID
     C                   MOVEL(P)  SCIDOV        ##KEY
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCOVDS
     C                   ENDIF                                                  @@KEY
      *
     C                   ENDSL
      ***** END ADD ******* 02/20/12 **********************************
      *
     C                   ENDSR                                                  KJDO
      ***** END ADD ******* 11/01/01 **********************************
      *
      *
      *
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     CRKEY         SCKEY
     C                   MOVEL     CRDESC        SCDSC1
     C                   MOVE      CRDESC        SCDSC2
     C                   MOVEL     CRSTAT        SCSTAT
     C                   MOVEL     CRNCCI        SCNCCI
      ***** BEG ADD ******* 07/13/11 **********************************
     C                   Z-ADD     CRUPSP        SCUPSP
     C                   Z-ADD     CRUPSC        SCUPSC
      ***** END ADD ******* 07/13/11 **********************************
      ***** BEG ADD ******* 08/17/11 **********************************
     C                   Z-ADD     CRNDNW        SCNDNW
     C                   Z-ADD     CRNTNP        SCNTNP
     C                   Z-ADD     CRMDNP        SCMDNP
      ***** END ADD ******* 08/17/11 **********************************
      ***** BEG ADD ******* 02/20/12 **********************************
     C                   MOVEL     CRIDOV        SCIDOV
     C                   MOVEL     'CXID'        ##TBID
     C                   MOVEL(P)  SCIDOV        ##KEY
      *
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCOVDS
     C                   ELSE
     C                   MOVE      *BLANKS       SCOVDS
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      ***** END ADD ******* 02/20/12 **********************************
      **** BEGIN ADD **** 04/16/98 ************************************
     C                   MOVEL     'CXCD'        ##TBID
     C                   MOVEL(P)  SCNCCI        ##KEY
      *
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCNDSC
     C                   ELSE
     C                   MOVE      *BLANKS       SCNDSC
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      **** END   ADD **** 04/16/98 ************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVE      *BLANKS       SCDSC1
     C                   MOVE      *BLANKS       SCDSC2
     C                   MOVEL     'A'           SCSTAT
     C                   MOVEL     *BLANKS       SCNCCI
      ***** BEG ADD ******* 02/20/12 **********************************
     C                   MOVEL     *BLANKS       SCIDOV
     C                   MOVE      *BLANKS       SCOVDS
      ***** END ADD ******* 02/20/12 **********************************
      ***** BEG ADD ******* 07/13/11 **********************************
     C                   CLEAR                   SCUPSP
     C                   CLEAR                   SCUPSC
      ***** END ADD ******* 07/13/11 **********************************
      ***** BEG ADD ******* 08/17/11 **********************************
     C                   CLEAR                   SCNDNW
     C                   CLEAR                   SCNTNP
     C                   CLEAR                   SCMDNP
      ***** END ADD ******* 08/17/11 **********************************
      **** BEGIN ADD **** 04/16/98 ************************************
     C                   MOVE      *BLANKS       SCNDSC
      **** END   ADD **** 04/16/98 ************************************
     C*
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   MOVE      CRST          SCST
     C                   Z-ADD     CREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   Z-ADD     CRNOTC        SCNOTC
     C                   Z-ADD     CRADDM        SCADDM
     C                   Z-ADD     CRRFEE        SCRFEE
     C***  END   ADD  ***  11/10/98  **********************************
      *
      ***** BEG ADD ***** 03/08/05 ************************************
     C                   MOVE      CRAREI        SCAREI
     C                   Z-ADD     CRRDAY        SCRDAY
     C                   Z-ADD     CRPCNT        SCPCNT
      ***** END ADD ***** 03/08/05 ************************************
     C***  Begin Add  ***  06/24/05  **************************************
     C                   MOVE      CRACNF        SCACNF
     C                   Z-ADD     CRCDAY        SCCDAY
     C                   MOVE      CRATRK        SCATRK
     C                   MOVE      CRPCHL        SCPCHL
     C*
     C     SCPCHL        IFEQ      'N'
     C                   MOVE      *BLANK        SCPCHL
     C                   ENDIF                                                  SCPCHL
     C***  End   Add  ***  06/24/05  **************************************
      *
      ****** BEG ADD ***** 09/25/06 ***********************************
     C                   MOVE      CRICE         SCICE
      ****** END ADD ***** 09/25/06 ***********************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         CRCO#
     C                   MOVEL     SCKEY         CRKEY
     C                   MOVEL     SCDSC1        CRDESC
     C                   MOVE      SCDSC2        CRDESC
     C                   MOVEL     SCSTAT        CRSTAT
     C                   MOVEL     SCNCCI        CRNCCI
      ***** BEG ADD ******* 07/13/11 **********************************
     C                   Z-ADD     SCUPSP        CRUPSP
     C                   Z-ADD     SCUPSC        CRUPSC
      ***** END ADD ******* 07/13/11 **********************************
      ***** BEG ADD ******* 08/17/11 **********************************
     C                   Z-ADD     SCNDNW        CRNDNW
     C                   Z-ADD     SCNTNP        CRNTNP
     C                   Z-ADD     SCMDNP        CRMDNP
      ***** END ADD ******* 08/17/11 **********************************
      ***** BEG ADD ******* 02/20/12 **********************************
     C                   MOVEL     SCIDOV        CRIDOV
      ***** END ADD ******* 02/20/12 **********************************
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   MOVE      SCST          CRST
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CREFDT
     C                   Z-ADD     SCNOTC        CRNOTC
     C                   Z-ADD     SCADDM        CRADDM
     C                   Z-ADD     SCRFEE        CRRFEE
     C                   MOVE      SCCERT        CRCERT
     C***  END   ADD  ***  11/10/98  **********************************
      *
      ***** BEG ADD ***** 03/08/05 ************************************
     C     *IN45         IFEQ      *ON
     C                   MOVE      SCAREI        CRAREI
     C                   Z-ADD     SCRDAY        CRRDAY
     C                   Z-ADD     SCPCNT        CRPCNT
      *
      ****** BEG ADD ***** 09/25/06 ***********************************
     C                   MOVE      SCICE         CRICE
      ****** END ADD ***** 09/25/06 ***********************************
     C                   ENDIF
      *
      ***** END ADD ***** 03/08/05 ************************************
     C***  Begin Add  ***  06/24/05  **************************************
     C                   MOVE      SCACNF        CRACNF
     C                   Z-ADD     SCCDAY        CRCDAY
     C                   MOVE      SCATRK        CRATRK
     C                   MOVE      SCPCHL        CRPCHL
     C***  End   Add  ***  06/24/05  **************************************
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
      ***** BEG ADD ******* 11/01/01 **********************************
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90     Searches
     C                   ENDCS
      *
      ***** END ADD ******* 11/01/01 **********************************
      ***** BEG DEL ******* 11/01/01 **********************************
      **********
     C********** *INKJ     IFEQ *ON
     C**********
     C*****BEGIN ADD  ***  11/10/98  **********************************
     C********** HFLD      IFEQ 'SCNCCI'
     C*****END** ADD  ***  11/10/98  **********************************
     C**********
     C**********           MOVELOPTNM     @@PRDS
     C**********           MOVEL'CXCD'    @@ID
     C**********           MOVE *BLANKS   @@KEY
     C**********           MOVE *BLANKS   @@RTC
     C**********
     C**********           CALL 'W2493R'
     C**********           PARM           @@PRDS 40
     C**********           PARM           @@CALL  1
     C**********           PARM           @@ID
     C**********           PARM           @@KEY
     C**********           PARM           @@RTC   1
     C**********
     C********** @@KEY     IFNE *BLANKS
     C**********
     C**********           MOVEL@@KEY     SCNCCI
      *****BEGIN ADD **** 04/16/98 ************************************
     C**********           MOVEL'CXCD'    ##TBID
     C**********           MOVELSCNCCI    ##KEY     P
      **********
     C********** GTKY      CHAINPGT01                60
     C********** *IN60     IFEQ *OFF
     C**********           MOVELGTDESC    SCNDSC
     C**********           ELSE
     C**********           MOVE *BLANKS   SCNDSC
     C**********           ENDIF                           *IN60 IFEQ *OFF
      *****END** ADD **** 04/16/98 ************************************
     C**********
     C**********           ENDIF                           @@KEY
     C**********
     C**********           SETON                     7590
     C**********
     C*****BEGIN ADD  ***  11/10/98  **********************************
     C**********           ENDIF                           HFLD='SCNCCI'
     C**********
     C**********           SETON                     90
     C*****END** ADD  ***  11/10/98  **********************************
     C**********
     C**********           ENDIF                           *INKJ
      ***** END DEL ******* 11/01/01 **********************************
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
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     @@MODE        COMP      'C'                                    41    CHANGE
     C***  END   ADD  ***  11/10/98  **********************************
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
      *
      ***** BEG ADD ***** 03/08/05 ************************************
     C*
     C* CHECK THAT AUTO REINSTALL IS INSTALLED, ELSE DON'T DISPLAY FLDS
     C*
     C                   MOVEL     'WC'          ##WCK
     C*
     C     WCKY          CHAIN     SWC01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     WCFL29        IFNE      *BLANKS
     C                   MOVE      *ON           *IN45
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  SYFL01
      ***** END ADD ***** 03/08/05 ************************************
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
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C*
     C                   MOVEL     DSTIME        CRTIME
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
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C*
     C                   MOVEL     DSTIME        CRTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTCRP
     C*
     C                   ENDSR
