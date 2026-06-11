     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     h bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0040R02 - AGENT MASTER INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE AGENT MASTER FILE.       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0040R - WORK WITH AGENTS                                  *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W0041R02 - AGENT ASSIGNMENT INFORMATION                    *
     H*    W0042R   - WORK WITH PRODUCERS                             *
     H*    SC010R   - DARTS                                           *
     H*                                                               *
     H*  CREATED: 11/10/97  PROGRAMMER: C:33  PROJECT: WC683          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  07/31/24 C:79 Ticket 140/117070                              *
     H*                Use different indicator for Agent Group Code   *
     H*                                                               *
     H*  01/16/23 c:62 Project WC2838                                 *
     H*                Add FTP Directory                              *
     H*                                                               *
     H*  11/16/22 c:65 Project WC2838                                 *
     H*                Define(WEB) was lost when program went to      *
     H*                SQLRPG so remove the tag around handler.       *
     H*                                                               *
     H*  10/03/22 c:62 Project WC2838                                 *
     H*                Add Email Information                          *
     H*                                                               *
     h*  07/02/15 C:306 Support: SEQ86962 / 31805                    *
     h*                 Added the email validation check             *
     h*
     H* 07/14/14 C:295 Project: CI103                                 *
     H*                Update Agent details to WCMS to FQCIC          *
     H*                                                               *
     H* 04/21/14 C:283 Support WA045 83386                            *
     H*                Re-Compile. Corrected the fields Phone number  *
     H*                1, 2 and Fax Number to show leading zeros.     *
     H*                                                               *
     H* 06/13/13 C:180 SUPPORT WA146 82519                            *
     H*                When an agent is deactivated and its mail-to   *
     H*                modifier value is made the same as the         *
     H*                agent's modifier, do not allow active agents   *
     H*                with the same FEIN to have their mail-to       *
     H*                modifiers changed to that of the deactivated   *
     H*                agent.                                         *
     H*
     H*                                                               *
     H* 01/23/13 C:33  Project: SY266  #2251                          *
     H*                Save RRN after writing record.                 *
     H*                                                               *
     H* 12/27/11 C:282 Project: WC2005                                *
     H*                Add API WA085R to change EFT payee name/FEIN   *
     H*                                                               *
     H* 09/01/09 C:220 SUPPORT: 69725                                 *
     H*                CONDITION WRITER SR AS PGM W0041R02 CALL HAS.  *
     H*                                                               *
     H* 04/16/08 C:136 PROJECT: 93WHG                                 *
     H*                ADD MARKETING REGION FIELD                     *
     H*                                                               *
     H* 03/27/08 C:61  PROJECT WC1532                                 *
     H*              - If '1099 mailed to modifier' has changed then  *
     H*                pop up window to inquire if change should be   *
     H*                copied to all modifiers for the same agent     *
     H*                FEIN.                                          *
     H*              - If 'Checks mailed to modifier' has changed then*
     H*                pop up window to inquire if change should be   *
     H*                copied to all modifiers for the same agent     *
     H*                FEIN.                                          *
     H*                                                               *
     H* 02/12/08 C:191 Seq#: 62227                                    *
     H*                Changes in display file only. No change in PGM *
     H*                                                               *
     H* 11/26/07 C:62  PRJ#: 114MPE                                   *
     H*                Add exclude from export flag processing.       *
     H*                                                               *
     H* 10/30/06 C:53  Seq#: 54438                                    *
     H*                Correct mailing modifier issues                *
     H*                Correct lost data due to file chains           *
     H*                                                               *
     H* 05/23/06 C:33  Seq#: WA091/52382                              *
     H*                Check for inactive agent modifiers being       *
     H*                 assigned as a 'Mailed to' mod.                *
     H*                                                               *
     H*  05/05/06  C:132 Project: 135WEW                              *
     H*                - Added Agent Short Name field - AGSHNM        *
     H*                - Move Agency services rep and Marketing Re p  *
     H*                  to second screen.                            *
     H*                                                               *
     H*  03/03/06  C:167 Project: 114WLP                              *
     H*                  Added Background Check Completion            *
     H*                  date field to Agent Master. Added            *
     H*                  Background Check Completion date             *
     H*                  to screen SCR02.                             *
     H*                  *** ONLY for client 114                      *
     H*                                                               *
     H* 12/29/05 C:168 PROJECT 114WKZ                                 *
     H*              - Replaced the following fields in the screen    *
     H*                *** ONLY for client 114                        *
     H*                Secretary Name -> E/O Coverage                 *
     H*                Marketing Contact ->  E/O Coverage Expiration  *
     H*                                                               *
     H* 11/08/05 C:132 PROJECT: BASECD                                *
     H*              - Add edit for SS# when missing a "Y" in 1099    *
     H*                field. add edit for invalid County Code in     *
     H*                change mode.                                   *
     H*                                                               *
     H* 06/02/05 C:132 PROJECT WA093  47445                           *
     H*              - Expanded Agent type code incorrect, using old  *
     H*                two position instead of three when loaded      *
     H*                from the file to screen.                       *
     H*                                                               *
     H* 05/03/05 C:132 PROJECT WA093  46556                           *
     H*              - In Add Mode, if no record in ZIP BY COUNTY  *
     H*                do not display error message.                  *
     H*              - Blank out Agent Type Code Description before   *
     H*                Chain to Agent Type Code file.                 *
     H*                                                               *
     H* 04/15/05 C:132 PROJECT WA071  46556                           *
     H*                In CHANGE Mode, if no record in ZIP BY COUNTY  *
     H*                do not display error message.                  *
     H*                                                               *
     H* 03/16/05 C:132 PROJECT WC1118                                 *
     H*                In ADD/CHG Mode, Check ZIP BY COUNTY file for  *
     H*                valid county code on second screen, even if    *
     H*                Blanked out.                                   *
     H*                                                               *
     H* 12/01/04 C:132 PROJECT WC1232                                 *
     H*                ADD new fields with edits.                     *
     H*                AGTPC2 - Expanded Agent Type Code replaces the *
     H*                         existing AGTPCD (2A to 3A).           *
     H*                AGGRCD - Agent Group Code - New File for this  *
     H* ==== ** ==== ** ==== **                                       *
     H*        NOTICE: New file WTAGP/L needed for above changes, and *
     H*                new programs W2167F/R, W2167F02/R02 must go    *
     H*                with project WC1232 changes.                   *
     H*                FIXAGTTPC is also needed to put old AGTPCD in  *
     H*                the new field AGTPC2.                          *
     H* ==== ** ==== ** ==== **                                       *
     H*                                                               *
     H* 05/06/03 C:132 PROJECT WC1118                                 *
     H*                In ADD/CHG Mode, Check ZIP BY COUNTY file for  *
     H*                valid county code, and load into SCCNCD on the *
     H*                second screen. Get from file WMCZP. If CHANGE  *
     H*                mode, only do if AGCNTY County Code is BLANK   *
     H*                                                               *
     H* 02/26/02 C:61  PROJECT WC577                                  *
     H*                Add prompt for E-mail to SCR01.                *
     H*                Enable F5=Refresh in display mode.             *
     H*                V3 standards SCR02.                            *
     H*                                                               *
     H*  12/31/01 C:136 PROJECT: WC1023                               *
     H*                 Add federal name entry. Also, load all of the *
     H*                 agent assignment records that have the same   *
     H*                 name.                                         *
     H*                                                               *
     H*  09/06/01 C:132 PROJECT: WC1047                               *
     H*                 ADD AGSS# Field in WMAGP file for Identifying *
     H*                 ID Number is a SocSec# = 'Y' or FEIN = 'N'    *
     H*                                                               *
     H*  03/07/01 C:09  SUPPORT: WA081/26603 - WA101/26400            *
     H*                 CORRECT ISSUE WITH LOSS OF FILE POINTER ON    *
     H*                 FILE WMAGP THAT WAS CAUSING DATA UPDATE ERRORS*
     H*                                                               *
     H*  04/27/00 C:53  SUPPORT: WC065/19768                          *
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.*
     H*                                                               *
     H*  07/19/99  C:33  PROJECT: WC899                               *
     H*                  ADD ACCOUNT CURRENT, # OF DAYS TO DUE DATE.  *
     H*                                                               *
     H*  06/16/98  C:33  SUPPORT: PER JJ                              *
     H*                  BE SURE THE CORRECT MOD IS LOADED PRIOR TO   *
     H*                  CHANGING THE ASSIGMENT LEVEL                 *
     H*                                                               *
     H*  02/04/98  C:33  PROJECT: WC683                               *
     H*                  ADD F14=DARTS                                *
     H*                                                               *
     H* 12/11/97 C:84  PROJECT# WC167                                 *
     H*          C:33  IF 1099 MAILED-TO CHANGED, SET ALL OTHER       *
     H*                MODIFIERS FOR THIS AGENT TO THE SAME VALUE.    *
     H*****************************************************************
     FW0040F02  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FWMAGP     UF A E             DISK
     F                                     RENAME(PAG01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMAAL     UF   E           K DISK
     FWMAGL     UF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWMAEL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FSTSTL3    IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     FWMAEP     IF   E             DISK
     F                                     RENAME(PAE01:PAE03)
     FWTCNP     IF   E             DISK
     F                                     RENAME(PCN01:PCN03)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
      *
      * Zip Codes by County (CO#/STATE/ZIPCODE)
     FWMCZL     IF   E           K DISK
     F*
      * Agent Group Codes (CO#/KEY)
     FWTAGL     IF   E           K DISK
     F                                     RENAME(PAG01:PAG03)
      * GENERIC TABLE FILE
     FWTGTL     IF   E           K DISK
      *
      /COPY SCOPYMT01D
     D*********  Beg Add ******** 07/14/14 ****************************
     DciAgtChg         pr                  extpgm('CIAGTCHG')
     D @@Rrn                          9  0
     D @@Type                        10
     D @@OFein                        9  0
     D @@OMod                         3  0
     D @@OName                      100
     D*********  End Add ******** 07/14/14 ****************************
     d*** Begin Add *** 10/03/22 *******************************************
     d* Retrieve Secured Data
     d StRtvDta        pr                  extPgm('STRTVDTA')
     d  inCompany#                    3  0
     d  inType                        1
     d  inTypeKey                    50
     d  inNumber1                     1
     d  outData1                    256
     d  inNumber2                     1
     d  outData2                    256

     d* Write Secured Data
     d StWrtDta        pr                  extPgm('STWRTDTA')
     d  inCompany#                    3  0
     d  inType                        1
     d  inTypeKey                    50
     d  inNumber1                     1
     d  inData1                     256
     d  inNumber2                     1
     d  inData2                     256

     d  inCompany#     s              3  0
     d  inType         s              1    Inz('A')
     d  inTypeKey      s             50
     d  inNumber1      s              1    Inz('1')
     d  inData1        s            256
     d  inNumber2      s              1    Inz('2')
     d  inData2        s            256
     d  outData1       s            256
     d  outData2       s            256
     d  hldPassword    s                   Like(SCpassword) Inz
     d  dirLength      s              2S 0 Inz
     d*** End   Add *** 10/03/22 *******************************************
     D*
     D STAF            S             10    DIM(6) CTDATA PERRCD(1)              POSSIBLE STAFF SEARC
     D LABL            S             18    DIM(4) CTDATA PERRCD(1)              Lavel name for field
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D                 DS
     D  DTS                    1     48  0
     D                                     DIM(6)                               SCREEN DATES
     D  SCDT01                 1      8  0
     D  SCDT02                 9     16  0
     D  SCDT03                17     24  0
     D  SCDT04                25     32  0
     D  SCDT05                33     40  0
     D  SCDT06                41     48  0
     D                 DS
     D  SFS                    1     18
     D                                     DIM(6)                               SCREEN STAFF CODES
     D  SCSF01                 1      3
     D  SCSF02                 4      6
     D  SCSF03                 7      9
     D  SCSF04                10     12
     D  SCSF05                13     15
     D  SCSF06                16     18
     D                 DS
     D  DTD                    1     42  0
     D                                     DIM(6)                               DATA DATES
     D  AGDD01                 1      7  0
     D  AGDD02                 8     14  0
     D  AGDD03                15     21  0
     D  AGDD04                22     28  0
     D  AGDD05                29     35  0
     D  AGDD06                36     42  0
     D                 DS
     D  SFD                    1     18
     D                                     DIM(6)                               DATA STAFF CODES
     D  AGSF01                 1      3
     D  AGSF02                 4      6
     D  AGSF03                 7      9
     D  AGSF04                10     12
     D  AGSF05                13     15
     D  AGSF06                16     18
     D                 DS
     D  QCKEY                  1     40
     D  QCFEIN                 1      9  0
     D  QCMOD                 10     12  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Agent Master Informa-
     D                                     tion')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D #OFSCR          C                   CONST(2)

     D***  Begin Add  ***  01/23/13  **************************************
     DsaveRRN          s                   like(dbrrn)
     D***  End   Add  ***  01/23/13  **************************************
     D*********  Beg Add ******** 07/14/14 ****************************
     D @@Rrn           s              9  0
     D @@Type          s             10
     D @@OFein         s              9  0
     D @@OMod          s              3  0
     D @@OName         s            100
     D*********  Beg Add ******** 07/14/14 ****************************
     C*
      ****** beg add ********* 07/02/15 ****************************************
     d/copy sPrGenErr
      ****** end add ********* 07/02/15 ****************************************
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
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
     C*
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C     AGKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFEIN
     C                   KFLD                    XXMOD
     C*
     C     AGKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFEIN
     C*
     C     AGKY3         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    AGCO#
     C                   KFLD                    AGFEIN
     C                   KFLD                    AGMOD
     C*
     C*  KEY LIST FOR AREA
     C*
     C     AEKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCAREA
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
     C*
     C*  SYSTEM TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
      *
      * Zip Codes by County File
      *
     C     CZKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##CZST            2
     C                   KFLD                    ##ZIP5            5 0
      *
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID            4
     C                   KFLD                    KKKEY            10
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    SC109M        S#109M
     C     *LIKE         DEFINE    SCCCKM        S#CCKM
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    CNKEY         XXKEY
     C     *LIKE         DEFINE    ATKEY         ##KEY
     C     *LIKE         DEFINE    *IN60         SFSRCH
     C     *LIKE         DEFINE    AGNAME        SVNAME
     C     *LIKE         DEFINE    AGMOD         XXMOD
     C     *LIKE         DEFINE    AG109M        SV109M
     C     *LIKE         DEFINE    DBRRN         XXSVRR
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* ASGNCH - Change Agent Assignment records
     C*================================================================
     C*
     C     ASGNCH        BEGSR
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C     AAKY          CHAIN     PAA01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  AGNAME        AANAME
     C                   MOVEL(P)  AGFDNM        AAFDNM
     C*
     C                   MOVEL(P)  Q1USER        AAUSER
     C                   MOVEL(P)  Q1PGM         AAPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AADT
     C*
     C                   MOVEL     DSTIME        AATIME
     C*
     C                   UPDATE    PAA01
     C*
     C     AAKY          READE     PAA01                                  99
     C*
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C                   ENDSR                                                  ASGNCH
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     AGSTAT        IFEQ      'I'
     C                   MOVE      'A'           AGSTAT
     C                   ELSE
     C                   MOVE      'I'           AGSTAT
     C                   ENDIF                                                  AGSTAT=I
     C*
     C                   MOVEL(P)  Q1USER        AGUSER
     C                   MOVEL(P)  Q1PGM         AGPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AGDT
     C*
     C                   MOVEL     DSTIME        AGTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHGSTS
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
     C*  THE PROVIDER NUMBER CANNOT BE EQUAL TO ZERO
     C*
     C  N90SCFEIN        IFLE      *ZERO
     C                   SETON                                        86  90
     C                   ENDIF                                                  SCFEIN<=*ZERO
     C*
     C*  THE MODIFIER CANNOT BE LESS THAN ZERO
     C*
     C  N90SCMOD         IFLT      *ZERO
     C                   SETON                                        87  90
     C                   ENDIF                                                  SCMOD<*ZERO
     C*
     C*  BE SURE THE FEIN/MOD HAS NOT BEEN PREVIOUSLY USED
     C*
     C  N90
     CAN 40AGKY          CHAIN(N)  PAG01                              60
     C  N90
     CAN 40
     CANN60              SETON                                        88  90
     C*
     C*  THE AGENT NAME CANNOT BE BLANK
     C*
     C  N90SCNAME        IFEQ      *BLANK
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCNAME=*BLANK
     C*
     C*  THE ADDRESS LINE CANNOT BE BLANK
     C*
     C  N90SCMAD1        IFEQ      *BLANK
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCMAD1=*BLANK
     C*
     C*  THE CITY CANNOT BE BLANK
     C*
     C  N90SCMCTY        IFEQ      *BLANK
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCMCTY=*BLANK
     C*
     C*  THE STATE MUST BE VALID
     C*
     C                   MOVEL(P)  SCMST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        74  90
     C*
     C*  THE ZIP CODE CANNOT BE ZERO
     C*
     C  N90SCMZP1        IFLE      *ZERO
     C                   SETON                                        75  90
     C                   ENDIF                                                  SCMZP1<=*ZERO
     C*
     C*  VALIDATE PHYSICAL ADDRESS IF ENTERED
     C*
     C  N90SCPAD1        IFNE      *BLANK
     C*
     C*  THE CITY CANNOT BE BLANK
     C*
     C  N90SCPCTY        IFEQ      *BLANK
     C                   SETON                                        76  90
     C                   ENDIF                                                  SCMCTY=*BLANK
     C*
     C*  THE STATE MUST BE VALID
     C*
     C                   MOVEL(P)  SCPST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        77  90
     C*
     C*  THE ZIP CODE CANNOT BE ZERO
     C*
     C  N90SCPZP1        IFLE      *ZERO
     C                   SETON                                        78  90
     C                   ENDIF                                                  SCMZP1<=*ZERO
     C*
     C                   ENDIF                                                  SCPAD1<>*ZERO
     C*
      *  If in ADD MODE, preload the County Code & Desc in screen
      *  If CHG MODE & Screen County Code BLANKS, See if can Preload it
     C                   MOVE      *BLANKS       ##CNTY            2
     C                   MOVE      *BLANKS       ##CZST
     C                   MOVE      *ZEROS        ##ZIP5
     C     @@MODE        IFEQ      'A'
     C     *IN90         ANDEQ     *OFF
     C     @@MODE        ORNE      'D'
     C     *IN90         ANDEQ     *OFF
     C     SCCNCD        ANDEQ     *BLANKS                                      COUNTY CODE
      * If Physical ST & Zip present, use it, else use Mailing ST, ZIP
     C     SCPST         IFGT      *BLANKS
     C     SCPZP1        ANDGT     *ZEROS
     C                   MOVE      SCPST         ##CZST
     C                   MOVE      SCPZP1        ##ZIP5
     C                   ELSE
     C                   MOVE      SCMST         ##CZST
     C                   MOVE      SCMZP1        ##ZIP5
     C                   ENDIF                                                  END SCPST=*
     C     CZKY          CHAIN     PCZ01                              60
     C                   SELECT
     C     SCMZP1        WHENGT    *ZEROS
     C     *IN60         ANDEQ     *OFF
     C     CZSTAT        ANDEQ     'I'
     C                   SETON                                        90  34
     C     SCPZP1        WHENGT    *ZEROS
     C     *IN60         ANDEQ     *OFF
     C     CZSTAT        ANDEQ     'I'
     C                   SETON                                        90  36
     C                   ENDSL
     C     *IN60         IFEQ      *OFF
     C     CZSTAT        ANDEQ     'A'
     C                   MOVE      CZCNTY        SCCNCD
     C                   MOVE      CZCNTY        ##CNTY
     C*
     C                   MOVEL(P)  ##CZST        XXKEY
     C     XXKEY         CAT       SCCNCD:0      XXKEY
     C     TBKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *OFF
     C     CNSTAT        ANDEQ     'A'
     C                   MOVEL(P)  CNDESC        SCCNDS
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  END-CZCNTY
     C                   ENDIF                                                  @@MODE=A/*IN09=*ON
      *
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90AGDT          IFGT      SAVEDT
     C     AGDT          OREQ      SAVEDT
     C     AGTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  AGDT>SAVEDT
     C*
     C*  DEFAULT CHECKS MAILED TO MOD TO CURRENT MODIFIER
     C*
     C  N90SCCCKM        IFEQ      *BLANK
     C     *IN40         ANDEQ     *ON
     C*
     C                   MOVEL     SCMOD         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         SCCCKM
     C*
     C     SCCCKM        IFEQ      *BLANK
     C                   MOVE      '  0'         SCCCKM
     C                   ENDIF                                                  SCCCKM=*BLANK
     C*
     C                   ENDIF                                                  SCCCKM=*BLANK
      *
     C     CLTID         IFEQ      '114'
      *
      *  E/O Coverage flag cna only be blank or Y/N
     C     SCCOVR        IFNE      *BLANKS
     C     SCCOVR        ANDNE     'Y'
     C     SCCOVR        ANDNE     'N'
     C                   SETON                                        31  90
     C                   ENDIF                                                              K
      *
      *  E/O Coverage flag cna only be blank or Y/N
     C     SCEOEX        IFNE      *ZEROS
     C                   Z-ADD     SCEOEX        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEOEX
     C     SRERR         IFEQ      *ON
     C                   SETON                                        9032
     C                   ENDIF
     C                   ENDIF                                                              K
      *
     C                   ENDIF                                                  CLTID       K
     C*
     ***** Begin Chg ***** 07/02/15 **********************************
      /Free
           erScEmal = *Off;
            If %Len(%trim(scemal)) > 0;
              erScEmal = Check_Email(scemal:msgscemal);
              If erScEmal = *On;
                 *IN90 = *On;
              EndIf;
            EndIf;
      /End-free
      ***** End Chg ***** 07/02/15 **********************************
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
        //*** Begin Add *** 10/03/22 ***************************************
        erSize     = *off;
        mgSize     = *blanks;
        erCount    = *off;
        mgCount    = *blanks;
        erSite     = *off;
        mgSite     = *blanks;
        erUserName = *off;
        mgUserName = *blanks;
        erPassword = *off;
        mgPassword = *blanks;
        *in90      = *Off;
        //*** End   Add *** 10/03/22 ***************************************
        //*** Begin Add *** 01/16/23 ***************************************
        erFtpDir   = *off;
        mgFtpDir   = *blanks;
        //*** End   Add *** 01/16/23 ***************************************
     C*
     C*  AGENCY SERVICES REPRESENTATIVE CODE MUST BE VALID
     C*
     C  N90SCSVCR        IFNE      *BLANK
     C*
     C     SCSVCR        CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        83  90
     C                   ELSE
     C                   MOVEL(P)  UPUSNM        SCSVCD
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCSVCR<>*BLANK
     C*
     C*  MARKETING REPRESENTATIVE CODE MUST BE VALID
     C*
     C  N90SCMKRP        IFNE      *BLANK
     C*
     C     SCMKRP        CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        85  90
     C                   ELSE
     C                   MOVEL(P)  UPUSNM        SCMKRD
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCMKRP<>*BLANK
     C*
     C*  THE 1099 FLAG MUST BE 'Y' OR *BLANK
     C*
     C  N90SC1099        IFNE      'Y'
     C     SC1099        ANDNE     *BLANK
     C                   SETON                                        71  90
     C                   ENDIF                                                  SC1099<>Y/*BLANK
     C*
     C*  1099 FLAG MUST BE 'Y' BEFORE EDIT FOR AGSS# = 'Y' or 'N' or ' '
     C*
     C*  Chg to always check SS# flag even if 1099 flag not = "Y"
     C                   MOVE      *OFF          *IN51
     C  N90SCSS#         IFNE      'Y'
     C     SCSS#         ANDNE     'N'
     C     SCSS#         ANDNE     *BLANK
     C                   SETON                                        51  90
     C                   ENDIF                                                  SCSS#<>Y/*BLANK
     C*
     C  N90SC1099        IFEQ      'Y'
     C     SC109M        ANDNE     *BLANKS
     C*
     C*  1099'S MAILED TO MOD MUST BE VALID OR EQUAL TO CURRENT MOD
     C*
     C                   MOVE      SC109M        XXMOD
     C*
     C  N90XXMOD         IFNE      SCMOD
     C*
     C     AGKY1         CHAIN(N)  PAG01                              60
     C  N90*IN60         IFEQ      *ON
     C     AGSTAT        OREQ      'I'
     C                   SETON                                        72  90
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXMOD<>SCMOD
      *
      *  If '1099 mailed to modifier' changed, prompt to determine if
      *  all records with same FEIN are to be updated with the new
      *  value.
      *
     C  N90SC109M        IFNE      S#109M
     C                   EXFMT     WDW01
     C                   MOVE      SC109M        S#109M
     C                   ENDIF                                                  SC109M
      *
     C                   ENDIF                                                  SC1099=Y
     C*
     C*  CHECKS MAILED TO MOD MUST BE VALID OR EQUAL TO CURRENT MOD
     C*
     C     SCCCKM        IFNE      *BLANKS
     C*
     C                   MOVE      SCCCKM        XXMOD
     C*
     C  N90XXMOD         IFNE      SCMOD
     C*
     C     AGKY1         CHAIN(N)  PAG01                              60
     C  N90*IN60         IFEQ      *ON
     C     AGSTAT        OREQ      'I'
     C                   SETON                                        74  90
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXMOD<>SCMOD
     C*
     C                   ENDIF                                                  SCCCKM<>*BLANKS
      *
      *  If 'Checks mailed to modifier' changed, prompt to determine if
      *  all records with same FEIN are to be updated with the new
      *  value.
      *
      ***** Begin delete 06/13/13 ******************************************
     C* N90SCCCKM        IFNE      S#CCKM
     C*                  EXFMT     WDW02
     C*                  MOVE      SCCCKM        S#CCKM
     C*                  ENDIF                                                  SCCCKM
      ***** End delete   06/13/13 ******************************************
      ***** Begin add    06/13/13 ******************************************
      /free
             if *in90 = *OFF and SCCCKM <> S#CCKM and AGSTAT <> 'I' ;
                EXFMT WDW02;
             endif;
      /end-free
      ***** End add      06/13/13 ******************************************
     C*
     C*  W9 SENT DATE MUST BE VALID, IF ENTERED
     C*
     C  N90SCW9SE        IFNE      *ZERO
     C*
     C                   Z-ADD     SCW9SE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCW9SE
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        75  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCW9SE<>*ZEROS
     C*
     C*  W9 RECIEVED DATE MUST BE VALID, IF ENTERED
     C*
     C  N90SCW9RC        IFNE      *ZERO
     C*
     C                   Z-ADD     SCW9RC        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCW9RC
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        76  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCW9RC<>*ZEROS
     C*
     C*  BACKUP WITHHOLDING MUST BE 'Y' OR *BLANK
     C*
     C  N90SCBKUP        IFNE      'Y'
     C     SCBKUP        ANDNE     *BLANK
     C                   SETON                                        77  90
     C                   ENDIF                                                  SCBKUP<>Y/*BLANK
     C*
     C*  SPECIAL AGENT PROCESSING MUST BE 'Y' OR *BLANK
     C*
     C  N90SCSPA         IFNE      'Y'
     C     SCSPA         ANDNE     *BLANK
     C                   SETON                                        78  90
     C                   ENDIF                                                  SCSPA<>Y/*BLANK
     C*
     C*  EDIT THE AGENT TYPE CODE
     C*
     C  N90SCTPC2        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCTPC2        XXKEY
     C*
     C     TBKY          CHAIN     PAT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     ATSTAT        OREQ      'I'
     C                   SETON                                        79  90
     C                   ELSE
     C                   MOVEL(P)  ATDESC        SCTPDS
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCTPC2<>*BLANKS
     C*  =========================
     C*  EDIT THE AGENT GROUP CODE
     C*  =========================
     C*
     C* If Agt Group Code was blanked out, remove the Description too
     C  N90SCGRCD        IFEQ      *BLANKS
     C     SCGRDS        ANDNE     *BLANKS
     C                   MOVE      *BLANKS       SCGRDS
     C                   ENDIF                                                  SCGRCD = *BLANKS
     C*
     C  N90SCGRCD        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCGRCD        XXKEY
     C*
     C     AGKY3         CHAIN     PAG03                              60
     C*
     C     *IN60         IFEQ      *ON
     C     AGSTAT        OREQ      'I'
     C*** Beg Chg *** 07/31/24 ****************************************
     C****************** SETON                                        83  90
     C                   SETON                                        55  90
     C*** End Chg *** 07/31/24 ****************************************
     C                   MOVEL(P)  *BLANKS       SCGRDS
     C                   ELSE
     C                   MOVEL(P)  AGDESC        SCGRDS
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCGRCD<>*BLANKS
     C*
     C*  COUNTY CODE MUST BE VALID AND ACTIVE
     C*
      *  If in ADD MODE, preload the County Code & Desc in screen
      *  If CHG MODE & Screen County Code BLANKS, See if can Preload it
     C                   MOVE      *BLANKS       ##CNTY            2
     C                   MOVE      *BLANKS       ##CZST
     C                   MOVE      *ZEROS        ##ZIP5
     C     @@MODE        IFEQ      'A'
     C     *IN90         ANDEQ     *OFF
     C     @@MODE        ORNE      'D'
     C     *IN90         ANDEQ     *OFF
     C     SCCNCD        IFEQ      *BLANKS                                      COUNTY CODE
      * If Physical ST & Zip present, use it, else use Mailing ST, ZIP
     C     SCPST         IFGT      *BLANKS
     C     SCPZP1        ANDGT     *ZEROS
     C                   MOVE      SCPST         ##CZST
     C                   MOVE      SCPZP1        ##ZIP5
     C                   ELSE
     C                   MOVE      SCMST         ##CZST
     C                   MOVE      SCMZP1        ##ZIP5
     C                   ENDIF                                                  END SCPST=*
     C     CZKY          CHAIN     PCZ01                              60
     C  N90*IN60         IFEQ      *OFF
     C     CZSTAT        ANDEQ     'A'
     C                   MOVE      CZCNTY        SCCNCD
     C                   MOVE      CZCNTY        ##CNTY
     C*
     C                   MOVEL(P)  ##CZST        XXKEY
     C     XXKEY         CAT       SCCNCD:0      XXKEY
     C     TBKY          CHAIN     PCN01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CNDESC        SCCNDS
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  END-CZCNTY
     C                   ELSE
     C*
     C     SCPST         IFNE      *BLANKS
     C                   MOVEL(P)  SCPST         XXKEY
     C                   ELSE
     C                   MOVEL(P)  SCMST         XXKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
     C*
     C     XXKEY         CAT       SCCNCD:0      XXKEY
     C*
     C     TBKY          CHAIN     PCN01                              60
     C*
     C  N90*IN60         IFEQ      *OFF
     C                   MOVEL(P)  CNDESC        SCCNDS
     C                   ELSE
     C                   SETON                                        31  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCCNCD<>*BLANKS
     C*
     C                   ENDIF                                                  @@MODE=A/*IN09=*ON
     C*
     C*  AREA MUST BE VALID
     C*
     C  N90SCAREA        IFNE      *ZERO
     C*
     C     AEKY          CHAIN     WMAEL                              60
     C*
     C     *IN60         IFEQ      *ON
     C     AESTAT        OREQ      'I'
     C                   SETON                                        81  90
     C                   ELSE
     C                   MOVEL(P)  AEDESC        SCARED
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCAREA<>*ZERO
     C*
     C*  EXCLUDE FROM EXPORT MUST BE VALID IF ENTERED.
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN69         ANDEQ     *ON
     C     SCEEXP        ANDNE     'Y'
     C     SCEEXP        ANDNE     *BLANKS
     C                   SETON                                        8790
     C                   ENDIF                                                  SCEEXP
     C*
     C*  VERIFY AGENT MARKETING REGION IF ENTERED
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMKRG        ANDNE     *BLANKS
     C                   MOVEL     'AMKR'        KKTBID
     C                   MOVEL     SCMKRG        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN84
     C                   MOVE      *ON           *IN90
     C                   ELSE
     C                   MOVEL(P)  GTDESC        SCMRDS
     C                   WRITE     SCR02
     C                   ENDIF                                                  *IN60
     C                   ENDIF                                                  SCMKRG
     C*
     C*  ACCOUNT CURRENT, # DAYS TO DUE DATE CANNOT BE LESS THAN ZERO
     C*
     C  N90SCACDU        IFLT      *ZERO
     C                   SETON                                        82  90
     C                   ENDIF                                                  SCACDU<*ZERO
     C     CLTID         IFEQ      '114'
     C                   MOVE      '1'           *IN89
     C*
     C*  BACKGROUND CHECK COMPLETION DATE MUST BE VALID, IF ENTERED
     C*
     C  N90SCBKCH        IFNE      *ZERO
     C*
     C                   Z-ADD     SCBKCH        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCBKCH
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        80  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCBKCH<>*ZEROS
     C                   ENDIF
        //*** Begin Add *** 10/03/22 ************************************
        // Size must be greater than or equal to zero
        If scSize < 0 And *in90 = *Off;
           erSize = *On;
           mgSize = 'Size must be greater than or equal to zero.';
           *in90  = *On;
        EndIf;

        // Count must be greater than or equal to zero.
        If scCount < 0 And *in90 = *Off;
           erCount = *On;
           mgCount = 'Count must be greater than or equal to zero.';
           *in90  = *On;
        EndIf;

        // User Name and Password must be entered if Site entered
        If scSite <> ' ';
           If scUserName = ' ' And *in90 = *Off;
              erUserName = *On;
              mgUserName = 'User name must be entered.';
              *in90  = *On;
           EndIf;
           If scPassword = ' ' And *in90 = *Off;
              erPassword = *On;
              mgPassword = 'Password must be entered.';
              *in90  = *On;
           EndIf;
        // User Name and Password can't be entered if Site not entered
        Else;
           If scUserName <> ' ' And *in90 = *Off;
              erUserName = *On;
              mgUserName = 'User name can''t be entered.';
              *in90  = *On;
           EndIf;
           If scPassword <> ' ' And *in90 = *Off;
              erPassword = *On;
              mgPassword = 'Password can''t be entered.';
              *in90  = *On;
           EndIf;
        EndIf;
        //*** End   Add *** 10/03/22 ************************************
        //*** Begin Add *** 01/16/23 ************************************
        // Directory must start with a / and end with a /
        dirLength = %Len(%Trim(scFtpDir));
        If scFtpDir <> ' ';
           If (%Subst(scFtpDir:1:1) <> '/' Or
              %Subst(scFtpDir:dirLength:1) <> '/') And (*in90 = *Off);
              erFtpDir = *On;
              mgFtpDir = 'Directory must begin with a / and end with a /.';
              *in90    = *On;
           EndIf;
        EndIf;
        //*** End   Add *** 01/16/23 ************************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90AGDT          IFGT      SAVEDT
     C     AGDT          OREQ      SAVEDT
     C     AGTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  AGDT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*  DATES MUST BE VALID
     C*
     C                   DO        6             X
     C*
     C     DTS(X)        IFNE      *ZERO
     C*
     C                   MOVEA     '000000'      *IN(31)
     C     X             ADD       30            X1                3 0
     C                   MOVE      *ON           *IN(X1)
     C*
     C                   Z-ADD     DTS(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DTS(X)
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        82  90
     C                   LEAVE
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  THERE MUST BE A CORRESPONDING STAFF CODE ENTERED
     C*
     C  N90SFS(X)        IFEQ      *BLANK
     C                   SETON                                        84  90
     C                   LEAVE
     C                   ENDIF                                                  SFS,X=*BLANK
     C*
     C                   ENDIF                                                  DTS,X<>*ZERO
     C*
     C*  STAFF CODES MUST BE VALID
     C*
     C  N90SFS(X)        IFNE      *BLANK
     C*
     C     SFS(X)        CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        83  90
     C                   LEAVE
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  THERE MUST BE A CORRESPONDING DATE CODE ENTERED
     C*
     C  N90DTS(X)        IFEQ      *ZERO
     C                   SETON                                        84  90
     C                   LEAVE
     C                   ENDIF                                                  STS,X=*ZERO
     C*
     C                   ENDIF                                                  SFS,X<>*BLANK
     C*
     C                   ENDDO                                                  X->6
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C                   Z-ADD     1             X1                3 0
     C     HFLD          LOOKUP    STAF(X1)                               60
     C                   MOVE      *IN60         SFSRCH
     C*
     C                   SELECT
     C*
     C*  STAFF CODES
     C*
     C     HFLD          WHENEQ    'SCMKRP'
     C     HFLD          OREQ      'SCSVCR'
     C     SFSRCH        OREQ      *ON
     C*
     C                   CALL      'S0005R'      UPRMPL                 99
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C     SFSRCH        IFEQ      *ON
     C*
     C                   MOVEL(P)  UPSTAF        SFS(X1)
     C                   ADD       31            X1
     C                   MOVE      *ON           *IN25
     C                   MOVE      *ON           *IN(X1)
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCMKRP'
     C                   MOVEL(P)  UPSTAF        SCMKRP
     C                   MOVEL(P)  UPUSNM        SCMKRD
     C     HFLD          WHENEQ    'SCSVCR'
     C                   MOVEL(P)  UPSTAF        SCSVCR
     C                   MOVEL(P)  UPUSNM        SCSVCD
     C                   MOVE      *ON           *IN26
     C                   ENDSL
     C*
     C                   ENDIF                                                  SFSRCH=*ON
     C*
     C                   ELSE
     C*
     C     SFSRCH        IFEQ      *ON
     C*
     C                   ADD       30            X1
     C                   MOVE      *ON           *IN24
     C                   MOVE      *ON           *IN(X1)
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCMKRP'
     C                   MOVE      *ON           *IN26
     C     HFLD          WHENEQ    'SCSVCR'
     C                   MOVE      *ON           *IN25
     C                   ENDSL
     C*
     C                   ENDIF                                                  SFSRCH=*ON
     C*
     C                   ENDIF                                                  ##RRN<>*ZERO
     C*_______________________________________________________________
     C*
     C*  AGENT TYPE SEARCH
     C*
     C*
     C     HFLD          WHENEQ    'SCTPC2'
     C*
     C                   CALL      'W2166R'
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      *BLANKS       ##KEY
     C*
     C     ##KEY         IFNE      *BLANKS
     C                   MOVEL(P)  ##KEY         SCTPC2
     C                   MOVEL(P)  SCTPC2        XXKEY
     C     TBKY          CHAIN     PAT01                              60
     C                   MOVEL(P)  ATDESC        SCTPDS
     C                   MOVE      *ON           *IN24
     C                   ELSE
     C                   MOVE      *ON           *IN21
     C                   ENDIF                                                  ##KEY<>*BLANK
     C*_______________________________________________________________
     C*
     C*_______________________________________________________________
     C*
     C*  AGENT GROUP SEARCH
     C*
     C     HFLD          WHENEQ    'SCGRCD'
     C*
     C                   CALL      'W2167R'
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      *BLANKS       ##KEY
     C*
     C     ##KEY         IFNE      *BLANKS
     C                   MOVEL(P)  ##KEY         SCGRCD
     C                   MOVEL(P)  SCGRCD        XXKEY
     C     AGKY3         CHAIN     PAG03                              60
     C     *IN60         IFEQ      *OFF
     C     AGSTAT        ANDEQ     'A'
     C                   MOVEL(P)  AGDESC        SCGRDS
     C                   ELSE
     C                   MOVE      *BLANKS       SCGRDS
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   MOVE      *ON           *IN22                          Cursor NEXT Fld
     C                   ELSE
     C                   MOVE      *ON           *IN24
     C                   ENDIF                                                  ##KEY<>*BLANK
     C*_______________________________________________________________
     C*
     C*
     C*  COUNTY CODE
     C*
     C     HFLD          WHENEQ    'SCCNCD'
     C*
     C     SCPST         IFNE      *BLANKS
     C                   MOVEL     SCPST         ZZST              2
     C                   ELSE
     C                   MOVEL     SCMST         ZZST
     C                   ENDIF                                                  SCPST<>*BLANKS
     C*
     C                   CALL      'W2215R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    XXKEY
     C                   PARM      ZZST          ##ST              2
     C                   PARM      2             ##VIEW            1 0
     C*
     C     ##RRN         CHAIN     WTCNP                              60
     C  N602             SUBST     CNKEY:3       SCCNCD
     C  N60              MOVEL(P)  CNDESC        SCCNDS
     C*
     C     ##RRN         COMP      *ZEROS                             232322
     C*_______________________________________________________________
     C*
     C*  AREA SEARCH
     C*
     C     HFLD          WHENEQ    'SCAREA'
     C*
     C                   CALL      'W4015R'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C*
     C     ##RRN         IFNE      *ZERO
     C*
     C     ##RRN         CHAIN     WMAEP                              60
     C  N60              Z-ADD     AEAREA        SCAREA
     C  N60              MOVEL(P)  AEDESC        SCARED
     C                   MOVE      *ON           *IN33
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN23
     C*
     C                   ENDIF                                                  ##RRN<>*ZERO
     C*
     C     HFLD          WHENEQ    'SCMKRG'
     C                   CALL      'W2493R'
     C                   PARM      *BLANKS       #1PRDS           40
     C                   PARM      *BLANK        #1CALL            1
     C                   PARM      'AMKR'        #1ID              4
     C                   PARM      *BLANKS       #1KEY            10
     C                   PARM      *BLANK        #1RTC             4
     C     #1KEY         IFNE      *BLANKS
     C                   MOVEL     #1KEY         SCMKRG
     C                   MOVEL     'AMKR'        KKTBID
     C                   MOVEL     SCMKRG        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCMRDS
     C                   MOVE      *ON           *IN33
     C                   ELSE
     C                   MOVE      *ON           *IN34
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14
     C*===============================================================
     C*
     C*  F14=DARTS
     C*
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   CLEAR                   QCKEY
     C*
     C                   Z-ADD     SCFEIN        QCFEIN
     C                   Z-ADD     SCMOD         QCMOD
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0040R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
      *
      * Sets the correct label for the fields on the screen
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
        //*** Begin Add *** 10/03/22 ************************************
        scSize     = 0;
        scCount    = 0;
        scSite     = ' ';
        scUserName = ' ';
        scPassword = ' ';
        //*** End   Add *** 10/03/22 ************************************
        //*** Begin Add *** 01/16/23 ***************************************
        scFtpDir   = ' ';
        //*** End   Add *** 01/16/23 ***************************************
      *
     C     CLTID         IFEQ      '114'
     C                   MOVEL     LABL(3)       SCLBL1
     C                   MOVEL     LABL(4)       SCLBL2
     C                   SETON                                        69
     C                   ELSE
     C                   MOVEL     LABL(1)       SCLBL1
     C                   MOVEL     LABL(2)       SCLBL2
     C                   SETOFF                                       69
     C                   ENDIF
      *
     C                   MOVE(P)   AGMKRG        SCMKRG
     C     SCMKRG        IFNE      *BLANKS
     C                   MOVEL     'AMKR'        KKTBID
     C                   MOVEL     SCMKRG        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCMRDS
     C                   ELSE
     C                   MOVE      *BLANKS       SCMRDS
     C                   ENDIF
     C*
     C   40              MOVE      'A'           AGSTAT
     C*
     C                   Z-ADD     AGFEIN        SCFEIN
     C                   Z-ADD     AGMOD         SCMOD
     C                   MOVEL(P)  AGNAME        SCNAME
     C                   MOVEL(P)  AGNAME        SVNAME
     C                   MOVEL(P)  AGMAD1        SCMAD1
     C                   MOVEL(P)  AGMCTY        SCMCTY
     C                   MOVE      AGMST         SCMST
     C                   MOVEL     AGMZIP        SCMZP1
     C                   MOVE      AGMZIP        SCMZP2
     C                   Z-ADD     AGARE1        SCARE1
     C                   MOVEL     AGPHN1        SCPH11
     C                   MOVE      AGPHN1        SCPH12
     C                   MOVEL(P)  AGPAD1        SCPAD1
     C                   MOVEL(P)  AGPCTY        SCPCTY
     C                   MOVE      AGPST         SCPST
     C                   MOVEL     AGPZIP        SCPZP1
     C                   MOVE      AGPZIP        SCPZP2
     C                   Z-ADD     AGARE2        SCARE2
     C                   MOVEL     AGPHN2        SCPH21
     C                   MOVE      AGPHN2        SCPH22
     C                   MOVEL     AGEEXP        SCEEXP
      *
     C                   MOVEL(P)  AGSHNM        SCSHNM
      *
      * Moves the values to the correct fields on the screen
      *
     C     CLTID         IFEQ      '114'
     C                   MOVEL(P)  AGSECY        SCCOVR
     C                   MOVEL     AGCONT        @@DTI             7 0
     C                   Z-ADD     @@DTI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCEOEX
     C                   ELSE
     C                   MOVEL(P)  AGSECY        SCSECY
     C                   MOVEL(P)  AGCONT        SCCONT
     C                   ENDIF
      *
     C                   MOVEL(P)  AGCOMM        SCCOMM
     C*
     C                   MOVEL     AGTPC2        SCTPC2
     C                   MOVEL     AGGRCD        SCGRCD
     C*
     C                   MOVE      *BLANKS       SCGRDS
     C     SCGRCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCGRCD        XXKEY
     C     AGKY3         CHAIN     PAG03                              60
     C     *IN60         IFEQ      *OFF
     C     AGSTAT        ANDEQ     'A'
     C                   MOVEL(P)  AGDESC        SCGRDS
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCTPC2<>*BLANKS
     C*
     C                   Z-ADD     AGACDU        SCACDU
     C                   MOVEL(P)  AGFDNM        SCFDNM
     C                   MOVEL(P)  AGFDNM        SVFDNM           40
     C*
     C                   MOVEL(P)  SCTPC2        XXKEY
     C                   CLEAR                   SCTPDS
     C     TBKY          CHAIN     PAT01                              60
     C  N60              MOVEL(P)  ATDESC        SCTPDS
     C*
     C                   MOVE      AGSTAT        SCSTAT
     C                   MOVE(P)   AGCNTY        SCCNCD
     C*
     C     SCPST         IFNE      *BLANKS
     C                   MOVEL(P)  SCPST         XXKEY
     C                   ELSE
     C                   MOVEL(P)  SCMST         XXKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
     C*
     C     XXKEY         CAT       SCCNCD:0      XXKEY
     C                   CLEAR                   SCCNDS
     C     TBKY          CHAIN     PCN01                              60
     C  N60              MOVEL(P)  CNDESC        SCCNDS
     C*
     C                   Z-ADD     AGAREA        SCAREA
     C                   CLEAR                   SCARED
     C     AEKY          CHAIN     WMAEL                              60
     C  N60              MOVEL(P)  AEDESC        SCARED
     C*
     C                   MOVEL     *BLANKS       EDTMSK
     C                   MOVEL     AG109M        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         SC109M
     C*
     C  N40AG109M        IFEQ      *ZERO
     C                   MOVE      '  0'         SC109M
     C                   ENDIF                                                  AG109M=*ZERO
     C*
     C                   Z-SUB     1             SV109M
     C                   MOVE      AG1099        SC1099
     C*
     C                   MOVE      AGSS#         SCSS#
     C*
     C                   MOVEL     *BLANKS       EDTMSK
     C                   MOVEL     AGCCKM        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         SCCCKM
     C*
     C     SCCCKM        IFEQ      *BLANK
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   MOVEL     SCMOD         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         SCCCKM
     C*
     C     SCCCKM        IFEQ      *BLANK
     C                   MOVE      '  0'         SCCCKM
     C                   ENDIF                                                  SCCCKM=*BLANK
     C*
     C                   ELSE
     C*
     C                   MOVE      '  0'         SCCCKM
     C*
     C                   ENDIF                                                  *IN40=*ON
     C*
     C                   ENDIF                                                  SCCCKM=*BLANK
     C*
     C                   MOVE      AGSPA         SCSPA
     C                   MOVEL(P)  AGHCON        SCHCON
     C                   MOVE      AGBKUP        SCBKUP
     C                   Z-ADD     AGFAXA        SCFAXA
     C                   MOVEL     AGFAX#        SCFAX1
     C                   MOVE      AGFAX#        SCFAX2
     C                   MOVEL(P)  AGIADR        SCIADR
     C                   MOVEL(P)  AGEMAL        SCEMAL
     C                   MOVEL(P)  AGSVCR        SCSVCR
     C*
     C                   CLEAR                   SCSVCD
     C     SCSVCR        CHAIN     PUP01                              60
     C  N60              MOVEL(P)  UPUSNM        SCSVCD
     C*
     C                   Z-ADD     AGW9SE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCW9SE
     C*
     C                   Z-ADD     AGW9RC        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCW9RC
     C*
     C                   DO        6             X                 3 0
     C*
     C                   Z-ADD     DTD(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DTS(X)
     C*
     C                   MOVEL(P)  SFD           SFS
     C*
     C                   ENDDO                                                  X->6
     C*
     C                   MOVEL(P)  AGCOM2        SCCOM2
     C                   MOVEL(P)  AGMKRP        SCMKRP
     C*
     C                   CLEAR                   SCMKRD
     C     SCMKRP        CHAIN     PUP01                              60
     C  N60              MOVEL(P)  UPUSNM        SCMKRD
     C*
     C     CLTID         IFEQ      '114'
     C                   MOVE      '1'           *IN89
     C                   Z-ADD     AGBKCH        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCBKCH
     C*
     C                   ENDIF
     C                   MOVE      SC109M        S#109M
     C                   MOVE      SCCCKM        S#CCKM
        //*** Begin Add *** 10/03/22 *******************************   *****
        scSize    = AGETsize;
        scCount   = AGETcnt;
        scSite    = AGEftp;

        // Let's get the user id and password
        inTypeKey  = %EditC(AGfein:'X') + %EditC(AGmod:'X');
        outData1   = ' ';
        outData2   = ' ';
        StRtvDta(AGco#:inType:inTypeKey:inNumber1:outData1:inNumber2   :
                 outData2);
        scUserName  = outData1;
        scPassword  = outData2;
        hldPassword = scPassword;
        If scPassword <> ' ';
           scPassword  = '**********';
        EndIf;
        //*** End   Add *** 10/03/22 *******************************   *****
        //*** Begin Add *** 01/16/23 ***************************************
        scFtpDir  = AGEftpD;
        //*** End   Add *** 01/16/23 ***************************************
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C     SC109M        IFEQ      *BLANK
     C*
     C     AGKY2         CHAIN(N)  PAG01                              60
     C     *IN60         DOWEQ     *OFF
     C     AGSTAT        ANDEQ     'I'
     C     *IN60         OREQ      *OFF
     C     AGMOD         ANDEQ     SCMOD
     C     AGKY2         READE(N)  PAG01                                  60
     C                   ENDDO
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     AG109M        XXMOD
     C                   ELSE
     C                   Z-ADD     SCMOD         XXMOD
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   MOVEL     *BLANKS       EDTMSK
     C                   MOVEL     XXMOD         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         SC109M
     C*
     C     XXMOD         IFEQ      *ZERO
     C                   MOVE      '  0'         SC109M
     C                   ENDIF                                                  XXMOD=*ZERO
     C*
     C                   ENDIF                                                  SC109M=*BLANK
     C*
     C     SCCCKM        IFEQ      *BLANK
     C*
     C     AGKY2         CHAIN(N)  PAG01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     AGSTAT        ANDEQ     'I'
     C     *IN60         OREQ      *OFF
     C     AGMOD         ANDEQ     SCMOD
     C     AGKY2         READE(N)  PAG01                                  60
     C                   ENDDO
     C*
     C     *IN60         IFEQ      *OFF
     C     *LIKE         DEFINE    AGCCKM        XXCCKM
     C                   Z-ADD     AGCCKM        XXCCKM
     C                   ELSE
     C                   Z-ADD     SCMOD         XXCCKM
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   MOVEL     *BLANKS       EDTMSK
     C                   MOVEL     XXCCKM        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         SCCCKM
     C*
     C     XXCCKM        IFEQ      *ZERO
     C                   MOVE      '  0'         SCCCKM
     C                   ENDIF                                                  XXCCKM=*ZERO
     C*
     C                   ENDIF                                                  SCCCKM=*BLANK
     C*
     C                   MOVEL(P)  SCSHNM        AGSHNM
     C*
     C                   Z-ADD     ##CO#         AGCO#
     C                   Z-ADD     SCFEIN        AGFEIN
     C                   Z-ADD     SCMOD         AGMOD
     C                   MOVEL(P)  SCNAME        AGNAME
     C                   MOVEL(P)  SCMAD1        AGMAD1
     C                   MOVEL(P)  SCMCTY        AGMCTY
     C                   MOVE      SCMST         AGMST
     C                   MOVEL     SCMZP1        AGMZIP
     C                   MOVE      SCMZP2        AGMZIP
     C                   Z-ADD     SCARE1        AGARE1
     C                   MOVEL     SCPH11        AGPHN1
     C                   MOVE      SCPH12        AGPHN1
     C                   MOVEL(P)  SCPAD1        AGPAD1
     C                   MOVEL(P)  SCPCTY        AGPCTY
     C                   MOVE      SCPST         AGPST
     C                   MOVEL     SCPZP1        AGPZIP
     C                   MOVE      SCPZP2        AGPZIP
     C                   Z-ADD     SCARE2        AGARE2
     C                   MOVEL     SCPH21        AGPHN2
     C                   MOVE      SCPH22        AGPHN2
     C                   MOVEL     SCEEXP        AGEEXP
      *
      *
     C     CLTID         IFEQ      '114'
     C                   MOVEL     SCCOVR        AGSECY
     C                   Z-ADD     SCEOEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         @@DTO             7
     C                   MOVEL     @@DTO         AGCONT
     C                   ELSE
     C                   MOVEL(P)  SCSECY        AGSECY
     C                   MOVEL(P)  SCCONT        AGCONT
     C                   ENDIF
      *
     C                   MOVEL(P)  SCCOMM        AGCOMM
     C*
     C                   MOVEL     SCTPC2        AGTPC2
     C                   MOVEL     SCGRCD        AGGRCD
     C*
     C                   MOVEL(P)  SCCNCD        AGCNTY
     C                   Z-ADD     SCAREA        AGAREA
     C                   Z-ADD     SCACDU        AGACDU
     C                   MOVEL(P)  SCFDNM        AGFDNM
     C                   MOVE      SC109M        AG109M
     C                   MOVE      SC1099        AG1099
     C*
     C*  If Screen SS# = 'Y', Load it ELSE Load Blank
     C     SCSS#         IFEQ      'Y'
     C                   MOVE      SCSS#         AGSS#
     C                   ELSE
     C                   MOVE      *BLANKS       AGSS#
     C                   ENDIF
     C                   MOVE      SCCCKM        AGCCKM
     C                   MOVE      SCSPA         AGSPA
     C                   MOVEL(P)  SCHCON        AGHCON
     C                   MOVE      SCBKUP        AGBKUP
     C                   Z-ADD     SCFAXA        AGFAXA
     C                   MOVEL     SCFAX1        AGFAX#
     C                   MOVE      SCFAX2        AGFAX#
     C                   MOVEL(P)  SCIADR        AGIADR
     C                   MOVEL(P)  SCEMAL        AGEMAL
     C                   MOVEL(P)  SCSVCR        AGSVCR
     C*
     C                   Z-ADD     SCW9SE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AGW9SE
     C*
     C                   Z-ADD     SCW9RC        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AGW9RC
     C*
     C     CLTID         IFEQ      '114'
     C                   Z-ADD     SCBKCH        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AGBKCH
     C                   ENDIF
     C*
     C                   DO        6             X
     C*
     C                   Z-ADD     DTS(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTD(X)
     C*
     C                   MOVEL(P)  SFS(X)        SFD(X)
     C*
     C                   ENDDO                                                  X->6
     C*
     C                   MOVEL(P)  SCCOM2        AGCOM2
     C                   MOVEL(P)  SCMKRP        AGMKRP
     C                   MOVE      SCSTAT        AGSTAT
        //*** Begin Add *** 10/03/22 ************************************
        AGETsize  = scSize;
        AGETcnt   = scCount;
        AGEftp    = scSite;

        // Let's get the user id and password
        inTypeKey  = %EditC(AGfein:'X') + %EditC(AGmod:'X');
        inData1    = scUserName;

        If scPassword = '**********';
           If hldPassword <> ' ';
              inData2 = hldPassword;
           EndIf;
        Else;
           inData2 = scPassword;
        EndIf;

        StWrtDta(AGco#:inType:inTypeKey:inNumber1:inData1:inNumber2:
                 inData2);
        //*** End   Add *** 10/03/22 ************************************
        //*** Begin Add *** 01/16/23 ***************************************
        AGEftpD   = scFtpDir;
        //*** End   Add *** 01/16/23 ***************************************
     C*
     C                   MOVEL(P)  Q1USER        AGUSER
     C                   MOVEL(P)  Q1PGM         AGPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AGDT
     C*
     C                   Z-ADD     DSTIME        AGTIME
     C*
     C                   MOVE(P)   SCMKRG        AGMKRG
     C*
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
     C*** Beg Add 07/31/24 ************************************************
     C                   SETOFF                                       55
     C*** End Add 07/31/24 ************************************************
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*
     C     @@MODE        IFEQ      'A'
     C     *INKC         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C                   CALL      'W0041R02'                           99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@MODE        ##MODE            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C                   PARM      Q1PGM         ##PGM            10
     C*
     C     *INKC         IFEQ      *OFF
     C*
     C                   CALL      'W0042R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM      DBRRN         ##RRN             9 0
     C*
     C                   ENDIF                                                  *INKC=*OFF
     C*
     C                   ENDIF                                                  @@MODE=A
      *
      *  Update all "1099 mailed to modifier" for same FEIN
      *                  AND/OR
      *  Update all "Check mailed to modifier" for same FEIN
      *
     C     S1109M        IFEQ      'Y'
     C     S1CCKM        OREQ      'Y'
     C     AGKY2         CHAIN     PAG01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     S1109M        IFEQ      'Y'
     C                   MOVE      SC109M        AG109M
     C                   ENDIF
      *
     C     S1CCKM        IFEQ      'Y'
     C                   MOVE      SCCCKM        AGCCKM
     C                   ENDIF
      *
     C     S1109M        IFEQ      'Y'
     C     S1CCKM        OREQ      'Y'
     C                   MOVEL(P)  Q1USER        AGUSER
     C                   MOVEL(P)  Q1PGM         AGPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AGDT
     C                   Z-ADD     DSTIME        AGTIME
     C                   UPDATE    PAG01
     C                   ENDIF
      *
     C     AGKY2         READE     PAG01                                  60
     C                   ENDDO                                                  AGKY2
      *
     C                   ENDIF                                                  109M CCKM
      *
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C                   CLEAR                   S1109M
     C                   CLEAR                   S1CCKM
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C     @@MODE        IFEQ      'A'
     C                   CLEAR                   @@RRN
     C                   ENDIF                                                  @@MODE=A
     C*
     C                   CLEAR                   WCFL15
     C     'WC'          CHAIN     SWC01                              60
     C     WCFL15        COMP      'Y'                                    96
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
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
     C                   ENDSR                                                  PREOTH
     C*
     C*================================================================
     C* SETMOD - Set all modifiers for this agent to same AG109M value
     C*================================================================
     C*
     C     SETMOD        BEGSR
     C*
     C                   Z-ADD     DBRRN         XXSVRR
     C*
     C                   MOVE      SC109M        XXMOD
     C*
     C     AGKY2         SETLL     PAG01
     C     AGKY2         READE(N)  PAG01                                  60
     C*
B01  C     *IN60         DOWEQ     *OFF
     C*
B02  C     XXMOD         IFNE      AG109M
     C     SCMOD         ANDNE     AGMOD
     C*
     C     DBRRN2        CHAIN     WMAGP                              60
     C*
     C                   Z-ADD     XXMOD         AG109M
     C*
     C                   MOVEL(P)  Q1USER        AGUSER
     C                   MOVEL(P)  Q1PGM         AGPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AGDT
     C*
     C                   Z-ADD     DSTIME        AGTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  XXMOD<>AG109M
     C*
     C     AGKY2         READE(N)  PAG01                                  60
     C*
E01  C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   Z-ADD     XXSVRR        DBRRN
     C*
     C                   ENDSR                                                  SETMOD
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
     C*
     C*  IF THE NAME CHANGES, UPDATE THE NAME IN ALL ASSIGNMENT RECORDS
     C*
     C     SVNAME        IFNE      AGNAME
     C     SVFDNM        ORNE      AGFDNM
     C                   EXSR      ASGNCH
     C                   CALL      'WA085R'
     C                   PARM      'A'           XXTYPE            1
     C                   PARM      DBRRN         XXRRN             9 0
     C                   PARM      *BLANK        XXRTN             1
E01  C                   ENDIF                                                  SVNAME<>AGNAME
     C*
     C*********  Beg Add ******** 07/14/14 ****************************
      /Free
        If SVname<>AGname;
           @@Rrn  = DBrrn;
           @@Type = 'DBA';
           @@OFein= AGfein;
           @@OMod = AGmod;
           @@OName= SVname;
           CIAGTCHG (@@Rrn:@@Type:@@OFein:@@OMod:@@OName);
        Endif;
      /End-Free
     C*********  End Add ******** 07/14/14 ****************************
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
     C                   MOVEL(P)  Q1USER        AGIUSR
     C                   MOVEL(P)  Q1PGM         AGIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AGIDT
     C*
     C                   Z-ADD     DSTIME        AGITM
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C                   WRITE     DBRCDF
     C                   ENDIF                                                   CHECK FOR EXIT
     C*
      /Free
       //  Begin Add  ***  01/23/13  **************************************
       saveRRN = dbRRN;
       //  End   Add  ***  01/23/13  **************************************
      /End-free
     C                   FEOD      WMAGP
      /Free
       //  Begin Add  ***  01/23/13  **************************************
       dbRRN = saveRRN;
       //  End   Add  ***  01/23/13  **************************************
      /End-free
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C*
     C                   ENDSR                                                  WRITER
**  STAF
SCSF01
SCSF02
SCSF03
SCSF04
SCSF05
SCSF06
**  LABL
Secretary name   :
Marketing contact:
E/O Coverage  Y/N:
E/O Coverage Exp :
