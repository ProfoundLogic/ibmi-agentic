     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0076R - ENTITY/LOCATION SEARCH
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL        *
     H*                ENTITY/LOCATIONS BY NAME OR NUMBER. EACH       *
     H*                ENTITY/LOCATION CAN BE ACCESSED FOR UPDATE,    *
     H*                DISPLAY OR SELECTION DEPENDING UPON THE CALLING*
     H*                PROGRAM.                                       *
     H*                THIS WORK WITH WILL BE CALLED AS A SEARCH      *
     H*                AUTOMATICALLY IF A REQUEST IS MADE FROM THE    *
     H*                ENDORSEMENT MAINTENANCE PROGRAM (W0050R1) TO   *
     H*                DELETE OR ADD AN ENTITY OR LOCATION.           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0050R   - WORK WITH EMPLOYERS                             *
     H*    W0050R1  - ENDORSEMENT MAINTENANCE                         *
     H*  CALLS TO:                                                    *
     H*    W0076R02 - ENTITY/LOCATION INFORMATION (OPT 2/5)           *
     H*    W1057R   - WORK WITH OWNERS/OFFICERS (OPT 7)               *
     H*    W7550R   - PRINT LABELS (OPT 19)                           *
     H*    W7550R   - PRINT ALL LABELS (F 19)                         *
     H*                                                               *
     H*  CREATED: 04/15/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 12/28/11 C:189 SUPPORT:78684
     H*                First location is never printing on the endor-
     H*                sement form on cancellation.
     H*
     H* 07/19/10 C:190 SUPPORT:71340
     H*                First location printing (in error) along with
     H*                the actual add/chg/del locations on endorsement form.
     H*
     H* 05/27/10 C:190 SUPPORT:71340
     H*            If called from Employer Mid Term Endorsements,
     H*            Do not Allow Owner/Officer processing (OPT 7).
     H*
     H* 05/25/10 C:190 Seq# 72410
     H*                Stop producing output if no changes has been
     H*                entered while Midterm Entity Location Change
     H*                endorsement (Do not write record to WMENP).
     H*
     H* 09/17/09 C:33  Project: WC1118     SY184  Priss #5
     H*                Ensure the county code prints on the endorsement
     H*
     H* 08/10/09 C:33  Project: SY184  Priss #5
     H*                DARTS record can not be found.
     H*
     H* 05/06/09 C:61  PROJECT 114WTU
     H*                - Print second line of Entity/Location
     H*                  'Name/Federal name' on mid-term ENDORSE cover
     H*                  page.
     H*
     H* 05/05/09 C:191 PROJECT 114WTU                                 *
     H*                - Include second ENTITY/LOCATION in sub setting*
     H*                                                               *
     H* 08/27/07 C:136 SUPPORT: WA045 59822                           *
     H*                SET ON *INLR IN ALL CASES SO THAT THE OPTION   *
     H*                ARRAY WOULD RE-INITIALIZE AND DISPLAY CORRECTLY
     H*
     H* 04/23/07 C:33  Seq#: WA099/56028
     H*                Be sure mailing address prints on Endorsment
     H*                 form if physical address is not entered.
      *          C:61  Do not print "before" zip code if address not
      *                entered.
     H*
     H* 01/09/07 C:61  SUPPORT 132WFM WA132 56551
     H*                SROPTFNC doing weird things when called as
     H*                mid-term endorsement.  Removed some lines from
     H*                compile time array to get program working.
     H*
     H* 09/06/06 C:61  PROJECT 132WFM
     H*                Add option 92=MPN Provider
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*                                                               *
     H*  02/14/02 C:09  SUPPORT: WA093/Seq #31264                     *
     H*                 Values not cleared resulting in a DARTS       *
     H*                 comment ("Deletion of Location") that does    *
     H*                 not correspond with the DARTS code assigned   *
     H*                 ASAL ("Addition of Location").                *
     H*                                                               *
     H*  08/21/00 C:09  SUPPORT: WA048/Seq #23490                     *
     H*                 Process was not loading the midterm end field *
     H*                 that holds the entity/location ID (ALELID).   *
     H*                                                               *
     H*  04/07/00 C:09  SUPPORT: WA048/22477                          *
     H*                 Process was not properly recognizing a        *
     H*                 Fed ID # change made in midterm endorsements  *
     H*                 on an entity.  Endorsement output produced was*
     H*                 not including NCCI end (WC890600A) nor the    *
     H*                 policy page workplace extension page.         *
     H*                                                               *
     H*  06/11/99 C:118 SUPPORT: WA091/18288                          *
     H*                 IF EMPLOYER CANCELLATION DATE IS LESS THAN OR *
     H*                 EQUAL TO CURRENT DATE, DON'T DISPLAY F19=     *
     H*                 PRINT ALL LABELS OR OPTION 19=PRINT A LABEL.  *
     H*                                                               *
     H*  05/20/99 C:09 PROJECT: CM044 (WA048/SEQ# 18166)              *
     H*                REFERENCE XXQTE NOT @@QTE.                     *
     H*                                                               *
     H*  04/30/99 C:09 PROJECT: CM027                                 *
     H*                MIDTERM ENTITY/LOCATION ENDORSEMENT REWRITE.   *
     H*                                                               *
     H*  03/30/99 C:33 PROJECT: WC577                                 *
     H*          C:118 RESET VALID OPTION FLAG AFTER READS TO BE SURE *
     H*                CORRECT RRN'S ARE LOADED.                      *
     H*                                                               *
     H*  02/05/99 C:87 PROJECT: WC878C                                *
     H*                Add a new option "8=Policy years" to call      *
     H*                W0075R.  This option will only be available    *
     H*                if WCFL14 = 'Y' in file SWCPF.  The second     *
     H*                option line is enabled and the subfile page    *
     H*                size has been reduced to accomodate.           *
     H*                                                               *
     H*  09/11/98 C:35 PROJECT: 60AND                                 *
     H*                DO NOT ALLOW OPTION 19 (PRINT LABEL) FOR       *
     H*                CANCELLED EMPLOYERS.  THIS WAS CAUSING AN      *
     H*                ENDLESS LOOP.                                  *
     H*                                                               *
     H*  09/01/98 C:33 SEQ#: DR095/12876                              *
     H*                PASS THE HOLD VALUE OF @@QTE IN ACDPL2         *
     H*                                                               *
     H*  08/07/98 C:78 SEQ#: DR095/12876                              *
     H*                ADD PARAMETER ##QTE WHEN CALLING W0076R02 AND  *
     H*                W1057R FOR QUOTE MODE. MOVE VALUE 'Q' OR       *
     H*                *BLANKS TO QCQTE FOR QUOTE MODE.               *
     H*                                                               *
     H*  07/17/98 C:33 PROJECT: WC577                                 *
     H*                ALLOW PROGRAM TO BE USED TO SEARCH ALL         *
     H*                ENTITIES/LOCATIONS.                            *
     H*                                                               *
     H* 04/10/98  C:33  PROJECT: WC577                                *
     H*                 USE ENTITY/LOCATION ADDRESS (NOT EMP MSTR).   *
     H*                                                               *
     H* 04/03/98  C:78  WC825                                         *
     H*                 ALLOW ACCESS TO DARTS. F14 ADDED.             *
     H*                                                               *
     H* 03/05/98  C:57  CM027                                         *
     H*                 REMOVE LOCKOUT OF OWNER/OFFICER DURING MID-   *
     H*                 TERM ENDORSEMENT PROCESSING.                  *
     H*                                                               *
     H* 02/12/98  C:35  PROJECT: SUPPORT                              *
     H*                 CHANGE CODE SO THAT THE CANCELLATION DATE     *
     H*                 DISPLAYED ON THE SCREEN IS THE CX DATE        *
     H*                 ASSOCIATED WITH THE MOST RECENT EFFECTIVE     *
     H*                 DATE.  ALSO, THIS FIELD IS NOW BEING CLEARED  *
     H*                 SO THAT IT WON'T CARRY DATA FROM A PREVIOUSLY *
     H*                 DISPLAYED EMPLOYER IF THE CURRENT EMPLOYER    *
     H*                 IS NOT CANCELLED.  ALSO, CONTROL DISPLAY OF   *
     H*                 THIS FIELD WITH AN INDICATOR SO THAT IF THE   *
     H*                 EMPLOYER IS NOT CANCELLED, THE FIELD AND      *
     H*                 HEADING WILL NOT SHOW ON THE SCREEN.          *
     H*                                                               *
     H*  02/05/98  C:57  CM027                                        *
     H*            IF CALLED FROM EMPLOYER MID TERM ENDORSEMENTS,     *
     H*            DO NOT ALLOW OWNER/OFFICER PROCESSING (OPT 7).     *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  01 - Used on the PAGEDOWN keyword                            *
     H*  02 - Used on the PAGEUP keyword                              *
     H*  03 - Used to condition SFLCLR                                *
     H* N03 - Used to condition SFLDSPCTL                             *
     H*  04 - Used to condition SFLDSP                                *
     H*  05 - Used to condition DSPATR(RI) on the selection field     *
     H* N06 - Used to condition DSPATR(PC) on the position to field   *
     H*  07 - Used to condition DSPATR(PC) on the selection field     *
     H*  08 - Used to condition SFLNXTCHG                             *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H* N10 - Used to condition SFLEND (on message subfile)           *
     H*  11 - Used to indicate the list is subsetted                  *
     H*  12 - Used to condition SFLDROP/SFLFOLD                       *
     H*  13 - Used to condition SFLCLR (on confirm delete)            *
     H* N13 - Used to condition SFLDSPCTL (on confirm delete)         *
     H*  14 - Used to condition SFLDSP (on confirm delete)            *
     H*  15 - Used to condition SFLEND (on confirm delete)            *
     H*  21 - Entity/Loc Name Search                                  *
     H*  22 - E/L Number Search                                       *
     H*  30 - Called as a search                                      *
     H*  31 - Client rpts IAIABC/POC (WCFL14='Y' in SWCPF)            *
     H*  40 - More Option Controller                                  *
     H*  41 - More Option Controller                                  *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capable screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*****************************************************************
     FW0076F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWMALP     IF   E             DISK
     F                                     RENAME(PAL01:PFRCDF)
     FWMALL2    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAL01:DBRCDF)
     F***  BEGIN ADD  ***  04/30/99  *********************************
     F************WMALL   IF  E           K        DISK
     FWMALL     UF   E           K DISK
     F***  END CHG ******  04/30/99  *********************************
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PAL01:DBRCDF2)
     F***  BEGIN ADD  ***  07/17/98  *********************************
     FWMALL3    IF   E           K DISK
     F                                     INFDS(DBFDSB)
     F                                     RENAME(PAL01:DBRCDFB)
     FWMALL5    IF   E           K DISK
     F                                     INFDS(DBFDSC)
     F                                     RENAME(PAL01:DBRCDFC)
     F***  END   ADD  ***  07/17/98  *********************************
     FWMFNL     IF   E           K DISK
     FWMEMP     IF   E             DISK
     FWMCMLC    IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F***  BEGIN ADD  ***  04/30/99  *********************************
     FWTMYP     IF   E             DISK
     FWMDXL     IF   E           K DISK
     FWMENL3    IF A E           K DISK
     FWDF2L     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     F***  END ADD ******  04/30/99  *********************************
     F***  Begin Add  ***  09/17/09  **************************************
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PSY01)
     F***  End   Add  ***  09/17/09  **************************************
      ***** BEG ADD ******* 09/06/06 **********************************
      *
      * MPN Provider
     FWMMPL     IF   E           K DISK
     F                                     INFDS(MPFDS)
      ***** END ADD ******* 09/06/06 **********************************
      *
      *
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D***  BEGIN ADD  ***  07/17/98  *********************************
     D HDA             S             78    DIM(2) CTDATA PERRCD(1)              Alt Column headings
     D***  END   ADD  ***  07/17/98  *********************************
      ***** BEGIN CHG ****** 04/03/98 *********************************
     D****************    OF      1  15  1   OFD    35   Options, Function
     D***  BEGIN CHG  ***  02/05/99  **********************************
     D******************* OF      1  16  1   OFD    35   Options, Function
      ***** BEG CHG ******* 09/06/06 **********************************
     D*************       OF      1  17  1   OFD    35   Options, Function
     D OF              S              1    DIM(27) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(27) ALT(OF)
      ***** END CHG ******* 09/06/06 **********************************
     D***  END   CHG  ***  02/05/99  **********************************
      ***** END   CHG ****** 04/03/98 *********************************
      **** BEGIN ADD **** 02/12/98 *********************************************
      **** END   ADD **** 02/12/98 *********************************************
     D DTA2            S              1    DIM(68)
     D CSZ             S              1    DIM(20)
     D***  BEGIN ADD  ***  04/30/99  *********************************
      ***** BEG DEL ******* 05/06/09 **********************************
     D*****               NM        500 40
     D*****               FN        500 40
      ***** END DEL ******* 05/06/09 **********************************
      ***** BEG ADD ******* 05/06/09 **********************************
     D NM              S             50    DIM(500)
     D N2              S             50    DIM(500)
     D NM2             S             50    DIM(500)
     D N22             S             50    DIM(500)
     D FN              S             50    DIM(500)
     D F2              S             50    DIM(500)
     D FN2             S             50    DIM(500)
     D F22             S             50    DIM(500)
      ***** END ADD ******* 05/06/09 **********************************
     D P1              S             40    DIM(500)
     D P2              S             40    DIM(500)
     D P3              S             40    DIM(500)
     D PC              S             25    DIM(500)
     D PS              S              2    DIM(500)
     D PZ              S              9  0 DIM(500)
     D***  Begin Add  ***  09/17/09  **************************************
     D CN              S             25    DIM(500)
     D***  End   Add  ***  09/17/09  **************************************
     D M1              S             40    DIM(500)
     D M2              S             40    DIM(500)
     D M3              S             40    DIM(500)
     D MC              S             25    DIM(500)
     D MS              S              2    DIM(500)
     D MZ              S              9  0 DIM(500)
     D FE              S              9  0 DIM(500)
     D TB              S             20    DIM(500)
     D ID              S             10    DIM(500)
     D DD              S              7  0 DIM(500)
     D DR              S             30    DIM(500)
     D NA              S              1    DIM(500)
     D MA              S              1    DIM(500)
     D PA              S              1    DIM(500)
     D***  Begin Add  ***  09/17/09  **************************************
     D CA              S              1    DIM(500)
     D***  End   Add  ***  09/17/09  **************************************
     D*** BEGIN ADD *** 04/07/00 ***********************************************
     D FI              S              1    DIM(500)
     D*** END ADD ***** 04/07/00 ***********************************************
      ***** BEG DEL ******* 05/06/09 **********************************
     D*********           NM2       500 40
     D*********           FN2       500 40
      ***** END DEL ******* 05/06/09 **********************************
     D P12             S             40    DIM(500)
     D P22             S             40    DIM(500)
     D P32             S             40    DIM(500)
     D PC2             S             25    DIM(500)
     D PS2             S              2    DIM(500)
     D PZ2             S              9  0 DIM(500)
     D***  Begin Add  ***  09/17/09  **************************************
     D CN2             S             25    DIM(500)
     D***  End   Add  ***  09/17/09  **************************************
     D M12             S             40    DIM(500)
     D M22             S             40    DIM(500)
     D M32             S             40    DIM(500)
     D MC2             S             25    DIM(500)
     D MS2             S              2    DIM(500)
     D MZ2             S              9  0 DIM(500)
     D FE2             S              9  0 DIM(500)
     D TB2             S             20    DIM(500)
     D ID2             S             10    DIM(500)
     D DD2             S              7  0 DIM(500)
     D DR2             S             30    DIM(500)
     D ST2             S              1    DIM(500)
     D TP2             S              1    DIM(500)
     D***  END ADD ******  04/30/99  *********************************
     D*
     D***PY SCOPYWW01E
     D***  BEGIN ADD ****  04/30/99  *********************************
     D AGTAPI          C                   CONST('WA002R')
     D***  END ADD ******  04/30/99  *********************************
     D*
     D***PY SCOPYWW01I
     D*
     D***  BEGIN ADD  ***  07/17/98  *********************************
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D DBFDSC          DS
     D  DBRRNC               397    400B 0
     D***  END   ADD  ***  07/17/98  *********************************
      ***** BEG ADD ******* 09/06/06 **********************************
     D MPFDS           DS
     D  MPRRN                397    400B 0
      ***** END ADD ******* 09/06/06 **********************************
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  DATA1                  1     27
     D  X1NAME                 1     15
     D  X1ELID                18     27
     D                 DS
     D  DATA2                  1     27
     D  X2ELID                 1     10
     D  X2NAME                13     27
     D***  BEGIN ADD  ***  07/17/98  *********************************
     D                 DS
     D  SSDIV                  1      5
     D  S1DIV                  1      1
     D  S2DIV                  2      2
     D  S3DIV                  3      3
     D  S4DIV                  4      4
     D  S5DIV                  5      5
     D***  END   ADD  ***  07/17/98  *********************************
      **** BEGIN ADD **** 02/12/98 *********************************************
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
      **** END   ADD **** 02/12/98 *********************************************
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)                              CLAIM PMTS
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
      ******* BEGIN ADD ***** 04/03/98 ********************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
      ******* END   ADD ***** 04/03/98 ********************************
     D*** BEG DEL ***** 03/05/98 ***********************************************
     D*** BEG ADD ***** 02/05/98 ***********************************************
     D***SVLDA       DS                           1024
     D***                                     1   50LDEND#
     D***                                     1   5 LDENDA
     D***                                     6  140LDMYRR
     D***                                    15  220LDEEDT
     D***                                    23  250LDFYR
     D***                                    26  26 LDPYRL
     D*** END ADD ***** 02/05/98 ***********************************************
     D*** END DEL ***** 03/05/98 ***********************************************
     D***  BEGIN ADD  ***  04/30/99  *********************************
     D                 DS
     D  XXZIPX                 1      9
     D  XXZIP5                 1      5
     D  XXZIP4                 6      9
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D                 DS
     D  XXLINE                 1     35
     D  XXDSPL                28     28
     D XXDES1          C                   CONST('Federal ID #:')
     D XXDES2          C                   CONST('Ent/Loc ID #:')
     D***  END ADD ******  04/30/99  *********************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Entity/Locations')
     D ACDPGM          C                   CONST('W0076R02')
     D***  BEGIN ADD  ***  07/17/98  *********************************
     D CHGNUM          C                   CONST('SA020R')
     D***  END   ADD  ***  07/17/98  *********************************
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D DLTED           C                   CONST('* * DELETED * *')
     D SDLT            C                   CONST('Delete date entered -
     D                                     press F5.')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D***  BEGIN CHG  ***  02/05/99  **********************************
     D************* 12 ****************** C ******* SF1PAG
     D SF1PAG          C                   CONST(9)
     D***  END   CHG  ***  02/05/99  **********************************
     D COMMA           C                   CONST(',')

     I*** BEG DEL ***** 03/05/98 ***********************************************
     I*** BEG ADD ***** 02/05/98 ***********************************************
     I***           '                    -C         CLRFKY
     I***           '       NNN'
     I*** END ADD ***** 02/05/98 ***********************************************
     I*** END DEL ***** 03/05/98 ***********************************************

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
      ***** BEGIN ADD ******* 08/07/98 ********************************
     C                   PARM                    @@QTE             1
      ***** END   ADD ******* 08/07/98 ********************************
     C*
     C*  NOTE: -If @@RRN2 is not passed the program will display all
     C*         Entities/Locations.
     C*        -For displaying/maintaining/searching all Entities/Loca-
     C*         tions for a particular employer the rrn of the employer
     C*         will need to be passed into @@RRN2.
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C***  BEGIN CHG  ***  07/17/98  *********************************
     C**********           PARM @@RRN2    ##RRN2  90
     C                   PARM                    ##RRN2            9 0
     C***  END   CHG  ***  07/17/98  *********************************
      ****** BEGIN ADD ****** 08/07/98 *******************************
     C                   PARM      XXQTE         ##QTE             1
      ****** END   ADD ****** 08/07/98 *******************************
     C*
     C     ACDPL2        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      SCELTP        ##ELFL            1
     C***  BEGIN CHG  ***  09/01/98  *********************************
     C*******BEGIN ADD ******** 08/07/98 ******************************
     C**********           PARM @@QTE     ##QTE   1
     C*******END   ADD ******** 08/07/98 ******************************
     C                   PARM      XXQTE         ##QTE             1
     C***  END   CHG  ***  09/01/98  *********************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C*
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C***  END   ADD  ***  07/17/98  *********************************
     C***  BEGIN ADD  ***  02/05/99  **********************************
     C     PL0075        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      SCDBRR        ##RRN             9 0
     C***  END   ADD  ***  02/05/99  **********************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    EMCO#
     C                   PARM                    EMFND
     C                   PARM                    EMEMP#
     C                   PARM                    EMDIV
     C                   PARM      LDFYR         ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*** END ADD ***** 04/30/99 ***********************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
      ***** BEG ADD ******* 09/06/06 **********************************
      *
      *  MPN Provider
      *
     C     MPKY          KLIST
     C                   KFLD                    ALCO#
     C                   KFLD                    ALFND
     C                   KFLD                    ALEMP#
     C                   KFLD                    ALDIV
     C                   KFLD                    KKELFL
     C                   KFLD                    ALELID
      ***** END ADD ******* 09/06/06 **********************************
     C*
     C     KEY1          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C     KEY1A         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKNAME
     C                   KFLD                    KKELID
     C*
     C     KEY1B         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKELID
     C*
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY2A         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKNAME
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKELID
     C*
     C     KEY2B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKELID
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C*
     C     KEY3A         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    KKNAME
     C                   KFLD                    KKELID
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY3B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKELID
     C***  END   ADD  ***  07/17/98  *********************************
     C*
     C*  GROUP MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXDLDT
     C*
     C     CMKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C*  SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C*
     C*  ENTITY/LOCATION FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C*  ENDORS/DARTS CONTROL DEFINITION
     C*
     C     DXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*
     C*  ENDORSEMENT REQUEST WORKFILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
      *
      * KEY LIST FOR THE GROUP AUXILIARY FILE
      *
     C     F2KY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
      * KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    ENFND
     C                   KFLD                    ENEMP#
     C                   KFLD                    ENDIV
     C                   KFLD                    ENFYR
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*** END ADD ***** 04/30/99 ***********************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
      ***** BEG ADD ******* 09/06/06 **********************************
     C     *LIKE         DEFINE    MPELFL        KKELFL
      ***** END ADD ******* 09/06/06 **********************************
     C     *LIKE         DEFINE    ALNAME        KKNAME
     C     *LIKE         DEFINE    ALELID        KKELID
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C     *LIKE         DEFINE    ALFND         KKFND
     C     *LIKE         DEFINE    ALEMP#        KKEMP#
     C     *LIKE         DEFINE    ALDIV         KKDIV
     C***  END   ADD  ***  07/17/98  *********************************
     C***  BEG   CHG  ***  05/05/09  *********************************
     C*****      *LIKE     DEFN ALNAME    U1PDSC
     C* EXTEND U1PDSC LENGTH TO 100 CHARS TO ACCEPT EMPLOYER NAME PART 1 AND 2
     C                   MOVE      *BLANKS       U1PDSC          100
     C***  END   CHG  ***  05/05/09  *********************************
     C     *LIKE         DEFINE    ALNAME        U2PDSC
     C     *LIKE         DEFINE    ALMAD1        XXADR1
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        ZZNAME
     C     *LIKE         DEFINE    ALEFDT        XXEFDT
     C     *LIKE         DEFINE    ALEFDT        XXDATE
     C     *LIKE         DEFINE    ALDLDT        XXDLDT
      *** BEGIN ADD *** 04/03/98 ***********************************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      *** END   ADD *** 04/03/98 ***********************************************
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C     *LIKE         DEFINE    ALDIV         ZZDIV
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C     *LIKE         DEFINE    SCDBRR        SVDBRR
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C*** END ADD ***** 04/30/99 ***********************************************
     C*  Define and init fields for CHGOFA subroutine
     C                   MOVE      *BLANKS       XXOPFN            1
     C                   MOVE      *BLANKS       XXNMBR            2
     C                   MOVE      *BLANKS       XXMIS             3
     C***  END   ADD  ***  07/17/98  *********************************
      **** BEGIN ADD **** 02/12/98 *********************************************
     C                   Z-ADD     0             I1                3 0
      **** END   ADD **** 02/12/98 *********************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C*===============================================================
     C* ADJDIV - SUBROUTINE TO ADJUST THE SUBSET DIVISION
     C*===============================================================
     C*
     C     ADJDIV        BEGSR
     C*
     C     SSDIV         IFNE      *BLANKS
     C*
     C                   DO        5             X                 3 0
     C*
     C     S5DIV         IFEQ      *BLANK
     C                   MOVE      S4DIV         S5DIV
     C                   MOVE      S3DIV         S4DIV
     C                   MOVE      S2DIV         S3DIV
     C                   MOVE      S1DIV         S2DIV
     C                   CLEAR                   S1DIV
     C                   ELSE
     C                   LEAVE
     C                   ENDIF                                                  S5DIV=*BLANK
     C*
     C                   ENDDO                                                  X->5
     C*
     C                   ENDIF                                                  SSDIV<>*BLANKS
     C*
     C                   ENDSR                                                  ADJDIV
     C*
     C*===============================================================
     C* CHGOFA - SUBROUTINE TO CHANGE OPTION/FUNCTION ARRAY
     C*===============================================================
     C*  Before calling this subroutine make the following assignments:
     C*       XXOPFN = '1' if Option, '2' if Function
     C*       XXNMBR = Option/Function number to change
     C*            NOTE: Pass option/function numbers as 2 char fields.
     C*                  e.g. MOVE '05' XXNMBR or MOVE '20' XXNMBR
     C*       XXMIS = 'XXX' where each 'X' is Y or N depending on change rqrd
     C*
     C*  This subroutine when called evaluates array OF for XXOPFN and checks
     C*  OFD to see if it is the option/function that matches XXNMBR. If so, it
     C*  replaces 'XXX' with XXMIS.
     C*
     C     CHGOFA        BEGSR
     C*
     C                   MOVE      *OFF          FOUND             1
     C                   Z-ADD     1             I                 2 0
     C*
     C*  Go through array looking for a match
     C     FOUND         DOWEQ     *OFF
     C     OF(I)         ANDNE     '3'
     C*
     C     OF(I)         IFEQ      XXOPFN
     C*
     C                   MOVEL     OFD(I)        DUMNUM            2
     C*
     C     XXNMBR        IFEQ      DUMNUM
     C*
     C                   MOVEL(P)  OFD(I)        DUMOFD           30
     C                   MOVE      XXMIS         DUMOFD
     C                   MOVEL     DUMOFD        OFD(I)
     C                   MOVE      *ON           FOUND
     C*
     C                   ENDIF                                                  ZZNUM=DUMNUM
     C*
     C                   ENDIF                                                  OF,I=XXOPFN
     C*
     C                   ADD       1             I                              INCREMENT COUNT
     C*
     C                   ENDDO                                                  FOUND DOWEQ *OFF
     C*
     C                   ENDSR                                                  CHGOFA
     C***  END   ADD  ***  07/17/98  *********************************
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C*
     C     CHKSSM        BEGSR
     C*
     C     SSNAME        IFNE      *BLANKS
     C     SSPAD1        ORNE      *BLANKS
     C     SSELTP        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF                                                  SSNAME<>*BLANKS
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C                   MOVE      'Y'           VLDOPT            1
     C***  END   ADD  ***  07/17/98  *********************************
     C*
     C                   SELECT
     C*
     C     SCSEL         WHENEQ    '4'
      ***** BEG ADD ******* 05/25/10 **********************************
     C                   MOVE      'Y'           XXDLT             1
      ***** END ADD ******* 05/25/10 **********************************
     C*
     C  N90SCDLDT        IFNE      *ZERO
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCDLDT<>*ZERO
     C*
     C**N90****  ##LVL     IFLT 95
     C*********            SETON                     72  90
     C*********            ENDIF                           ##LVL<95
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C*  IF TODAY'S DATE IS LESS THAN EFFECTIVE DATE USE EFFECTIVE DATE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     XXDATE        IFLT      XXEFDT
     C                   Z-ADD     XXEFDT        XXDLDT
     C                   ELSE
     C                   Z-ADD     XXDATE        XXDLDT
     C                   ENDIF                                                  XXDATE<XXEFDT
     C*
     C*  IF THERE IS ANY CLAIM THAT EXISTS WITH AN ACCIDENT DATE > OR =
     C*  THE DELETE DATE, FLAG AN ERROR MESSAGE BUT ALLOW USER TO
     C*  TO OVERRIDE.
     C*
     C     CMKY          SETLL     PCM01
     C     CMKY1         READE     PCM01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   XFOOT     AR            XXPMTS           11 2
     C*
     C     CMSTAT        IFNE      'C'
     C     CMCTG1        ANDEQ     SCELID
     C     CMSTAT        OREQ      'C'
     C     XXPMTS        ANDNE     *ZEROS
     C     CMCTG1        ANDEQ     SCELID
     C*
     C*  ALLOW OVERRIDE ON ERROR MSG ONLY IF USER HAS PROPER AUTHORITY
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     ##LVL         ANDGE     WCSL03
     C*
     C                   MOVE      'N'           SCEOVR
     C*
     C                   EXFMT     WDWMSG
     C*
     C     SCEOVR        IFNE      'Y'
     C                   SETON                                        766090
     C                   ENDIF                                                  SCEOVR<>Y
     C*
     C                   ELSE
     C*
     C                   SETON                                        766090
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  CMSTAT<>C
     C*
     C  N90CMKY1         READE     PCM01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
      **** BEGIN ADD **** 09/11/98 *********************************************
      *
      * LABELS CANNOT BE PROCESSED FOR CANCELLED EMPLOYERS
      *
     C     SCSEL         WHENEQ    '19'
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
      *
     C     CX(I1)        IFNE      0
     C     CX(I1)        ANDLE     XXDATE
     C                   SETON                                        77  90
     C                   ENDIF                                                  CX,I1 IFNE 0
      **** END   ADD **** 09/11/98 *********************************************
     C                   ENDSL
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C                   Z-ADD     SCDBRR        SVDBRR
     C*** END ADD ***** 04/30/99 ***********************************************
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C                   EXSR      ADJDIV
     C***  END   ADD  ***  07/17/98  *********************************
     C*
     C     SSELTP        IFNE      *BLANKS
     C     SSELTP        ANDNE     'E'
     C     SSELTP        ANDNE     'L'
     C                   SETON                                        20  90
     C                   ENDIF                                                  SSELTP<>L/E/*BLANKS
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C***  Begin Add  ***  09/17/09  **************************************
     C*================================================================
     C* GTCNTY - Get the county information
     C*================================================================
     C     GTCNTY        BEGSR
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
     C*
     C     ALPST         IFNE      *BLANKS
     C     ALPST         CAT(P)    ALCNTY        XXKEY
     C                   ELSE
     C     ALMST         CAT(P)    ALCNTY        XXKEY
     C                   ENDIF
     C*
     C                   CLEAR                   CNDESC
     C     TBKY          CHAIN     PCN01                              60
     C     ALCNTY        CAT(P)    '-':1         XXCNTY           35
     C                   CAT       CNDESC:1      XXCNTY
     C*
     C                   ENDSR
     C***  End   Add  ***  09/17/09  **************************************
     C*
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C*
     C     INZSST        BEGSR
     C*
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C                   Z-ADD     SCFND         SSFND
     C***  END   ADD  ***  07/17/98  *********************************
     C                   Z-ADD     SCEMP#        SSEMP#
     C***  BEGIN CHG  ***  07/17/98  *********************************
     C**********           Z-ADDSCDIV     SSDIV
     C                   MOVEL     SCDIV         XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         SSDIV
     C***  END   CHG  ***  07/17/98  *********************************
     C                   CLEAR                   SSNAME
     C                   CLEAR                   SSPAD1
     C                   CLEAR                   SSELTP
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      *LOVAL        KKELID
     C                   MOVE      *LOVAL        KKNAME
     C***  BEGIN ADD  ***  07/17/98  ********************************
     C                   Z-ADD     *LOVAL        KKFND
     C                   Z-ADD     *LOVAL        KKEMP#
     C                   Z-ADD     *LOVAL        KKDIV
     C***  END   ADD  ***  07/17/98  ********************************
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C     *IN42         ANDEQ     *ON
     C***  END   ADD  ***  07/17/98  *********************************
     C                   MOVE      *LOVAL        KKELID
     C                   MOVEL     SCPST1        KKNAME
     C     *IN22         WHENEQ    *ON
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C     *IN42         ANDEQ     *ON
     C***  END   ADD  ***  07/17/98  *********************************
     C                   MOVEL     SCPST2        KKELID
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C     *IN21         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C                   MOVEL     SCPST1        KKNAME
     C                   MOVE      *LOVAL        KKELID
     C                   Z-ADD     *LOVAL        KKFND
     C                   Z-ADD     *LOVAL        KKEMP#
     C                   Z-ADD     *LOVAL        KKDIV
     C     *IN22         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C                   MOVE      SCPST9        KKFND
     C                   MOVE      SCPS9A        KKEMP#
     C                   MOVE      SCPS9B        KKDIV
     C                   MOVE      SCPS9C        KKELID
     C***  END   ADD  ***  07/17/98  *********************************
     C                   ENDSL
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCELID        KKELID
     C                   MOVEL     SCELNM        KKNAME
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C                   Z-ADD     SFFND         KKFND
     C                   Z-ADD     SFEMP#        KKEMP#
     C                   Z-ADD     SFDIV         KKDIV
     C***  END   ADD  ***  07/17/98  *********************************
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*===============================================================
     C* LDADDR - SUBROUTINE TO LOAD THE ADDRESS LINES
     C*===============================================================
     C*
     C     LDADDR        BEGSR
     C*
     C                   CLEAR                   DTA2
     C                   CLEAR                   CSZ
     C*
     C     ' '           CHECKR    XXADR1        XXLEN
     C                   Z-ADD     XXLEN         X                 3 0
     C*
     C     ' '           CHECKR    XXADR2        XXLEN             3 0
     C                   Z-ADD     XXLEN         Y                 3 0
     C*
     C     X             ADD       Y             Z                 3 0
     C*
     C                   MOVEA     XXADR1        DTA2
     C*
     C     XXADR2        IFNE      *BLANKS
     C*
     C     Z             IFGT      44
     C*
     C     22            SUB       Y             Y
     C                   MULT      2             Y
     C     22            ADD       1             Y
     C                   MOVEA     COMMA         DTA2(Y)
     C                   ADD       1             Y
     C                   MOVEA     *BLANK        DTA2(Y)
     C                   ADD       1             Y
     C                   MOVEA     XXADR2        DTA2(Y)
     C*
     C                   ELSE
     C*
     C                   ADD       1             X
     C                   MOVEA     COMMA         DTA2(X)
     C                   ADD       2             X
     C                   MOVEA     XXADR2        DTA2(X)
     C*
     C                   ENDIF                                                  Z>44
     C*
     C                   ENDIF                                                  XXADR2<>*BLANKS
     C*
     C                   MOVEA     XXCTY         CSZ
     C     ' '           CHECKR    XXCTY         XXLEN
     C     XXLEN         ADD       1             X
     C                   MOVEA     COMMA         CSZ(X)
     C                   ADD       1             X
     C                   MOVEA     *BLANK        CSZ(X)
     C                   ADD       1             X
     C                   MOVEA     XXST          CSZ(X)
     C                   ADD       3             X
     C                   MOVEA     XXZIP         CSZ(X)
     C*
     C                   MOVEA     DTA2          XDTA2            48
     C     ' '           CHECKR    XDTA2         XXLEN
     C     XXLEN         ADD       1             X
     C                   MOVEA     COMMA         DTA2(X)
     C                   ADD       2             X
     C                   MOVEA     CSZ           DTA2(X)
     C*
     C                   MOVEL     *BLANKS       XXADR1
     C                   MOVEL     *BLANKS       XXADR2
     C                   MOVEL     *BLANKS       XXCTY
     C                   MOVEL     *BLANKS       XXST
     C                   MOVEL     *BLANKS       XXZIP
     C*
     C                   ENDSR                                                  LDADDR
     C*
      ***  BEGIN ADD  ***  04/03/98  *********************************
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
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
      ******* BEGIN ADD ***** 08/07/98 ********************************
     C                   MOVE      XXQTE         QCQTE
      ******* END   ADD ***** 08/07/98 ********************************
     C***  Begin Add  08/10/09  *******************************************
     C                   SELECT
     C     XXQTE         WHENEQ    'Q'
     C                   MOVE      'Q'           QCQTE
     C                   OTHER
     C                   CLEAR                   QCQTE
     C                   ENDSL
     C***  End   Add  08/10/09  *******************************************
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C***  END   ADD  ***  04/03/98  *********************************
     C*
     C*================================================================
     C* KTDO - Process F19
     C*================================================================
     C*
     C*  F19=PRINT ALL LABELS
     C*
     C     KTDO          BEGSR
     C*
     C                   MOVE      SCEMP#        ##EMP#
     C                   MOVE      SCDIV         ##DIV
     C                   MOVE      *ZEROS        ##ELID
     C*
     C                   CALL      'W7550R'
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##ELID
     C                   PARM      'Z'           ##RTCD
     C                   PARM                    ##CO#
     C                   PARM      SCFND         ##FND
     C*
     C                   ENDSR                                                  KTDO
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C     VLDOPT        IFNE      'Y'
     C*
     C                   SELECT
B01  C     *IN42         WHENEQ    *OFF
     C     *IN21         ANDEQ     *ON
     C     SSFND         ANDEQ     *ZEROS
     C                   Z-ADD     DBRRNC        SCDBRR
B01  C     *IN42         WHENEQ    *OFF
     C     *IN21         ANDEQ     *ON
     C     SSFND         ANDNE     *ZEROS
     C                   Z-ADD     DBRRNB        SCDBRR
E01  C                   ENDSL
     C***** BEGIN DEL **** 03/30/99 ***********************************
     C**********           ELSE
     C**********
     C**********           MOVE 'N'       VLDOPT
     C***** END DEL ****** 03/30/99 ***********************************
     C                   ENDIF                                                  VLDOPT<>Y
     C*
     C     *IN42         IFEQ      *OFF
     C                   Z-ADD     ALFND         SFFND
     C                   Z-ADD     ALEMP#        SFEMP#
     C                   Z-ADD     ALDIV         SFDIV
     C                   ELSE
     C                   CLEAR                   SFFND
     C                   CLEAR                   SFEMP#
     C                   CLEAR                   SFDIV
E01  C                   ENDIF                                                  *IN42=*OFF
     C***  END   ADD  ***  07/17/98  *********************************
     C*
     C                   MOVEL     ALELID        SCELID
     C                   MOVEL     ALNAME        SCELNM
     C*
     C                   MOVEL(P)  ALNAME        X1NAME
     C                   MOVEL(P)  ALELID        X1ELID
     C                   MOVEL(P)  ALNAME        X2NAME
     C                   MOVEL(P)  ALELID        X2ELID
     C*
     C   21              MOVEL(P)  DATA1         SCDATA
     C*
     C   22              MOVEL(P)  DATA2         SCDATA
     C*
     C                   MOVEL     ALELTP        SCELTP
     C*
     C                   Z-ADD     ALEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     ALDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDLDT
     C*
     C*  LOAD MAILING ADDRESS
     C*
     C                   MOVEL     'Mail:'       SCMAIL
     C*
     C***  BEGIN CHG  ***  04/10/98  *********************************
     C********** EMMAD1    IFNE *BLANKS
     C**********
     C**********           MOVELEMMAD1    XXADR1
     C**********           MOVELEMMAD2    XXADR2 22
     C**********           MOVELEMMCTY    XXCTY  10
     C**********           MOVELEMMST     XXST    2
     C**********           MOVELEMMZIP    XXZIP   5
     C     ALMAD1        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  ALMAD1        XXADR1
     C                   MOVEL(P)  ALMAD2        XXADR2           22
     C                   MOVEL(P)  ALMCTY        XXCTY            10
     C                   MOVEL(P)  ALMST         XXST              2
     C                   MOVEL(P)  ALMZIP        XXZIP             5
     C***  END   CHG  ***  04/10/98  *********************************
     C*
     C                   EXSR      LDADDR
     C*
     C                   MOVEA     DTA2          SCMADR
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCMADR
     C*
     C                   ENDIF                                                  EMMAD1<>*BLANKS
     C*
     C*  LOAD PHYSICAL ADDRESS
     C*
     C                   MOVEL     'Phys:'       SCPHYS
     C*
     C***  BEGIN CHG  ***  04/10/98  *********************************
     C********** EMPAD1    IFNE *BLANKS
     C**********
     C**********           MOVELEMPAD1    XXADR1
     C**********           MOVELEMPAD2    XXADR2
     C**********           MOVELEMPCTY    XXCTY
     C**********           MOVELEMPST     XXST
     C**********           MOVELEMPZIP    XXZIP
     C     ALPAD1        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  ALPAD1        XXADR1
     C                   MOVEL(P)  ALPAD2        XXADR2
     C                   MOVEL(P)  ALPCTY        XXCTY
     C                   MOVEL(P)  ALPST         XXST
     C                   MOVEL(P)  ALPZIP        XXZIP
     C***  END   CHG  ***  04/10/98  *********************************
     C*
     C                   EXSR      LDADDR
     C*
     C                   MOVEA     DTA2          SCPADR
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCPADR
     C*
     C                   ENDIF                                                  EMPAD1<>*BLANKS
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* OPT7 - SUBROUTINE FOR OPTION 7
     C*===============================================================
     C*
     C* OPTION 7=OWNER/OFFICER INFORMATION
     C*
     C     OPT7          BEGSR
     C*
     C                   MOVE      @@CALL        ##CALL
     C*
     C     SCELTP        IFEQ      'L'
     C                   MOVE      '1'           ##CALL
     C                   ENDIF                                                  SCELTP=L
     C*
     C                   CALL      'W1057R'      ACDPL2                   99
     C*
     C                   ENDSR                                                  OPT7
     C*
     C*===============================================================
     C* OPT19 - SUBROUTINE FOR OPTION 19
     C*===============================================================
     C*
     C*  OPTION 19=PRINT LABEL
     C*
     C     OPT19         BEGSR
     C*
     C                   MOVE      SCEMP#        ##EMP#            9
     C                   MOVE      SCDIV         ##DIV             5
     C                   MOVE      SCELID        ##ELID           10
     C*
     C                   CALL      'W7550R'
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##ELID
     C                   PARM      'Z'           ##RTCD            1
     C                   PARM                    ##CO#
     C                   PARM      SCFND         ##FND
     C*
     C                   ENDSR                                                  OPT19
     C***  BEGIN ADD  ***  02/05/99  **********************************
     C*===============================================================
     C* OPT8 - SUBROUTINE FOR OPTION 8
     C*===============================================================
     C*
     C* OPTION 8=POLICY YEARS
     C*
     C     OPT8          BEGSR
     C*
     C                   MOVE      @@CALL        ##MODE
     C*
     C                   CALL      'W0075R'      PL0075                   99
     C*
     C                   ENDSR                                                  OPT8
     C***  END   ADD  ***  02/05/99  **********************************
      ***** BEG ADD ******* 09/06/06 **********************************
      *
      *===============================================================
      * OPT92 - MPN Provider
      *===============================================================
     C     OPT92         BEGSR
      *
     C                   MOVE      'Y'           KKELFL
     C     MPKY          CHAIN     PMP01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     MPRRN         #4RRN
     C                   MOVE      'C'           #4MODE
      *
     C                   ELSE
      *
     C                   Z-ADD     *ZERO         #4RRN
     C                   MOVE      'A'           #4MODE
     C                   ENDIF                                                  MPKY
      *
     C   09              MOVE      'D'           #4MODE
      *
     C                   CALL      'W0050R92'                           99
     C     *INKC         PARM                    #4KC              1
     C                   PARM                    #4KL              1
     C                   PARM                    #4MODE            1
     C                   PARM                    #4RRN             9 0          WMMPP
     C                   PARM      SCDBRR        #4RRN1            9 0          WMEMP
     C                   PARM      Q1PGM         #4PGM            10
     C*
     C                   ENDSR                                                  OPT92
     C*
     C*
      ***** END ADD ******* 09/06/06 **********************************
     C*
     C*
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C*
      ****** BEGIN ADD ***** 04/03/98 *********************************
     C     *INKN         CASEQ     *ON           KNDO
      ****** END   ADD ***** 04/03/98 *********************************
     C     *INKT         CASEQ     *ON           KTDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C     OTHOPT        BEGSR
     C*
     C                   MOVE      SCDBRR        ##RRN
     C*
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '19'          OPT19
     C***  BEGIN ADD  ***  02/05/99  **********************************
     C   31SCSEL         CASEQ     '8'           OPT8
     C***  END   ADD  ***  02/05/99  **********************************
      ***** BEG ADD ******* 09/06/06 **********************************
     C     SCSEL         CASEQ     '92'          OPT92
      ***** END ADD ******* 09/06/06 **********************************
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C***  BEG CHG ***** 08/27/07 ************************************
     C********** *IN42     IFEQ *OFF
     C**********           MOVE *ON       *INLR
     C**********           ENDIF                           *IN42=*OFF
     C                   MOVE      *ON           *INLR
     C***  END CHG ***** 08/27/07 ************************************
     C***  END   ADD  ***  07/17/98  *********************************
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C*
     C                   MOVEA     *BLANKS       NM2
     C                   MOVEA     *BLANKS       FN2
      ***** BEG ADD ******* 05/06/09 **********************************
     C                   MOVEA     *BLANKS       N22
     C                   MOVEA     *BLANKS       F22
      ***** END ADD ******* 05/06/09 **********************************
     C                   MOVEA     *BLANKS       P12
     C                   MOVEA     *BLANKS       P22
     C                   MOVEA     *BLANKS       P32
     C                   MOVEA     *BLANKS       PC2
     C                   MOVEA     *BLANKS       PS2
     C                   MOVEA     *ZEROS        PZ2
     C***  Begin Add  ***  09/17/09  **************************************
     C                   CLEAR                   CN2
     C***  End   Add  ***  09/17/09  **************************************
     C                   MOVEA     *BLANKS       M12
     C                   MOVEA     *BLANKS       M22
     C                   MOVEA     *BLANKS       M32
     C                   MOVEA     *BLANKS       MC2
     C                   MOVEA     *BLANKS       MS2
     C                   MOVEA     *ZEROS        MZ2
     C                   MOVEA     *ZEROS        FE2
     C                   MOVEA     *BLANKS       TB2
     C                   MOVEA     *BLANKS       ID2
     C                   MOVEA     *BLANKS       NA
     C                   MOVEA     *BLANKS       MA
     C                   MOVEA     *BLANKS       PA
     C                   MOVEA     *ZEROS        DD2
     C                   MOVEA     *BLANKS       DR2
     C                   MOVEA     *BLANKS       ST2
     C                   Z-ADD     1             X                 3 0
      ***** BEG ADD ******* 07/19/10 **********************************
     C                   Z-ADD     0             K                 3 0
      ***** END ADD ******* 07/19/10 **********************************
     C     ALKY          CHAIN(N)  DBRCDF2                            60
     C     *IN60         DOWEQ     '0'
      ***** BEG DEL ******* 05/06/09 **********************************
     C*********            MOVELALNAME    NM2,X
     C*********            MOVELALFDNM    FN2,X
      ***** END DEL ******* 05/06/09 **********************************
      ***** BEG ADD ******* 05/06/09 **********************************
      *
     C                   MOVEL(P)  ALNAME        NM2(X)
     C                   MOVEL(P)  ALNAM2        XXNM10           10
     C                   MOVE      XXNM10        NM2(X)
     C                   MOVE      ALNAM2        N22(X)
      *
     C                   MOVEL(P)  ALFDNM        FN2(X)
     C                   MOVEL(P)  ALFDN2        XXNM10           10
     C                   MOVE      XXNM10        FN2(X)
     C                   MOVE      ALFDN2        F22(X)
      *
      ***** END ADD ******* 05/06/09 **********************************
     C                   MOVEL     ALPAD1        P12(X)
     C                   MOVEL     ALPAD2        P22(X)
     C                   MOVEL     ALPCTY        PC2(X)
     C                   MOVEL     ALPST         PS2(X)
     C                   Z-ADD     ALPZIP        PZ2(X)
     C***  Begin Add  ***  09/17/09  **************************************
     C                   EXSR      GTCNTY
     C                   MOVEL(P)  XXCNTY        CN2(X)
     C***  End   Add  ***  09/17/09  **************************************
     C                   MOVEL     ALPZIP        XXZIPX
     C***  Begin Add  ***  04/23/07  **************************************
     C     ALPCTY        IFNE      *BLANKS
     C     ALPST         ORNE      *BLANKS
     C***  End   Add  ***  04/23/07  **************************************
     C     ALPCTY        CAT(P)    ',':0         P32(X)
     C                   CAT       ALPST:1       P32(X)
     C                   CAT       XXZIP5:1      P32(X)
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         P32(X)
     C                   CAT       XXZIP4:0      P32(X)
     C                   ENDIF
     C***  Begin Add  ***  04/23/07  **************************************
     C                   ENDIF
     C***  End   Add  ***  04/23/07  **************************************
     C*
     C                   MOVEL     ALMAD1        M12(X)
     C                   MOVEL     ALMAD2        M22(X)
     C                   MOVEL     ALMCTY        MC2(X)
     C                   MOVEL     ALMST         MS2(X)
     C                   Z-ADD     ALMZIP        MZ2(X)
     C                   MOVEL     ALMZIP        XXZIPX
     C***  Begin Add  ***  04/23/07  **************************************
     C     ALMCTY        IFNE      *BLANKS
     C     ALMST         ORNE      *BLANKS
     C***  End   Add  ***  04/23/07  **************************************
     C     ALMCTY        CAT(P)    ',':0         M32(X)
     C                   CAT       ALMST:1       M32(X)
     C                   CAT       XXZIP5:1      M32(X)
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         M32(X)
     C                   CAT       XXZIP4:0      M32(X)
     C                   ENDIF
     C***  Begin Add  ***  04/23/07  **************************************
     C                   ENDIF
     C***  End   Add  ***  04/23/07  **************************************
     C*
     C                   Z-ADD     ALFEIN        FE2(X)
     C                   MOVEL(P)  'BUSI'        XXTBID
     C                   MOVEL(P)  ALTPOB        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     GTDESC        TB2(X)
     C                   ELSE
     C                   MOVE      *BLANKS       TB2(X)
     C                   END
     C                   MOVEL     ALELID        ID2(X)
     C                   Z-ADD     ALDLDT        DD2(X)
     C                   MOVEL     ALDLRS        DR2(X)
      *
      ***** BEG DEL ******* 05/06/09 **********************************
     C*********  NM,X      IFNE NM2,X
     C*********  FN,X      ORNE FN2,X
      ***** END DEL ******* 05/06/09 **********************************
      ***** BEG ADD ******* 05/06/09 **********************************
     C     NM(X)         IFNE      NM2(X)
     C     N2(X)         ORNE      N22(X)
     C     FN(X)         ORNE      FN2(X)
     C     F2(X)         ORNE      F22(X)
      ***** END ADD ******* 05/06/09 **********************************
     C                   MOVE      'Y'           NA(X)
     C                   END
     C     M1(X)         IFNE      M12(X)
     C     M2(X)         ORNE      M22(X)
     C     M3(X)         ORNE      M32(X)
     C                   MOVE      'Y'           MA(X)
     C                   END
     C     P1(X)         IFNE      P12(X)
     C     P2(X)         ORNE      P22(X)
     C     P3(X)         ORNE      P32(X)
     C                   MOVE      'Y'           PA(X)
     C                   END
     C***  Begin Add  ***  09/17/09  **************************************
     C     CN(X)         IFNE      CN2(X)
     C                   MOVE      'Y'           CA(X)
     C                   ENDIF
     C***  End   Add  ***  09/17/09  **************************************
     C*** BEGIN ADD *** 04/07/00 ***********************************************
     C     FE(X)         IFNE      FE2(X)
     C                   MOVE      'Y'           FI(X)
     C                   END
     C*** END ADD ***** 04/07/00 ***********************************************
     C                   MOVE      ALSTAT        ST2(X)
     C                   MOVE      ALELTP        TP2(X)
      ***** BEG ADD ******* 07/19/10 **********************************
     C     ST2(X)        IFNE      ' '
     C                   ADD       1             K
     C                   ENDIF
      ***** END ADD ******* 07/19/10 **********************************
     C                   ADD       1             X
     C     ALKY          READE(N)  DBRCDF2                                60
     C                   END
      ***** BEG ADD ******* 07/19/10 **********************************
      ***** BEG DEL ******* 12/28/11 **********************************
     C*******    K         IFGT 1
     C*******              MOVE *BLANKS   ST2,1
     C*******              ENDIF
      ***** END DEL ******* 12/28/11 **********************************
      ***** END ADD ******* 07/19/10 **********************************
     C*
     C                   Z-ADD     1             X
     C     NM2(X)        DOWNE     *BLANKS
     C     X             ANDLT     500
      ***** BEG ADD ******* 05/06/09 **********************************
     C     N22(X)        ORNE      *BLANKS
     C     X             ANDLT     500
      ***** END ADD ******* 05/06/09 **********************************
     C*
     C     ST2(X)        IFEQ      'C'
     C     ST2(X)        OREQ      'A'
     C     ST2(X)        OREQ      'X'
     C*
     C*  GET ENDORS/DARTS INFORMATION
      ***** BEG ADD ******* 05/25/10 **********************************
     C     NA(X)         IFEQ      'Y'
     C     MA(X)         OREQ      'Y'
     C     PA(X)         OREQ      'Y'
     C     CA(X)         OREQ      'Y'
     C     FI(X)         OREQ      'Y'
     C     XXDLT         OREQ      'Y'
      ***** END ADD ******* 05/25/10 **********************************
     C*
     C     DXKY          CHAIN     PDX01                              60
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C*  SETUP FOR NEXT REQUEST NUMBER
     C*
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C*
     C                   MOVE      EMCO#         ENCO#
     C                   MOVE      EMFND         ENFND
     C                   MOVE      EMEMP#        ENEMP#
     C                   MOVE      EMDIV         ENDIV
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
     C*
     C                   MOVE      LDEEDT        SRDTI
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
     C                   MOVE      EMFEIN        ENFEN1
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
     C*** BEGIN ADD *** 02/14/02 ***********************************************
     C                   MOVE      *BLANKS       ENAELF
     C                   MOVE      *BLANKS       ENCELF
     C                   MOVE      *BLANKS       ENDELF
     C                   MOVE      *BLANKS       ENRMCD
     C*** END ADD ***** 02/14/02 ***********************************************
     C     ST2(X)        IFEQ      'A'
     C     TP2(X)        IFEQ      'E'
     C                   MOVE      'Y'           ENAELF
     C                   MOVEL     DXENAD        ENRMCD
     C                   ELSE
     C                   MOVE      'Y'           ENAELF
     C                   MOVEL     DXLCAD        ENRMCD
     C                   END
     C                   ELSE
     C     ST2(X)        IFEQ      'C'
     C     NA(X)         IFEQ      'Y'
     C     TP2(X)        IFEQ      'E'
     C                   MOVE      'Y'           ENCELF
     C                   MOVEL     DXENNC        ENRMCD
     C                   ELSE
     C                   MOVE      'Y'           ENCELF
     C                   MOVEL     DXLCNC        ENRMCD
     C                   END                                                    TP2,X='E'
     C                   END                                                    NA,X='Y'
     C***  Begin Add  ***  09/17/09  **************************************
     C     CA(X)         IFEQ      'Y'
     C                   MOVE      'Y'           ENCELF
     C     MA(X)         IFNE      'Y'
     C     PA(X)         ANDNE     'Y'
     C                   SELECT
     C     P12(X)        WHENNE    *BLANKS
     C     TP2(X)        ANDEQ     'E'
     C                   MOVEL     DXENPA        ENRMCD
     C     P12(X)        WHENNE    *BLANKS
     C     TP2(X)        ANDNE     'E'
     C                   MOVEL     DXLCPA        ENRMCD
     C     TP2(X)        WHENEQ    'E'
     C                   MOVEL     DXENMA        ENRMCD
     C                   OTHER
     C                   MOVEL     DXLCMA        ENRMCD
     C                   ENDSL
     C                   ENDIF
     C                   ENDIF
     C***  End   Add  ***  09/17/09  **************************************
     C     MA(X)         IFEQ      'Y'
     C     TP2(X)        IFEQ      'E'
     C                   MOVE      'Y'           ENCELF
     C                   MOVEL     DXENMA        ENRMCD
     C                   ELSE
     C                   MOVE      'Y'           ENCELF
     C                   MOVEL     DXLCMA        ENRMCD
     C                   END                                                    TP2,X='E'
     C                   END                                                    NA,X='Y'
     C     PA(X)         IFEQ      'Y'
     C     TP2(X)        IFEQ      'E'
     C                   MOVE      'Y'           ENCELF
     C                   MOVEL     DXENPA        ENRMCD
     C                   ELSE
     C                   MOVE      'Y'           ENCELF
     C                   MOVEL     DXLCPA        ENRMCD
     C                   END                                                    TP2,X='E'
     C                   END                                                    NA,X='Y'
     C*** BEGIN ADD *** 04/07/00 ***********************************************
     C     FI(X)         IFEQ      'Y'
     C                   MOVE      'Y'           ENCELF
     C                   MOVEL     DXMISC        ENRMCD
     C                   END                                                    FE,X='Y'
     C*** END ADD ***** 04/07/00 ***********************************************
     C                   ELSE
     C     ST2(X)        IFEQ      'X'
     C     TP2(X)        IFEQ      'E'
     C                   MOVE      'Y'           ENDELF
     C                   MOVEL     DXENDL        ENRMCD
     C                   ELSE
     C                   MOVE      'Y'           ENDELF
     C                   MOVEL     DXLCDL        ENRMCD
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
     C*
     C     LDMYRR        CHAIN     PMY01                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
     C*
     C*** BEGIN ADD *** 08/21/00 ***********************************************
     C                   MOVE      ID2(X)        ENELID
     C*** END ADD ***** 08/21/00 ***********************************************
     C*
     C*  LOAD BEFORE FIELDS
     C*
     C                   Z-ADD     FE(X)         ENOFI1
     C                   MOVEL     NM(X)         ENOEN1
     C                   MOVEL     FN(X)         ENOFN1
     C                   MOVEL     M1(X)         ENOM11
     C                   MOVEL     M2(X)         ENOM21
     C                   MOVEL     MC(X)         ENOMC1
     C                   MOVEL     MS(X)         ENOMS1
     C                   Z-ADD     MZ(X)         ENOMZ1
     C                   MOVEL     P1(X)         ENOP11
     C                   MOVEL     P2(X)         ENOP21
     C                   MOVEL     PC(X)         ENOPC1
     C                   MOVEL     PS(X)         ENOPS1
     C                   Z-ADD     PZ(X)         ENOPZ1
     C***  Begin Chg  ***  09/17/09  **************************************
     C**********           MOVE *BLANKS   ENOCN1
     C                   MOVEL     CN(X)         ENOCN1
     C***  End   Chg  ***  09/17/09  **************************************
     C                   MOVEL     *BLANKS       ENOC11
     C                   MOVEL     *BLANKS       ENOC21
     C                   MOVEL     *BLANKS       ENOC31
     C*
     C*  LOAD AFTER FIELDS
     C*
     C                   Z-ADD     FE2(X)        ENOFI2
     C                   MOVEL     NM2(X)        ENOEN2
     C                   MOVEL     FN2(X)        ENOFN2
     C                   MOVEL     M12(X)        ENOM12
     C                   MOVEL     M22(X)        ENOM22
     C                   MOVEL     MC2(X)        ENOMC2
     C                   MOVEL     MS2(X)        ENOMS2
     C                   Z-ADD     MZ2(X)        ENOMZ2
     C                   MOVEL     P12(X)        ENOP12
     C                   MOVEL     P22(X)        ENOP22
     C                   MOVEL     PC2(X)        ENOPC2
     C                   MOVEL     PS2(X)        ENOPS2
     C                   Z-ADD     PZ2(X)        ENOPZ2
     C***  Begin Chg  ***  09/17/09  **************************************
     C**********           MOVE *BLANKS   ENOCN2
     C                   MOVEL     CN2(X)        ENOCN2
     C***  End   Chg  ***  09/17/09  **************************************
     C                   MOVEL     *BLANKS       ENOC12
     C                   MOVEL     *BLANKS       ENOC22
     C                   MOVEL     *BLANKS       ENOC32
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
     C     F2KY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      F2CAR#        ENCAR1
     C                   END                                                    F2KY *IN60
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   ENDIF                                                  ELKY *IN60  OFF
     C                   MOVE      TP2(X)        ENELTP
     C*
     C     ST2(X)        IFEQ      'A'
     C     ST2(X)        OREQ      'X'
     C                   Z-ADD     0             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  NM2(X)        ENALIN
     C                   WRITE     PEN01
      ***** BEG ADD ******* 05/06/09 **********************************
      *
     C     N22(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  N22(X)        ENALIN
     C                   WRITE     PEN01
     C                   END
      ***** END ADD ******* 05/06/09 **********************************
     C*
     C     FN2(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  FN2(X)        ENALIN
     C                   WRITE     PEN01
     C                   END
      ***** BEG ADD ******* 05/06/09 **********************************
      *
     C     F22(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  F22(X)        ENALIN
     C                   WRITE     PEN01
     C                   END
      ***** END ADD ******* 05/06/09 **********************************
     C*
     C***  Begin Add  ***  04/23/07  **************************************
     C                   SELECT
     C     P12(X)        WHENNE    *BLANKS
     C     P22(X)        ORNE      *BLANKS
     C     P32(X)        ORNE      *BLANKS
     C***  End   Add  ***  04/23/07  **************************************
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  P12(X)        ENALIN
     C                   WRITE     PEN01
     C*
     C     P22(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  P22(X)        ENALIN
     C                   WRITE     PEN01
     C                   END
     C*
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  P32(X)        ENALIN
     C                   WRITE     PEN01
     C*
     C***  Begin Add  ***  04/23/07  **************************************
     C     M12(X)        WHENNE    *BLANKS
     C     M22(X)        ORNE      *BLANKS
     C     M32(X)        ORNE      *BLANKS
     C***  End   Add  ***  04/23/07  **************************************
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  M12(X)        ENALIN
     C                   WRITE     PEN01
     C*
     C     M22(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  M22(X)        ENALIN
     C                   WRITE     PEN01
     C                   END
     C*
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  M32(X)        ENALIN
     C                   WRITE     PEN01
     C*
     C                   ENDSL
     C***  End   Add  ***  04/23/07  **************************************
     C*
     C***  Begin Add  ***  09/17/09  **************************************
     C     CN2(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  CN2(X)        ENALIN
     C                   WRITE     PEN01
     C                   ENDIF
     C***  End   Add  ***  09/17/09  **************************************
     C*
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVE      FE2(X)        XXWK9A            9
     C     XXDES1        CAT(P)    XXWK9A:1      ENALIN
     C                   WRITE     PEN01
     C*
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  TB2(X)        ENALIN
     C                   WRITE     PEN01
     C*
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C     XXDES2        CAT(P)    ID2(X)        ENALIN
     C                   WRITE     PEN01
     C*
     C     ST2(X)        IFEQ      'X'
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  DR2(X)        ENALIN
     C                   WRITE     PEN01
     C                   END                                                    ST2,X='X'
     C*
     C                   END                                                    ST2,X='A,X'
     C*
     C     ST2(X)        IFEQ      'C'
     C*
     C                   Z-ADD     0             ENSEQ#
     C                   MOVEL(P)  NM(X)         ENBLIN
     C                   MOVEL(P)  NM2(X)        ENALIN
     C                   WRITE     PEN01
      ***** BEG ADD ******* 05/06/09 **********************************
     C     N2(X)         IFNE      *BLANKS
     C     N22(X)        ORNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  N2(X)         ENBLIN
     C                   MOVEL(P)  N22(X)        ENALIN
     C                   WRITE     PEN01
     C                   END
      ***** END ADD ******* 05/06/09 **********************************
     C     FN2(X)        IFNE      *BLANKS
     C     FN(X)         ORNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  FN(X)         ENBLIN
     C                   MOVEL(P)  FN2(X)        ENALIN
     C                   WRITE     PEN01
     C                   END
      ***** BEG ADD ******* 05/06/09 **********************************
     C     F22(X)        IFNE      *BLANKS
     C     F2(X)         ORNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  F2(X)         ENBLIN
     C                   MOVEL(P)  F22(X)        ENALIN
     C                   WRITE     PEN01
     C                   END
      ***** END ADD ******* 05/06/09 **********************************
     C*
     C     P1(X)         IFNE      P12(X)
     C     P2(X)         ORNE      P22(X)
     C     P3(X)         ORNE      P32(X)
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  P1(X)         ENBLIN
     C                   MOVEL(P)  P12(X)        ENALIN
     C                   WRITE     PEN01
     C     P22(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  P2(X)         ENBLIN
     C                   MOVEL(P)  P22(X)        ENALIN
     C                   WRITE     PEN01
     C                   END
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  P3(X)         ENBLIN
     C                   MOVEL(P)  P32(X)        ENALIN
     C                   WRITE     PEN01
     C                   END                                                    PHYS ADR CHG
     C*
     C     M1(X)         IFNE      M12(X)
     C     M2(X)         ORNE      M22(X)
     C     M3(X)         ORNE      M32(X)
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  M1(X)         ENBLIN
     C                   MOVEL(P)  M12(X)        ENALIN
     C                   WRITE     PEN01
     C     M22(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  M2(X)         ENBLIN
     C                   MOVEL(P)  M22(X)        ENALIN
     C                   WRITE     PEN01
     C                   END
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  M3(X)         ENBLIN
     C                   MOVEL(P)  M32(X)        ENALIN
     C                   WRITE     PEN01
     C                   END                                                    MAIL ADR CHG
     C*
     C***  Begin Add  ***  09/17/09  **************************************
     C     CN(X)         IFNE      CN2(X)
     C                   ADD       1             ENSEQ#
     C                   MOVEL     CN(X)         ENBLIN
     C                   MOVEL(P)  CN2(X)        ENALIN
     C                   WRITE     PEN01
     C                   ENDIF
     C***  End   Add  ***  09/17/09  **************************************
     C*
     C     FE(X)         IFNE      FE2(X)
     C                   ADD       1             ENSEQ#
     C                   MOVE      FE(X)         XXWK9A            9
     C     XXDES1        CAT(P)    XXWK9A:1      ENBLIN
     C                   MOVE      FE2(X)        XXWK9A            9
     C     XXDES1        CAT(P)    XXWK9A:1      ENALIN
     C                   WRITE     PEN01
     C                   END                                                    FED ID CHG
     C*
     C     TB(X)         IFNE      TB2(X)
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  TB(X)         ENBLIN
     C                   MOVEL(P)  TB2(X)        ENALIN
     C                   WRITE     PEN01
     C                   END                                                    TPOB CHG
     C*
     C                   END                                                    ST2,X='C'
     C*
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  *BLANKS       ENBLIN
     C                   MOVEL(P)  *BLANKS       ENALIN
     C                   WRITE     PEN01
     C*
     C*** BEG ADD ***** 05/25/10 ***********************************************
     C                   ENDIF
     C*** END ADD ***** 05/25/10 ***********************************************
     C                   END                                                    ST2,X=A,C,D
     C                   ADD       1             X
     C                   ENDDO                                                  DOW<=500
     C*
     C     ALKY          CHAIN     DBRCDF2                            60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      *BLANKS       ALSTAT
     C                   UPDATE    DBRCDF2
     C     ALKY          READE     DBRCDF2                                60
     C                   ENDDO                                                  DOW *IN60
     C*
     C                   END                                                    LDENDA<>' '
     C*** END ADD ***** 04/30/99 ***********************************************
     C                   ENDSR
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C*** BEG DEL ***** 03/05/98 ***********************************************
     C*** BEG ADD ***** 02/05/98 ***********************************************
     C***CHECK TO SEE IF CALLED FROM EMPLOYER MID TERM ENDORSEMENTS
     C***        *NAMVAR   DEFN *LDA      SVLDA
     C***        *LOCK     IN   *NAMVAR
     C***                  OUT  *NAMVAR
     C***        LDENDA    IFNE *ZEROS
     C***        LDENDA    ANDNE*BLANKS
     C***                  MOVELCLRFKY    OFD,5            OPT 7
     C***                  ENDIF
     C*** END ADD ***** 02/05/98 ***********************************************
     C*** END DEL ***** 03/05/98 ***********************************************
     C                   MOVE      *ON           *IN12
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C*
      **** BEGIN ADD ***** 08/07/98 **********************************
     C     Q1PRMS        IFGE      6
     C                   MOVE      @@QTE         XXQTE             1
     C                   ELSE
     C                   MOVE      *BLANKS       XXQTE             1
     C                   ENDIF
      **** END   ADD ***** 08/07/98 **********************************
      ***** BEG ADD ******* 05/25/10 **********************************
      *  ADD mid-term waiver
      *
     C     XXQTE         IFEQ      'A'
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'YNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  ADD
      *
      *  CHG mid-term waiver
     C     XXQTE         IFEQ      'C'
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  CHG
      *
      *  DLT mid-term waiver
      *
     C     XXQTE         IFEQ      'X'
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  DLT
      ***** END ADD ******* 05/25/10 **********************************
     C*
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C                   MOVE      *OFF          *IN42
     C*
     C                   SELECT
     C*
     C*  ALL ENTITIES/LOCATIONS
     C*
     C     Q1PRMS        WHENGE    5
     C     @@RRN2        ANDEQ     *ZEROS
     C     Q1PRMS        ORLT      5
     C*
     C                   CLEAR                   SCFND
     C                   CLEAR                   SCEMP#
     C                   CLEAR                   SCDIV
     C*
     C*  CHANGE FLAGS FOR OPT 4=DELETE    ?
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '04'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C*
     C*  CHANGE FLAGS FOR OPT 7=OWNERS/OFFICERS    ?
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '07'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C*
     C*  CHANGE FLAGS FOR OPT 19=PRINT LABELS      ?
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '19'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C*
     C*  CHANGE FLAGS FOR F6=ADD
     C*
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C*
     C*  CHANGE FLAGS FOR F14=DARTS  ?
     C*
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '14'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C*
     C*  CHANGE FLAGS FOR F19=PRINT ALL LABELS
     C*
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '19'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C*_______________________________________________________________
     C*
     C*  PASSED IN EMPLOYER
     C*
     C                   OTHER
     C*
     C                   MOVE      *ON           *IN42
     C*
     C                   Z-ADD     @@RRN2        ##RRN2
     C***  END   ADD  ***  07/17/98  *********************************
     C*
     C*  LOAD SCREEN HEADINGS
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     @@RRN2        CHAIN     PEM01                              60
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        ZZNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
      **** BEGIN DEL **** 02/12/98 *********************************************
     C******************** DO   5         I1      30
     C********************
     C********** CX,I1     IFNE *ZERO
     C******************** Z-ADDCX,I1     SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     SCCXDT
     C******************** ELSE
     C******************** LEAVE
     C******************** ENDIF                           CX,I1<>*ZERO
     C********************
     C******************** ENDDO                           I1->5
      **** END   DEL **** 02/12/98 *********************************************
      **** BEGIN ADD **** 02/12/98 *********************************************
     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
      *
     C     *ZERO         LOOKUP    EF(I1)                                 61
      *
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
     C     I1            IFNE      0
      *
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
      *
     C                   ENDIF                                                  I1 IFNE 0
      **** END   ADD **** 02/12/98 *********************************************
     C*
     C*  GET FUND MASTER INFORMATION
     C*
     C                   MOVEL     *BLANKS       SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C*
     C     FNEXDT        IFNE      *ZEROS
     C     SYSDT         COMP      FNEXDT                             09  09
     C                   ENDIF                                                  FNEXDT<>*ZEROS
     C*
     C                   CLEAR                   @@RRN
     C*
     C***** BEGIN ADD **** 06/11/99 ***********************************
     C*
     C*    IF CANCELLATION DATE IS LESS THAN OR EQUAL TO CURRENT DATE
     C*    DON'T DISPLAY F19=PRINT ALL LABELS OR OPTION 19=PRINT A
     C*    LABEL.
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C     CX(I1)        IFNE      0
     C     CX(I1)        ANDLE     XXDATE
     C*
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '19'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '19'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   ELSE
     C*
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '19'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '19'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   ENDIF                                                  <>0,LEXXDATE
     C***** END ADD ****** 06/11/99 ***********************************
     C***** BEG ADD ****** 05/27/10 ***********************************
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '07'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C                   ENDIF
     C***** END ADD ****** 05/27/10 ***********************************
     C*
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C                   ENDSL
     C*
     C     *IN42         IFEQ      *OFF
     C                   MOVEA     HDA           HD
     C                   ENDIF                                                  *IN42=*OFF
     C***  END   ADD  ***  07/17/98  *********************************
     C*
     C***  BEGIN ADD  ***  02/05/99  **********************************
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL14        COMP      'Y'                                    31
     C*
     C     *IN31         IFEQ      *OFF
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '08'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  *IN31
     C***  END   ADD  ***  02/05/99  **********************************
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C                   Z-ADD     *ZEROS        LDFYR
     C                   Z-ADD     *ZEROS        LDEND#
     C                   Z-ADD     *ZEROS        LDEEDT
     C                   Z-ADD     *ZEROS        LDMYRR
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C*
     C                   Z-ADD     1             X
     C                   MOVEA     *BLANKS       NM
      ***** BEG ADD ******* 05/06/09 **********************************
     C                   MOVEA     *BLANKS       N2
      ***** END ADD ******* 05/06/09 **********************************
     C                   MOVEA     *BLANKS       FN
      ***** BEG ADD ******* 05/06/09 **********************************
     C                   MOVEA     *BLANKS       F2
      ***** END ADD ******* 05/06/09 **********************************
     C                   MOVEA     *BLANKS       P1
     C                   MOVEA     *BLANKS       P2
     C                   MOVEA     *BLANKS       P3
     C                   MOVEA     *BLANKS       PC
     C                   MOVEA     *BLANKS       PS
     C                   MOVEA     *ZEROS        PZ
     C***  Begin Add  ***  09/17/09  **************************************
     C                   CLEAR                   CN
     C***  End   Add  ***  09/17/09  **************************************
     C                   MOVEA     *BLANKS       M1
     C                   MOVEA     *BLANKS       M2
     C                   MOVEA     *BLANKS       M3
     C                   MOVEA     *BLANKS       MC
     C                   MOVEA     *BLANKS       MS
     C                   MOVEA     *ZEROS        MZ
     C                   MOVEA     *ZEROS        FE
     C                   MOVEA     *BLANKS       TB
     C                   MOVEA     *BLANKS       ID
     C                   MOVEA     *ZEROS        DD
     C                   MOVEA     *BLANKS       DR
     C     ALKY          CHAIN(N)  DBRCDF2                            60
     C     *IN60         DOWEQ     '0'
      ***** BEG DEL ******* 05/06/09 **********************************
     C*********            MOVELALNAME    NM,X
     C*********            MOVELALFDNM    FN,X
      ***** END DEL ******* 05/06/09 **********************************
      ***** BEG ADD ******* 05/06/09 **********************************
      *
     C                   MOVEL(P)  ALNAME        NM(X)
     C                   MOVEL(P)  ALNAM2        XXNM10           10
     C                   MOVE      XXNM10        NM(X)
     C                   MOVE      ALNAM2        N2(X)
      *
     C                   MOVEL(P)  ALFDNM        FN(X)
     C                   MOVEL(P)  ALFDN2        XXNM10           10
     C                   MOVE      XXNM10        FN(X)
     C                   MOVE      ALFDN2        F2(X)
      *
      ***** END ADD ******* 05/06/09 **********************************
     C                   MOVEL     ALPAD1        P1(X)
     C                   MOVEL     ALPAD2        P2(X)
     C                   MOVEL     ALPCTY        PC(X)
     C                   MOVEL     ALPST         PS(X)
     C                   Z-ADD     ALPZIP        PZ(X)
     C***  Begin Add  ***  09/17/09  **************************************
     C                   EXSR      GTCNTY
     C                   MOVEL(P)  XXCNTY        CN(X)
     C***  End   Add  ***  09/17/09  **************************************
     C                   MOVEL     ALPZIP        XXZIPX
     C***  Begin Add  ***  04/23/07  **************************************
     C     ALPCTY        IFNE      *BLANKS
     C     ALPST         ORNE      *BLANKS
     C***  End   Add  ***  04/23/07  **************************************
     C     ALPCTY        CAT(P)    ',':0         P3(X)
     C                   CAT       ALPST:1       P3(X)
     C                   CAT       XXZIP5:1      P3(X)
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         P3(X)
     C                   CAT       XXZIP4:0      P3(X)
     C                   ENDIF
     C***  Begin Add  ***  04/23/07  **************************************
     C                   ENDIF
     C***  End   Add  ***  04/23/07  **************************************
     C                   MOVEL     ALMAD1        M1(X)
     C                   MOVEL     ALMAD2        M2(X)
     C                   MOVEL     ALMCTY        MC(X)
     C                   MOVEL     ALMST         MS(X)
     C                   Z-ADD     ALMZIP        MZ(X)
     C                   MOVEL     ALMZIP        XXZIPX
     C***  Begin Add  ***  04/23/07  **************************************
     C     ALMCTY        IFNE      *BLANKS
     C     ALMST         ORNE      *BLANKS
     C***  End   Add  ***  04/23/07  **************************************
     C     ALMCTY        CAT(P)    ',':0         M3(X)
     C                   CAT       ALMST:1       M3(X)
     C                   CAT       XXZIP5:1      M3(X)
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         M3(X)
     C                   CAT       XXZIP4:0      M3(X)
     C                   ENDIF
     C***  Begin Add  ***  04/23/07  **************************************
     C                   ENDIF
     C***  End   Add  ***  04/23/07  **************************************
     C                   Z-ADD     ALFEIN        FE(X)
     C                   MOVEL(P)  'BUSI'        XXTBID
     C                   MOVEL(P)  ALTPOB        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     GTDESC        TB(X)
     C                   ELSE
     C                   MOVE      *BLANKS       TB(X)
     C                   END
     C                   MOVEL     ALELID        ID(X)
     C                   Z-ADD     ALDLDT        DD(X)
     C                   MOVEL     ALDLRS        DR(X)
     C                   ADD       1             X
     C     ALKY          READE(N)  DBRCDF2                                60
     C                   END
     C*
     C*** BEGIN DEL *** 05/20/99 ***********************************************
     C*********************END                             LDENDA<>' '
     C*** END DEL ***** 05/20/99 ***********************************************
     C*
     C*** BEGIN CHG *** 05/20/99 ***********************************************
     C***********@@QTE     IFEQ 'A'
     C     XXQTE         IFEQ      'A'
     C*** END CHG ***** 05/20/99 ***********************************************
     C                   MOVEL     OFD(2)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(2)
     C                   MOVEL     OFD(3)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(3)
     C                   MOVEL     OFD(10)       XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(10)
     C*** BEG   ADD *** 05/27/10 ***********************************************
     C     LDENDA        IFEQ      *ZEROS
     C     LDENDA        ANDEQ     *BLANKS
     C*** END   ADD *** 05/27/10 ***********************************************
     C                   MOVEL     OFD(5)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C*** BEG   ADD *** 05/27/10 ***********************************************
     C                   ENDIF
     C*** END   ADD *** 05/27/10 ***********************************************
     C                   MOVEL     XXLINE        OFD(5)
     C                   MOVEL     OFD(6)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(6)
     C                   ELSE
     C*** BEGIN CHG *** 05/20/99 ***********************************************
     C***********@@QTE     IFEQ 'C'
     C     XXQTE         IFEQ      'C'
     C*** END CHG ***** 05/20/99 ***********************************************
     C                   MOVEL     OFD(2)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(2)
     C                   MOVEL     OFD(3)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(3)
     C                   MOVEL     OFD(10)       XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(10)
     C*** BEG   ADD *** 05/27/10 ***********************************************
     C     LDENDA        IFEQ      *ZEROS
     C     LDENDA        ANDEQ     *BLANKS
     C*** END   ADD *** 05/27/10 ***********************************************
     C                   MOVEL     OFD(5)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C*** BEG   ADD *** 05/27/10 ***********************************************
     C                   ENDIF
     C*** END   ADD *** 05/27/10 ***********************************************
     C                   MOVEL     XXLINE        OFD(5)
     C                   MOVEL     OFD(6)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(6)
     C                   ELSE
     C*** BEGIN CHG *** 05/20/99 ***********************************************
     C***********@@QTE     IFEQ 'X'
     C     XXQTE         IFEQ      'X'
     C*** END CHG ***** 05/20/99 ***********************************************
     C                   MOVEL     OFD(2)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(2)
     C                   MOVEL     OFD(3)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(3)
     C                   MOVEL     OFD(10)       XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(10)
     C*** BEG   ADD *** 05/27/10 ***********************************************
     C     LDENDA        IFEQ      *ZEROS
     C     LDENDA        ANDEQ     *BLANKS
     C*** END   ADD *** 05/27/10 ***********************************************
     C                   MOVEL     OFD(5)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C*** BEG   ADD *** 05/27/10 ***********************************************
     C                   ENDIF
     C*** END   ADD *** 05/27/10 ***********************************************
     C                   MOVEL     XXLINE        OFD(5)
     C                   MOVEL     OFD(6)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(6)
     C                   ELSE
     C                   MOVEL     OFD(2)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(2)
     C                   MOVEL     OFD(3)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(3)
     C                   MOVEL     OFD(10)       XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(10)
     C*** BEG   ADD *** 05/27/10 ***********************************************
     C     LDENDA        IFEQ      *ZEROS
     C     LDENDA        ANDEQ     *BLANKS
     C*** END   ADD *** 05/27/10 ***********************************************
     C                   MOVEL     OFD(5)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C*** BEG   ADD *** 05/27/10 ***********************************************
     C                   ENDIF
     C*** END   ADD *** 05/27/10 ***********************************************
     C                   MOVEL     XXLINE        OFD(5)
     C                   MOVEL     OFD(6)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(6)
     C                   END
     C                   END
     C                   END
     C*** END ADD ***** 04/30/99 ***********************************************
     C*** BEGIN ADD *** 05/20/99 ***********************************************
     C                   END                                                    LDENDA<>' '
     C*** END ADD ***** 05/20/99 ***********************************************
      ***** BEG ADD ******* 09/06/06 **********************************
      *
      *  MPN Provider (CLT132 only)
      *
     C     CLTID         IFEQ      '132'
      *** BEG CHG ***** 06/25/07 **************************************
     C*********  @@QTE     ANDNE'Q'
     C     XXQTE         ANDNE     'Q'
      *** END CHG ***** 06/25/07 **************************************
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '92'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
      *
      ***** END ADD ******* 09/06/06 **********************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
     C                   MOVE      'N'           VLDOPT
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C     *IN42         ANDEQ     *ON
     C     KEY1          READE     DBRCDF                                 95
     C*
     C     *IN22         WHENEQ    *ON
     C     *IN42         ANDEQ     *ON
     C     KEY1          READE(N)  DBRCDF2                                95
     C*
     C     *IN21         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDNE     *ZEROS
     C     KEY3          READE     DBRCDFB                                95
     C  n95              eval      dbrrn = dbrrnb
     C*
     C     *IN22         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDNE     *ZEROS
     C     KEY3          READE(N)  DBRCDF2                                95
     C*
     C     *IN21         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDEQ     *ZEROS
     C     KEY2          READE     DBRCDFC                                95
     C  n95              eval      dbrrn = dbrrnc
     C*
     C     *IN22         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDEQ     *ZEROS
     C     KEY2          READE(N)  DBRCDF2                                95
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  READ
     C*
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C*
     C     READP         BEGSR
     C*
     C                   MOVE      'N'           VLDOPT
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C     *IN42         ANDEQ     *ON
     C     KEY1          READPE    DBRCDF                                 95
     C*
     C     *IN22         WHENEQ    *ON
     C     *IN42         ANDEQ     *ON
     C     KEY1          READPE(N) DBRCDF2                                95
     C*
     C     *IN21         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDNE     *ZEROS
     C     KEY3          READPE    DBRCDFB                                95
     C  n95              eval      dbrrn = dbrrnb
     C*
     C     *IN22         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDNE     *ZEROS
     C     KEY3          READPE(N) DBRCDF2                                95
     C*
     C     *IN21         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDEQ     *ZEROS
     C     KEY2          READPE    DBRCDFC                                95
     C  n95              eval      dbrrn = dbrrnc
     C*
     C     *IN22         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDEQ     *ZEROS
     C     KEY2          READPE(N) DBRCDF2                                95
     C*
     C                   ENDSL
     C***  END   CHG  ***  07/17/98  *********************************
     C*
     C                   ENDSR                                                  READP
     C*
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C*
     C     SETGT         BEGSR
     C*
     C***  BEGIN CHG  ***  07/17/98  *********************************
     C***21***** KEY1A     SETGTDBRCDF               96
     C***22***** KEY1B     SETGTDBRCDF2              96
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C     *IN42         ANDEQ     *ON
     C     KEY1A         SETGT     DBRCDF                             96
     C*
     C     *IN22         WHENEQ    *ON
     C     *IN42         ANDEQ     *ON
     C     KEY1B         SETGT     DBRCDF2                            96
     C*
     C     *IN21         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDNE     *ZEROS
     C     KEY3A         SETGT     DBRCDFB                            96
     C*
     C     *IN22         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDNE     *ZEROS
     C     KEY3B         SETGT     DBRCDF2                            96
     C*
     C     *IN21         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDEQ     *ZEROS
     C     KEY2A         SETGT     DBRCDFC                            96
     C*
     C     *IN22         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDEQ     *ZEROS
     C     KEY2B         SETGT     DBRCDF2                            96
     C*
     C                   ENDSL
     C***  END   CHG  ***  07/17/98  *********************************
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C***  BEGIN CHG  ***  07/17/98  *********************************
     C***21***** KEY1A     SETLLDBRCDF               96
     C***22***** KEY1B     SETLLDBRCDF2              96
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C     *IN42         ANDEQ     *ON
     C     KEY1A         SETLL     DBRCDF                             96
     C*
     C     *IN22         WHENEQ    *ON
     C     *IN42         ANDEQ     *ON
     C     KEY1B         SETLL     DBRCDF2                            96
     C*
     C     *IN21         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDNE     *ZEROS
     C     KEY3A         SETLL     DBRCDFB                            96
     C*
     C     *IN22         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDNE     *ZEROS
     C     KEY3B         SETLL     DBRCDF2                            96
     C*
     C     *IN21         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDEQ     *ZEROS
     C     KEY2A         SETLL     DBRCDFC                            96
     C*
     C     *IN22         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSFND         ANDEQ     *ZEROS
     C     KEY2B         SETLL     DBRCDF2                            96
     C*
     C                   ENDSL
     C***  END   CHG  ***  07/17/98  *********************************
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C*** BEGIN DEL *** 04/30/99 ***********************************************
     C*********************CLEARSCR01S
     C*********************MOVELSDLT      SCDATA    P
     C*** END DEL ***** 04/30/99 ***********************************************
     C     SVDBRR        CHAIN     PFRCDF                             60
     C                   EXSR      MVDBSC
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C*** BEG CHG ****** 05/05/09 **********************************************
     C*** U1PDSC is now 100 chars, changed input values accordingly
     C***************      PARM 40        STRLEN  30
     C                   PARM      100           STRLEN            3 0
     C*** END CHG ****** 05/05/09 **********************************************
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR                                                  SLCTRD
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C*
     C     SUBSET        BEGSR
     C***  BEGIN ADD  ***  07/17/98  *********************************
     C*
     C*  CHECK ENTITY/LOCATION GROUP AGAINST SUBSET GROUP
     C*
     C     SSFND         IFNE      *ZEROS
     C     SSFND         ANDNE     ALFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFND<>*ZERO
     C***  END   ADD  ***  07/17/98  *********************************
     C*
     C*  CHECK ENTITY/LOCATION EMP#/DIV AGAINST SUBSET EMP#/DIV
     C*
     C     SSEMP#        IFNE      *ZEROS
     C***  BEGIN CHG  ***  07/17/98  *********************************
     C********** SSEMP#    ANDNEALEMP#
     C********** SSDIV     ANDNEALDIV
     C                   MOVE      SSDIV         ZZDIV
     C     SSEMP#        IFNE      ALEMP#
     C***  END   CHG  ***  07/17/98  *********************************
     C                   MOVE      *OFF          MEETSS
     C***  BEGIN CHG  ***  07/17/98  *********************************
     C                   ENDIF                                                  SSEMP#<>ALEMP#
     C     SSDIV         IFNE      *BLANKS
     C     ZZDIV         ANDNE     ALDIV
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSDIV<*BLANKS
     C                   ELSE
     C                   CLEAR                   SSDIV
     C***  END   CHG  ***  07/17/98  *********************************
     C                   ENDIF                                                  SSEMP#<>*ZERO
     C*
     C*  CHECK ENTITY/LOC NAME AGAINST SEARCH STRING
     C*
     C     SSNAME        IFNE      *BLANKS
     C*
     C*** BEG CHG ****** 05/05/09 **********************************************
     C*********  LO:UP     XLATEALNAME    U1PDSC    P
     C*** Concat ALNAME and ALNAM2 and then move to U1PDSC
     C                   MOVE      *BLANKS       U1PDSC
     C     ALNAME        CAT(P)    ALNAM2        U1PDSC
     C     LO:UP         XLATE(P)  U1PDSC        U1PDSC
     C*** END CHG ****** 05/05/09 **********************************************
     C     LO:UP         XLATE(P)  SSNAME        U2PDSC
     C*
     C                   EXSR      STRING
     C*
     C                   ENDIF                                                  SSNAME<>*BLANKS
     C*
     C*  CHECK EMPLOYERS PHYSICAL STREET ADDR 1 AGAINST SEARCH STRING
     C*
     C     SSPAD1        IFNE      *BLANKS
     C*
     C     LO:UP         XLATE(P)  ALPAD1        U1PDSC
     C     LO:UP         XLATE(P)  SSPAD1        U2PDSC
     C*
     C                   EXSR      STRING
     C*
     C                   ENDIF                                                  SSPAD1<>*BLANKS
     C*
     C*  CHECK IF ENTITY/LOCATION/BOTH
     C*
     C     SSELTP        IFNE      *BLANKS
     C*
     C                   SELECT
     C*
     C     SSELTP        WHENEQ    'L'
     C     ALELTP        ANDNE     'L'
     C                   MOVE      *OFF          MEETSS
     C*
     C     SSELTP        WHENEQ    'E'
     C     ALELTP        ANDNE     'E'
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDSL                                                              <>*ZEROS
     C*
     C                   ENDIF                                                  SSELTP<>*BLANKS
     C*
     C                   ENDSR                                                  SUBSET
     C*
** Column headings
Opt Entity/Loc Name  E/L Number  Typ  Eff Date  Delete Date
Opt E/L Number  Entity/Loc Name  Typ  Eff Date  Delete Date
** Column headings
Opt Entity/Loc Name  E/L Number  Typ  Eff Date   Dlt Date  Grp  Employer   Div
Opt E/L Number  Entity/Loc Name  Typ  Eff Date   Dlt Date  Grp  Employer   Div
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1077=Owner/Officer          YYN
1088=Policy years           YYN
11919=Print label           YYN
19292=MPN Provider          NNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               YYY
214F14=Darts                YYN
215F15=Fold/truncate        YYY
217F17=Subset               YYY
219F19=Print all labels     YYN
3
