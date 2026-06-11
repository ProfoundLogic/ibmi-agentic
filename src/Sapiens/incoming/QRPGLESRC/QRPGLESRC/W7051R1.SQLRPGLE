     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7051R1 - PRINT ENDORSEMENTS
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM WILL PRINT A REQUESTED ENDORSEMENT.  A DARTS     *
     H* RECORD WILL ALSO BE WRITTEN TO INDICATE THE INTENT OF THE     *
     H* ENDORSEMENT.                                                  *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   VARIOUS                                                     *
     H* CALLS TO:                                                     *
     H*                                                               *
     6* CREATED: 01/19/98        PROGRAMMER: 57        PROJECT: CM027 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/24/25 C:65  Project: Phw#118336  (Carewest)                *
     H*                Copy the large field holding before and after  *
     H*                information over to the WXMLENP1 file.         *
     H*                                                               *
     H* 12/12/24 C:342 Support: 118064 (AmWins)                       *
     H*                1) endorsement form WC890600B to print         *
     H*                 Fed name as well aling with DBA name.         *
     H*                2) Do not print DBA name twice if county is    *
     H*                 changed                                       *
     H*                                                               *
     H* 08/28/24 C:300 Support: 117426 (AmWins)                       *
     H*                1) Client Name is not printing in after box on *
     H*                Mid-term Endorsement for Deletion of Division. *
     H*                2) Client Name is not printing in before/after *
     H*                box on Mid-term Endorsement for Address Change *
     H*                                                               *
     H* 02/22/24 C:300 Support: 115088 (AmWins)                       *
     H*                DBA name when longer than 40 characters is not *
     H*                printing on cover letter of Mid-term Endorsement
     H*                for Addition of Division.                      *
     H*                                                               *
     H* 08/03/23 C:340 WC2670                                         *
     H*                Allow bills/premium notices to be emailed with *
     H*                mid-term endorsement document.                 *
     H*                Update XMLPREP File with XPEMAILD/XPEMAILF with*
     H*                XPDir and XPFile respectively.                 *
     H*                                                               *
     H* 10/26/18 C:267 Support: 50562                                 *
     H*                Update WXDA1L workfile for Division ADD with   *
     H*                with current job# for later use.               *
     H*                                                               *
     H* 05/17/18 C:136 Support: 50318                                 *
     H*                Pass output type to W7051C2 so that it doesn't *
     H*                try to print m/t endorsements if flagged for   *
     H*                Scriptura output only.                         *
     H*                                                               *
     H* 06/14/17 C:136 Support: 47681                                 *
     H*                Add ldPeoImpFlg to SVLDA just as a position    *
     H*                holder                                         *
     H*                                                               *
     H* 12/01/16 C:136 Project: SY371                                 *
     H*                Add Scriptura processing                       *
     H*                                                               *
     H* 11/10/10 C:33  Project: 139WPL                                *
     H*                Produce HCN program information.               *
     H*                                                               *
     H* 06/08/10 C:61  SUPPORT WA139 72906                            *
     H*                Always produce output for mid-term Revised     *
     H*                billing endorsement.                           *
     H*                                                               *
     H* 02/23/10 C:61  SUPPORT WA045 70612                            *
     H*              - Insure Description is populated correctly for  *
     H*                Entity/Location Mid-Term Endorsements.         *
     H*                                                               *
     H* 12/02/09 C:61  PROJECT 132WMP                                 *
     H*              - Change to always return Waiver State QQWVST.   *
     H*              - Correct change made 02/25/09, return correct   *
     H*                employer mailing state.                        *
     H*                                                               *
     H* 10/13/09 C:61  PROJECT 139WLR                                 *
     H*                Add processing to identify "clt139 Misc End"   *
     H*                processing.                                    *
     H*                                                               *
     H* 02/25/09 C:190 Seq#: 66412                                    *
2    H*                Pass in the user who actually entered the      *
2    H*                division  and use that ID for the Add          *
     H*                Division DARTS being written.                  *
     H*                                                               *
     H* 04/04/08 C:61  Project: 139WCI  Seq#: WA136/62597             *
     H*                Remove calls to SRLINE that attempt to prevent *
     H*                word splits.                                   *
     H*                                                               *
     H* 03/14/08 C:33  Project: 139WCI  Seq#: WA136/62597             *
     H*                Ensure 2nd line employer names are printed.   *
     H*                                                               *
     H* 10/01/07 C:33  Project: WC1330  Seq# DR139/59595              *
     H*          C:61  If processing "ADD of Division" endorsement;  *
     H*                assign DARTS remark to WMEMP/EMIUSR.           *
     H*                                                               *
     H* 01/29/07 C:184 PROJECT WC1525                                 *
     H*                Add parameter to indicate "UT Waiver of        *
     H*                Subrogation" form WC430305 is to be printed.   *
     H*                                                               *
     H* 11/15/06 C:61  PROJECT 139WCI                                 *
     H*                Print PEO Name if PEO group.
     H*                                                               *
     H* 07/10/06 C:61  SUPPORT WA099 53230                            *
     H*                Produce Mid-term Endorsements even if DARTS    *
     H*                code not specified.                            *
     H*                                                               *
     H* 06/23/06 C:61  PROJECT WC577                                  *
     H*                Produce Waiver Endorsement even if no DARTS    *
     H*                code.                                          *
     H*                                                               *
     H* 06/13/06 C:61  PROJECT WC1362                                 *
     H*              - Add paramenter for rating state.               *
     H*                                                               *
     H* 06/10/06 C:61  SUPPORT WA129 52050                            *
     H*                DARTS remark for Mid-Term Revised Billing      *
     H*                ENDORSE document was not being produced for    *
     H*                multi division policy.  Change to produce one  *
     H*                DARTS remark regardless of WMENP/ENREQ# value. *
     H*                                                               *
     H* 04/03/06 C:61  SUPPORT WA109 51864                            *
     H*              - Do not split lines for given request across    *
     H*                two forms.                                     *
     H*              - Skip line between requests.                    *
     H*                                                               *
     H* 12/05/05 C:33  Project: 139WAJ                                *
     H*                Create PDF attachment to DARTS.                *
     H*                                                               *
     H* 11/09/05 C:61  PROJECT WC1333                                 *
     H*                Add parameter to indicate "CA Waiver of        *
     H*                Subrogation" form WC040306 is to be printed.   *
     H*                                                               *
     H* 08/17/05 C:61  PROJECT WC1266                                 *
     H*                Add processing for Mid-Term                    *
     H*                "Rescission Division Deletion".                *
     H*                                                               *
     H* 05/11/05 C:61  SUPPORT  46984                                 *
     H*                Use info from first State reporting division   *
     H*                located.                                       *
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING FOR EXTENDED LENGTH EMPLR NAMES *
     H*                ON THE HEADER OF THE ENDORSEMENT.              *
     H*                                                               *
     H* 09/22/04 C:33  Project: WC1202                                *
     H*                Add processing for extended length emplr names.*
     H*                                                               *
     H* 06/09/04 C:61  PROJECT WC1223                                 *
     H*                Correct typo in DART remark verbiage.          *
     H*                                                               *
     H* 05/15/04 C:61  PROJECT WC1223                                 *
     H*                Add text to DART remark to indicate if user    *
     H*                requested mailing address change for all       *
     H*                entities / locations.                          *
     H*
     H*  01/15/04 C:09  PROJECT: CM027 (CLIENT #72)
     H*                 Remove change from 8/21/01 (see below).  We
     H*                 We should not be printing the billing address
     H*                 on the midterm endorsement form.  I even
     H*                 checked with Toni H. on this (Seq #28666) and
     H*                 she agrees this should have never been done.
     H*                 (Copied in from C072.BASE by C: 132.)
     H*
     H*  07/10/02 C:09  SEQ#: WA091/33337
     H*                 Make sure we include policy year when pulling
     H*                 the most current active agent to print on the
     H*                 endorsement.
     H*
     H*  04/16/02 C:90  PROJECT: SEQ#32101
     H*                 Removed 3/22/02 change. Correct overflow issue
     H*                 by moving overflow section (IX>20) after the
     H*                 add to the IX counter.
     H*
     H*  03/22/02 C:90  SEQ#: WA102/31565
     H*                 Correct overflow counter to print new page.
     H*
     H*  02/14/02 C:09  SEQ#: WA107/30152
     H*                 Need to remove processing that was loading
     H*                 owner/officer names in the DARTS comment on
     H*                 ADDITION/DELETION OF OWNER/OFFICERS.
     H*                 User could add or delete any number of own/offc
     H*                 To see the names, user will need to access
     H*                 the resolved endorsement document.
     H*
     H*  08/21/01 C:90  SEQ#: WA101/28666
     H*                 Add processing to include the alternate
     H*                 employer address on the endorsement if one
     H*                 was entered.
     H*
     H*  05/16/01 C:90  SEQ#: WA112/27402
     H*                 Include agent name in DARTS for agent change.
     H*
     H*  03/30/01 C:90  PROJECT: WC577
     H*                 Write a DARTS record for an Agent change.
     H*
     H*  03/19/01 C:90  PROJECT: WC577
     H*                 LOAD THE NEW ADDRESS IN THE HEADING ON AN
     H*                 ADDRESS CHANGE ENDORSEMENT.
     H*
     H*  08/10/00 C:90  PROJECT: CM027
     H*                 LOAD MISCELLANEOUS ENDORSEMENT DESCRIPTION FOR
     H*                 DARTS.
     H*
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H* 01/19/00 C:15 Support: WC048/20018                            *
     H*               Changed code which retrieves the next available *
     H*               DARTS seq# to be processed immediately before   *
     H*               the WRITE statement.                            *
     H*                                                               *
     H* 10/21/99 C:113 PROJECT: CM027                                 *
     H*                Changed to correctly record Deletion of Owner/ *
     H*                Officer.                                       *
     H*                                                               *
     H* 08/12/99 C:113 SUPPORT: WA093/#19196                          *
     H*                Exp. Mod dates were not converted to MMDDYYYY  *
     H*                in DARTS remark.                               *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H* 05/17/99 C:09 PROJECT: CM027  (WA048/SEQ #17503)              *
     H*               NEED TO ATTACH "OTHER WORKPLACES" EXT FOR       *
     H*               ADDITION/DELETION OF EMPLOYER DIVISION.         *
     H*                                                               *
     H* 05/06/99 C:09 PROJECT: CM044                                  *
     H*               NO NEED TO INDICATE IN DARTS REMARK ENT/LOC     *
     H*               CONTACT NAME CHANGES.                           *
     H*                                                               *
     H* 04/01/99 C:09 PROJECT: SUPPORT #13455/#15369                  *
     H*               CORRECT PROBLEM WITH DUPLICATE DARTS BEING      *
     H*               WRITTEN.                                        *
     H*                                                               *
     H* 02/03/99 C:09 PROJECT: SUPPORT (WA081/SEQ# 16382)             *
     H*               NEED TO "FORCE" THE AGENT #/NAME TO BE THE MOST *
     H*               CURRENT AGENT ASSIGNED TO THE POLICY.  THIS IS  *
     H*               WHAT SHOULD BE PRINTING, NOT THE AGENT IN EFFECT*
     H*               AS OF THE ENDORSEMENT EFFECTIVE DATE.           *
     H*                                                               *
     H* 02/01/99 C:09 PROJECT: SUPPORT (WA091/SEQ# 16333)             *
     H*               ADDED CARRIER POLICY #. POLICY FROM DATE AND    *
     H*               POLICY TO DATE (IN APPROPRIATE PRINT FORMAT)    *
     H*               ALSO, EMPLOYER INFO SHOULD BE FOR ST RPT DIV.   *
     H*                                                               *
     H* 07/01/98 C:57  PROJECT: 95WBH                                 *
     H*                ALLOW FOR MID TERM WAIVER OF SUBRO CHGS.       *
     H*                                                               *
     H* 08/11/98 C:09  PROJECT: SUPP  WA095  SEQ# 13037               *
     H*                WAIVER OF SUBRO SCHEDULE NOT GETTING CLEARED   *
     H*                WHEN ENDORSEMENT GOES TO 2 OR MORE PAGES.      *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*****************************************************************
     F*
     FWTMYP     IF   E             DISK
     FWMENL3    IF   E           K DISK
     FWMENP1    O    E             DISK
     FSTCTL1    IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMSHL6    UF   E           K DISK
     FSSYLF     IF   E           K DISK
     FWXICP     O    E             DISK
     FWMRPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     F                                     infds(dbfds)
      *  Rate Code
     FWMRSL     IF   E           K DISK
      *
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     fwxmlenp1  o    e             disk    usropn
     fxmlprel1  uf   e           k disk
      * New Division ADD - workfile
     FWXDA1L    UF   E           K DISK

     D XXCOMM          DS
     D  XXCOM1                 1    255
     D  XXCOM2               256    275
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D                 DS
     D  XXWKDT                 1      8
     D  XXWKMM                 1      2
     D  XXWKDD                 3      4
     D  XXWKCC                 5      6
     D  XXWKYY                 7      8
     D  XXWKCY                 5      8
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  BF                     1   1000
     D                                     DIM(20)
     D  ENBLN1                 1     50
     D  ENBLN2                51    100
     D  ENBLN3               101    150
     D  ENBLN4               151    200
     D  ENBLN5               201    250
     D  ENBLN6               251    300
     D  ENBLN7               301    350
     D  ENBLN8               351    400
     D  ENBLN9               401    450
     D  ENBLNA               451    500
     D  ENBLNB               501    550
     D  ENBLNC               551    600
     D  ENBLND               601    650
     D  ENBLNE               651    700
     D  ENBLNF               701    750
     D  ENBLNG               751    800
     D  ENBLNH               801    850
     D  ENBLNI               851    900
     D  ENBLNJ               901    950
     D  ENBLNK               951   1000
     D                 DS
     D  AF                     1   1000
     D                                     DIM(20)
     D  ENALN1                 1     50
     D  ENALN2                51    100
     D  ENALN3               101    150
     D  ENALN4               151    200
     D  ENALN5               201    250
     D  ENALN6               251    300
     D  ENALN7               301    350
     D  ENALN8               351    400
     D  ENALN9               401    450
     D  ENALNA               451    500
     D  ENALNB               501    550
     D  ENALNC               551    600
     D  ENALND               601    650
     D  ENALNE               651    700
     D  ENALNF               701    750
     D  ENALNG               751    800
     D  ENALNH               801    850
     D  ENALNI               851    900
     D  ENALNJ               901    950
     D  ENALNK               951   1000
     D                 DS
     D  XXZIP                  1      9
     D  XXZPM1                 1      5
     D  XXZPM2                 6      9
     D                 DS
     D  XXPOL1                 1     15
     D  ENFND                  1      3  0
     D  ENEMP#                 4     12  0
     D  ENFYR                 13     15  0
     D                 DS
     D  XXWK5A                 1      5
     D  XXEM1                  1      1
     D  XXEM2                  2      5
     D                 DS
     D  XXWK9A                 1      9
     D  XXPR1                  1      2
     D  XXSF1                  3      9
     D*
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D  LDPBCG                27     27
     D  LDOWCG                28     28
     D  LDPEOIMPFLG           29     29
     d ldOutputFile                  60    Overlay(svlda:191)
     d*** Beg Add *** 08/03/23 *** WC2670 **************************
     d xxDart                         1    overlay(svlda:251)
     d xxDartRMCD                     5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxEmailTo                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
     d keyEmail                      40    overlay(svlda:56)
     d
     d/copy sPrDate
     d*** End Add *** 08/03/23 *** WC2670 **************************

     d/copy sPrLetter
     d/copy sPrXML

     d CheckObjInLibl  pr                  ExtPgm('SRCHKOBJ')
     d  objName                      10    const
     d  objType                       7    const
     d  objNotinLibl                  1

     d isMassReq       s              1n   inz
     d massFlag        s              1
     d xmlLink         s                   like(XPLink)
     d xmlFileName     s                   like(XPXML)
     d xmlFileBlank    s                   like(XPXML)
     d xmlFile         s                   like(XPXML)
     d isUseSpoolOut   s              1n   inz
     d isUseXML        S              1n   inz
     d isUseScript     S              1n   inz
     d strLang         s             60
     d strLangCode     s            250
     d objNotinLibl    s              1
     d outString       s             10    inz(*blanks)
     d outReturn       s            250    inz(*blanks)
     d outPrefix       s              2    inz('WC')
     d outXmlFile      s             61
     d processType     c                   const('Mid-Term Endorsement')
     d xxForm          s             15a
     d ##Form          s             15a

     d StLetter        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)

     d dbfds           ds
     d  binElrrn             397    400b 0

     d intElrrn        s              9  0 inz(0)
     d*** Beg Add *** 08/03/23 *** WC2670 **************************
     d date7           s              7  0
     d time6           s              6  0
     d*** End Add *** 08/03/23 *** WC2670 **************************

     D XXCM00          C                   CONST('Requestor:')
     D XXCM01          C                   CONST('Name Change')
     D XXCM02          C                   CONST('Old DBA/Emplr Name:')
     D XXCM03          C                   CONST('New DBA/Emplr Name:')
     D XXCM04          C                   CONST('Federal ID Change')
     D XXCM05          C                   CONST('Old FEIN #:')
     D XXCM06          C                   CONST('New FEIN #:')
     D XXCM07          C                   CONST('Type Of Business Cha-
     D                                     nge')
     D XXCM08          C                   CONST('Addition of Owner/Of-
     D                                     ficer(s)')
     D XXCM09          C                   CONST('Name:')
     D XXCM10          C                   CONST('Nominal Change')
     D XXCM11          C                   CONST('Mailing & Physical A-
     D                                     ddress Change')
     D XXCM12          C                   CONST('Mailing Address Chan-
     D                                     ge')
     D XXCM13          C                   CONST('Physical Address Cha-
     D                                     nge')
     D XXCM14          C                   CONST('Deletion of Location')
     D XXCM15          C                   CONST('Deletion Date:')
     D XXCM16          C                   CONST('Reason:')
     D XXCM17          C                   CONST('Deletion of Entity')
     D XXCM18          C                   CONST('Addition of Location')
     D XXCM19          C                   CONST('Addition of Entity')
     D XXCM20          C                   CONST('Change of Entity:')
     D XXCM21          C                   CONST('Change of Location:')
     D XXCM22          C                   CONST('Miscellaneous Change')
     D XXCM23          C                   CONST('Addition of Division')
     D XXCM24          C                   CONST('Deletion of Division')
     D XXCM25          C                   CONST('Old County:')
     D XXCM26          C                   CONST('New County:')
     D XXCM27          C                   CONST('Increased Limit Chan-
     D                                     ge')
     D XXCM28          C                   CONST('Old Limits:')
     D XXCM29          C                   CONST('New Limits:')
     D XXCM30          C                   CONST('Old Exp Rating Mod-E-
     D                                     ff Dt:')
     D XXCM31          C                   CONST('New Exp Rating Mod-E-
     D                                     ff Dt:')
     D XXCM32          C                   CONST('Payroll/Class Code C-
     D                                     hange')
     D XXCM33          C                   CONST('Interim Adjustment O-
     D                                     f Premium')
     D XXCM34          C                   CONST('Correction to Certif-
     D                                     icate of Self-Insura-
     D                                     nce')
     D XXCM35          C                   CONST('Nature of Business C-
     D                                     hange')
     D XXCM36          C                   CONST('Risk Id Change')
     D XXCM37          C                   CONST('New Contingent Mod-E-
     D                                     ff Dt:')
     D XXCM38          C                   CONST('Addition of Waiver o-
     D                                     f Subro:')
     D XXCM39          C                   CONST('Deletion of Waiver o-
     D                                     f Subro:')
     D XXCM40          C                   CONST('Waiver of Subro Chan-
     D                                     ge:')
     D XXCM41          C                   CONST('Deletion of Owner/Of-
     D                                     ficer(s)')
     D XXCM42          C                   CONST('Agent Change:')
     D XXCM43          C                   CONST('User requested maili-
     D                                     ng address change fo-
     D                                     r all entities/locat-
     D                                     ions')
     D XXCM44          C                   CONST('Rescission Division -
     D                                     Deletion')
     D HCNEND          C                   CONST('WC420408')
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *==============================================================
      *  KLIST
      *==============================================================
      *
      *  Rating State
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C*
     C*  KEY LIST FOR THE ENDORSEMENT MASTER FILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXJOB
     C                   KFLD                    XXEND#
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE SUBMISSION HISTORY FILE
     C*
     C     SHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    ENREQ#
     C                   KFLD                    ENDT
     C                   KFLD                    ENTIME
     C*
     C*  KEY LIST FOR SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    ##KEY             2
     C*
     C*  CARRIER FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C     EMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ENAGT#
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
     C* WXDA1L workfile
     C*
     C     DAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXJOB
     C*
      *==============================================================
      * *LIKE DEFN
      *==============================================================
     C     *LIKE         DEFINE    ENREQ#        SVREQ#
     C     *LIKE         DEFINE    ENCO#         XXCO#
     C     *LIKE         DEFINE    ENFND         XXFND
     C     *LIKE         DEFINE    ENFYR         XXFYR
     C     *LIKE         DEFINE    ENEMP#        XXEMP#
     C     *LIKE         DEFINE    ENDIV         XXDIV
     C     *LIKE         DEFINE    ENEND#        XXEND#
     C     *LIKE         DEFINE    ENREQ#        XXREQ#
     C     *LIKE         DEFINE    ENDT          XXDATE
     C     *LIKE         DEFINE    ENELID        XXELID
     C     *LIKE         DEFINE    ENJOB         XXJOB
     C     *LIKE         DEFINE    ENPDT8        XXDAT8
     C     *LIKE         DEFINE    ENEED8        XXEED8
     C     *LIKE         DEFINE    ENPAD3        XXPCSZ
     C     *LIKE         DEFINE    ENMAD3        XXMCSZ
     C     *LIKE         DEFINE    AAFEIN        XXFEIN
     C     *LIKE         DEFINE    AAMOD         XXMOD
     C     *LIKE         DEFINE    AAAGT#        XXAGT#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQJOB             6
     C                   PARM                    QQEND#            5
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFLR            80
     C                   PARM                    QQOOCG            1
     C                   PARM                    QQPBCG            1
     C                   PARM                    QQOWCG            1
     C                   PARM                    QQ313             1
     C                   PARM                    QQ4306            1
     C                   PARM                    QQPDF             1
     C                   PARM                    QQJRST            2
     C                   PARM                    QQWVST            2
     C                   PARM                    QQMISC            1
     C                   Parm                    qqOutPutType      1
     C                   Parm                    qqXmlFlowName    61
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DtCYMDold     PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DtMDYYold     PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *  LINE FORMAT
      *
     C     LINEPL        PLIST
     C                   PARM                    ##LINE          256
     C                   PARM                    ##LEN             3 0
     C                   PARM                    ##REM           256

      /Free
       //*** Beg Add *** 08/03/23 *** WC2670 **************************
       Exec Sql Set Option Commit = *None;
       //*** End Add *** 08/03/23 *** WC2670 **************************
       isUseSpoolOut = UseSpoolOut(processType);
       isUseScript = UseScriptura(processType);
       isUseXML = UseScriptura(processType)
        or Use3rdXml(processType);
       Select;
       When isUseSpoolOut and not(isUseScript);
          qqOutPutType = 'P';
       When not(isUseSpoolOut) and isUseScript;
          qqOutPutType = 'S';
       Other;
          qqOutPutType = 'B';
       Endsl;
      /end-free

     C                   TIME                    DSDTTM
     C                   Z-ADD     *ZERO         IX                2 0
     C                   MOVE      *ON           FIRST             1
     C                   MOVE      *ON           FIRSTD            1
     C                   MOVE      *OFF          XXOOCG            1
     C                   MOVE      *OFF          XXMISC            1
     C                   MOVE      *OFF          XXPBCG            1
     C                   MOVE      *OFF          XXOWCG            1
     C                   MOVE      *OFF          XX313             1
     C                   MOVE      *OFF          X04306            1
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DtCYMDold
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQJOB         XXJOB
     C                   MOVE      QQEND#        XXEND#
     C                   MOVE      *BLANKS       QQDCID
     C                   MOVE      *BLANKS       QQFLR
     C*
     C                   MOVE      *BLANKS       XXPDF
     C     Q1PRMS        IFGE      16
     C                   MOVE      QQPDF         XXPDF             1
     C                   ENDIF
     C*
     C**************************************************************************
     C*  MAIN LINE - BEGINS
     C**************************************************************************
      *
      * Get Data Area Info
      *
     C                   MOVE      *ZEROS        SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
      *
      *  Get Employer Division State
      *
     C     EMKY2         CHAIN     PEM01                              60
     C                   MOVE      *BLANKS       SVIUSR           10
      *
     C     *IN60         IFEQ      *OFF
      *
     C     EMPST         IFNE      *BLANKS
     C                   MOVE      EMPST         XXEMST            2
     C                   ELSE
     C                   MOVE      EMMST         XXEMST
     C                   ENDIF
      *
     C     EMKY1         SETGT     PEM01
     C     EMKY1         READPE    PEM01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EMIUSR        SVIUSR
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  Get Rating State of Division
      *
     C     Q1PRMS        IFGE      17
     C     ELKY          CHAIN     PEL01                              60
     C  N60RSKY          CHAIN     PRS01                              60
     C  N60              MOVE      RSJRST        QQJRST
     C                   ENDIF
     C*
     C*  PROCESS ALL ENDORSEMENT REQUESTS
     C*
      /Free
       If isUseXml;
          Open Wxmlenp1;
          intElRrn = binElRrn;
          outReturn = processType;
          xmlFile = GetXMLFileName('E':intElRrn:0:0:
           outPrefix:outString:outReturn);
          Chain xmlFile xp100;
          ENxmlkey = XPxmlt;
          qqXmlFlowName = XPxml + '$';
          In *lock *dtaara;
             ldOutputFile = XPfile;
          Out *dtaara;
          XPdata = 'Y';
          XPPgm  = Q1Pgm;
          XPUser = Q1User;
          XPTime = dsTime;
          XPDt   = xxDate;
          Update xp100;
        //*** Beg Add *** 08/03/23 *** WC2670 **************************
        // Email other documents created along with this endorsment
          If xxEmail = 'Y' and keyEmail <> *blank;
             time6 = TimeTo6(SystemTime());
             date7 = DateTo7(SystemDate());
             Exec sql
              update XMLPREP
               set (XPemailD,XPemailF,XPmail,XPmailAdd,XPuser,XPpgm,XPdt,XPtime)
                = (:XPdir, :XPfile, ' ', ' ', :q1User, :q1pgm, :date7, :time6)
               where XPemailD = :keyEmail;
          Endif;
        //*** End Add *** 08/03/23 *** WC2670 **************************
       Endif;
      /End-free

     C     ENKY          SETLL     PEN01
     C     ENKY          READE     PEN01                                  40
     C                   Z-ADD     ENREQ#        SVREQ#
      *
     C     *IN40         DOWEQ     '0'
     C*
     C     FIRST         IFEQ      *ON
     C                   EXSR      SR1ST
     C                   ENDIF
     C                   ADD       1             IX
     C*
     C                   SELECT
     C     ENMISC        WHENNE    *BLANK
     C  N30              MOVEL     ENALIN        AF(IX)
     C  N30              MOVE      *ON           *IN30
     C     ENNCFL        WHENEQ    'Y'
     C     ENSEQ#        ANDEQ     *ZERO
     C                   MOVEL     ENENM1        BF(IX)
     C                   MOVEL     ENANM1        AF(IX)
     C                   ADD       1             IX
     C                   MOVEL     ENENM2        BF(IX)
     C                   MOVEL     ENANM2        AF(IX)
     C                   ADD       1             IX
     C     ENNCFL        WHENEQ    'Y'
     C     ENSEQ#        ANDNE     *ZERO
     C                   MOVEL     ENEFN1        BF(IX)
     C                   MOVEL     ENAFN1        AF(IX)
     C                   ADD       1             IX
     C                   MOVEL     ENEFN2        BF(IX)
     C                   MOVEL     ENAFN2        AF(IX)
     C                   ADD       1             IX
     c*** Beg Add *** 02/22/24 *************************************
     c* Addition of Division Flag
     C     ENADDF        WHENEQ    'Y'
     C     ENSEQ#        ANDEQ     *ZERO
     C                   MOVEL     ENANM1        AF(IX)
     C                   ADD       1             IX
     C                   MOVEL     ENANM2        AF(IX)
     C                   ADD       1             IX
     c* Deletion of Division Flag
     C     ENDLDF        WHENEQ    'Y'
     C     ENSEQ#        ANDEQ     *ZERO
     C                   MOVEL     ENENM1        BF(IX)
     c*** Beg Add *** 08/28/24 *************************************
     C                   MOVEL     ENENM1        AF(IX)
     c*** End Add *** 08/28/24 *************************************
     C                   ADD       1             IX
     C                   MOVEL     ENENM2        BF(IX)
     c*** Beg Add *** 08/28/24 *************************************
     C                   MOVEL     ENENM2        AF(IX)
     c*** End Add *** 08/28/24 *************************************
     C                   ADD       1             IX
     c*** End Add *** 02/22/24 *************************************

     c*** Beg Add *** 08/28/24 *************************************
     c* Address Change Flag
     c*** Beg Chg *** 12/12/24 *************************************
     C***                WHEN      (ENADFL='Y' or ENPAFL='Y' or ENMAFL='Y')
     C***                          and ENSEQ# = 0
     C                   WHEN      (ENADFL='Y' or ENPAFL='Y' or ENMAFL='Y')
     C                             and ENSEQ# = 0 and ENCNTF<>'Y'
     C                   MOVEL     ENFDN1        BF(IX)
     C                   MOVEL     ENFDN2        AF(IX)
     C                   ADD       1             IX
     c*** End Chg *** 12/12/24 *************************************
     C                   MOVEL     ENENM1        BF(IX)
     C                   MOVEL     ENENM1        AF(IX)
     C                   ADD       1             IX
     C                   MOVEL     ENENM2        BF(IX)
     C                   MOVEL     ENENM2        AF(IX)
     C                   ADD       1             IX
     c* Also move Address line 1
     C                   MOVEL     ENBLIN        BF(IX)
     C                   MOVEL     ENALIN        AF(IX)
     C                   ADD       1             IX
     c*** End Add *** 08/28/24 *************************************
     C                   OTHER
     C                   MOVEL     ENBLIN        BF(IX)
     C                   MOVEL     ENALIN        AF(IX)
     C                   ENDSL
     C*
     C*  WRITE A DARTS RECORD
      *
     C     ENSEQ#        IFEQ      *ZERO
      *
     C     ENMSFL        IFEQ      'S'
     C                   MOVE      *ON           XX313
      *
     C     XXEMST        IFEQ      'CA'
     C                   MOVE      *ON           X04306
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  Owner Officer Change
      *
     C     ENNOFL        IFEQ      'A'
     C     ENNOFL        OREQ      'C'
     C     ENNOFL        OREQ      'D'
     C                   MOVE      *ON           XXOOCG
     C                   ENDIF
      *
      *  CLT139 MISC CHANGE END
      *
     C     ENMISC        IFEQ      'A'
     C     ENMISC        OREQ      'C'
     C     ENMISC        OREQ      'D'
     C                   MOVE      *ON           XXMISC            1
     C                   ENDIF
      *
      *  Print Bill Change
      *
     C     ENILFL        IFEQ      'Y'
     C     ENERFL        OREQ      'Y'
     C     ENCRFL        OREQ      'Y'
     C     ENIAPF        OREQ      'Y'
     C                   MOVE      *ON           XXPBCG
     C                   ENDIF
      *
      *  Other Workplace Changes
      *
     C     ENAELF        IFEQ      'Y'
     C     ENCELF        OREQ      'Y'
     C     ENDELF        OREQ      'Y'
     C     ENADDF        OREQ      'Y'
     C     ENDLDF        OREQ      'Y'
     C     ENRDDF        OREQ      'Y'
     C                   MOVE      *ON           XXOWCG
     C                   ENDIF
      *
      /Free
       If ENrmcd <> *blanks and isUseSpoolOut = *On;
          Exsr Darts;
       Endif;
      /End-free
      *
     C                   ENDIF
     C*
     C     ENKY          READE     PEN01                                  40
      *
      *  If new request
      *
     C     ENREQ#        IFNE      SVREQ#
     C                   Z-ADD     ENREQ#        SVREQ#
     C                   ADD       1             IX
     C     20            SUB       IX            IXR               3 0
      *
      *   if do not have six lines left on page to write next request,
      *   then write to new page
      *
     C     IXR           IFLT      6
     C     ENKY          READPE    PEN01                                  40
     C                   EXSR      OTHINF
     C                   WRITE     PEN011
      /Free
       If isUseXML;
          Write penxml;
       Endif;
      /End-free
     C                   Z-ADD     *ZERO         IX
     C                   MOVE      *BLANKS       BF
     C                   MOVE      *BLANKS       AF
     C     ENKY          READE     PEN01                                  40
     C                   ENDIF
      *
     C                   ENDIF
      ***
      *
      *  if last line on page, write to new page
      *
     C     IX            IFGE      20
     C     ENKY          READPE    PEN01                                  40
     C                   EXSR      OTHINF
     C                   WRITE     PEN011
      /Free
       If isUseXML;
          Write penxml;
       Endif;
      /End-free
     C                   Z-ADD     *ZERO         IX
     C                   MOVE      *BLANKS       BF
     C                   MOVE      *BLANKS       AF
     C     ENKY          READE     PEN01                                  40
     C                   ENDIF
      *
     C                   ENDDO
     C*
     C*  IF IX > 0, WRITE A WPENP1 RECORD
     C*
     C     IX            IFGT      *ZERO
     C                   EXSR      OTHINF
     C                   WRITE     PEN011
      /Free
       If isUseXML;
          Write penxml;
       Endif;
      /End-free
     C                   ENDIF
      /Free
       If isUseXML;
          Close Wxmlenp1;
       Endif;
      /End-free
     C**************************************************************************
     C*  MAIN LINE - ENDS
     C**************************************************************************
     C*
     C*  UPDATE SUBMISSION HISTORY RECORD FOR ELECTRONIC SUBMISSIONS
     C*  WITH DARTS SEQUENCE NUMBER
     C*
     C     SHKY          CHAIN     PSH01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE(P)   Q#SEQ#        SHSEQ#
     C                   UPDATE    PSH01
     C                   ENDIF
      *
       // For a division ADD in mid-term endorsement,
       //   UPD workfile WXDA1L rcd with current job# for this job
       //   for future use with STLETTER
      /Free
       If ENADDF = 'Y';
          Chain DAKY pda01;
          If %found(WXDA1L);
             PDNJBN = Q1JOBN;
             Update pda01;
             unlock wxda1l;
          EndIf;
       EndIf;
      /end-free
     C*
     C*  MOVE FOLDER & DOC ID TO OUTPUT PARMS
     C*
     C                   MOVEL     Q2FLR         QQFLR
     C                   MOVEL     Q2DCID        QQDCID
     C                   MOVE      XXOOCG        QQOOCG
     C                   MOVE      XXPBCG        QQPBCG
     C                   MOVE      XXOWCG        QQOWCG
     C                   MOVE      XX313         QQ313
      *
     C     Q1PRMS        IFGE      15
     C                   MOVE      X04306        QQ4306
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      18
     C*
     C     XX313         IFEQ      '1'
     C                   MOVE      XXEMST        QQWVST
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       QQWVST
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      19
     C                   MOVE      XXMISC        QQMISC
     C                   ENDIF
     C*
     C*  IF PREMIUM BEARING CHGS AND/OR OTHER WORKPLACE CHGS OCCURRED
     C*  UPDATE THE LDA FIELDS
     C*
     C     XXPBCG        IFEQ      *ON
     C     XXOWCG        OREQ      *ON
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   MOVE      XXPBCG        LDPBCG
     C                   MOVE      XXOWCG        LDOWCG
     C                   OUT       *DTAARA
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
     C**************************************************************************
     C*  SR1ST - FIRST TIME PROCESS SUBROUTINE
     C**************************************************************************
     C     SR1ST         BEGSR
     C                   MOVE      *OFF          FIRST
     C*
     C*  WRITE A MERGED FORMS DATA RECORD
     C*
     C     '.inc(END'    CAT(P)    'ORSE,':0     ICSTMT
     C                   CAT       'AMTEMP':0    ICSTMT
     C                   CAT       ',)':0        ICSTMT
     C*
     C                   WRITE     PIC01
     C                   FEOD      WXICP
     C*
     C*  DETERMINE IF CLIENT IS ENROLLED IN "OV/400"
     C*
     C                   MOVEL     'SY'          ##KEY
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*  GET DOC ID AND FOLDER NAME FOR RESOLUTION OF DOCUMENT TO OV/400
     C*
     C                   MOVE      *BLANKS       Q2RTCD
     C     XXPDF         IFEQ      'Y'
     C                   MOVE      'P'           Q2RTCD
     C                   ENDIF
     C*
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       Q2FLR            80
     C                   PARM      *BLANKS       Q2DCID           12
     C                   PARM      'EN'          Q2PRE             2
     C                   PARM      QQCO#         Q2CO#             3
     C                   PARM      QQFND         Q2FND             3
     C                   PARM                    Q2RTCD            1
     C                   ENDIF
     C*
     C*  BUILD ANY REQUIRED WORK FIELDS
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DtMDYYold
     C                   MOVEL     SRDTO         XXDAT8
     C*
     C                   Z-ADD     ENEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DtMDYYold
     C                   MOVEL     SRDTO         XXEED8
     C*
     C     ENMCT1        IFNE      *BLANKS
     C     ENMST1        ORNE      *BLANKS
     C     ENMZP1        ORNE      *ZEROS
     C                   MOVEL     ENMZP1        XXZIP5            5
     C                   MOVE      ENMZP1        XXZIP4            4
     C     ENMCT1        CAT(P)    ',':0         XXMCSZ
     C                   CAT       ENMST1:1      XXMCSZ
     C                   CAT       XXZIP5:1      XXMCSZ
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXMCSZ
     C                   CAT       XXZIP4:0      XXMCSZ
     C                   ENDIF
     C                   ENDIF
     C*
     C     ENPCT1        IFNE      *BLANKS
     C     ENPST1        ORNE      *BLANKS
     C     ENPZP1        ORNE      *ZEROS
     C                   MOVEL     ENPZP1        XXZIP5
     C                   MOVE      ENPZP1        XXZIP4
     C     ENPCT1        CAT(P)    ',':0         XXPCSZ
     C                   CAT       ENPST1:1      XXPCSZ
     C                   CAT       XXZIP5:1      XXPCSZ
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXPCSZ
     C                   CAT       XXZIP4:0      XXPCSZ
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C**************************************************************************
     C*  OTHINF - LOAD OTHER INFO TO WMENP1 RECORD
     C**************************************************************************
     C     OTHINF        BEGSR
      *
     C     ENPGM         IFEQ      'W0090R'
     C                   MOVE      *ON           XXPBCG
     C                   ENDIF
     C*
     C*   MOVE THE INPUT & WORK FIELDS TO THE OUTPUT RECORD HERE
     C*
     C                   MOVE      XXDAT8        ENPDT8
     C                   MOVE      XXEED8        ENEED8
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVEL     ENNAM1        ENNAME
     C                   MOVEL     ENFDN1        ENFDNM
     C                   MOVEL     ENENM1        ENEN1
     C                   MOVEL     ENENM2        ENEN2
     C                   MOVEL     ENEFN1        ENFD1
     C                   MOVEL     ENEFN2        ENFD2
     C                   MOVEL     ENCON1        ENCONT
     C                   MOVEL     ENATT1        ENATTN
     C                   MOVEL     ENCN31        ENCNT3
     C                   MOVE      ENFEN1        ENFEID
     C                   MOVEL     ENTOB1        ENTPOB
     C                   MOVEL     ENMA11        ENMAD1
     C                   MOVEL     ENMA21        ENMAD2
     C                   MOVEL     XXMCSZ        ENMAD3
     C*
     C     ENMA21        IFEQ      *BLANKS
     C                   MOVEL     XXMCSZ        ENMAD2
     C                   MOVEL     *BLANKS       ENMAD3
     C                   ENDIF
     C*
     C                   MOVEL     ENPA11        ENPAD1
     C                   MOVEL     ENPA21        ENPAD2
     C                   MOVEL     XXPCSZ        ENPAD3
     C     ENPA21        IFEQ      *BLANKS
     C                   MOVEL     XXPCSZ        ENPAD2
     C                   MOVEL     *BLANKS       ENPAD3
     C                   ENDIF
     C*
     C                   MOVEL     ENCNT1        ENCNTY
     C                   MOVE      ENOFI1        ENOFID
     C                   MOVEL     ENOFN1        ENOFNM
     C                   MOVEL     ENOEN1        ENOENM
     C*
     C                   MOVEL     ENOM11        ENOMA1
     C                   MOVEL     ENOM21        ENOMA2
     C*
     C                   MOVE      ENOMZ1        XXZIP4            4
     C     ENOMC1        CAT(P)    ',':0         ENOMA3
     C                   CAT       ENOMS1:1      ENOMA3
     C                   CAT       XXZIP5:1      ENOMA3
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         ENOMA3
     C                   CAT       XXZIP4:0      ENOMA3
     C                   ENDIF
     C*
     C     ENOMA2        IFEQ      *BLANKS
     C                   MOVEL     ENOMA3        ENOMA2
     C                   MOVEL     *BLANKS       ENOMA3
     C                   ENDIF
     C*
     C                   MOVEL     ENOP11        ENOPA1
     C                   MOVEL     ENOP21        ENOPA2
     C*
     C                   MOVE      ENOPZ1        XXZIP4            4
     C     ENOPC1        CAT(P)    ',':0         ENOPA3
     C                   CAT       ENOPS1:1      ENOPA3
     C                   CAT       XXZIP5:1      ENOPA3
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         ENOPA3
     C                   CAT       XXZIP4:0      ENOPA3
     C                   ENDIF
     C*
     C     ENOPA2        IFEQ      *BLANKS
     C                   MOVEL     ENOPA3        ENOPA2
     C                   MOVEL     *BLANKS       ENOPA3
     C                   ENDIF
     C*
     C                   MOVEL     ENOCN1        ENOCNT
     C                   MOVEL     ENOC11        ENOC1N
     C                   MOVEL     ENOC21        ENOC2N
     C                   MOVEL     ENOC31        ENOC3N
     C                   MOVEL     ENIC11        ENILCD
     C                   MOVEL     ENID11        ENILDS
     C                   MOVEL     ENIC21        ENINL2
     C                   MOVEL     ENID21        ENI2DS
     C                   MOVE      ENEM11        ENEXM1
     C                   MOVE      ENEM21        ENEXM2
     C                   MOVE      ENEM31        ENEXM3
     C                   MOVE      ENED11        ENEXD1
     C                   MOVE      ENED21        ENEXD2
     C                   MOVE      ENED31        ENEXD3
     C                   MOVEL     ENNOB1        ENNAOB
     C                   MOVEL     ENPOL1        ENPOLY
     C                   MOVE      ENCAR1        ENCARR
     C                   MOVE      ENPFD1        ENPFDT
     C                   MOVE      ENPTD1        ENPTDT
     C                   MOVEL     ENRSK1        ENRSK#
     C*** begin add ***  02/24/25  ***************************************
     C                   Movel     ENBefore      ENBefore
     C                   Movel     ENAfter       ENAfter
     C*** end add *****  02/24/25  ***************************************
     C*
     C*  EMPLOYER INFORMATION THAT PRINTS AT THE TOP OF THE ENDORSEMENT
     C*  FORM SHOULD BE INFO FOR THE STATE REPORTING DIVISION
     C*
     C                   MOVE      *BLANKS       ENNAME
     C                   MOVE      *BLANKS       ENFDNM
     C                   MOVE      *BLANKS       ENEN1
     C                   MOVE      *BLANKS       ENEN2
     C                   MOVE      *BLANKS       ENFD1
     C                   MOVE      *BLANKS       ENFD2
     C                   MOVE      *BLANKS       ENPAD1
     C                   MOVE      *BLANKS       ENPAD2
     C                   MOVE      *BLANKS       ENPAD3
     C                   MOVE      *BLANKS       ENMAD1
     C                   MOVE      *BLANKS       ENMAD2
     C                   MOVE      *BLANKS       ENMAD3
     C                   MOVE      *BLANKS       ENCPL#
     C                   Z-ADD     *ZEROS        ENPFD8
     C                   Z-ADD     *ZEROS        ENPTD8
     C*
     C     EMKY1         CHAIN     PEM01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   MOVEL     EMNAME        ENNAME
     C                   MOVEL     EMFDNM        ENFDNM
     C*
     C                   MOVEL     EMNAME        ENEN1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENEN1
     C                   MOVE      EMNAM2        ENEN2
     C*
     C                   MOVEL     EMFDNM        ENFD1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENFD1
     C                   MOVE      EMFDN2        ENFD2
     C*
     C     EMMCTY        IFNE      *BLANKS
     C     EMMST         ORNE      *BLANKS
     C     EMMZIP        ORNE      *ZEROS
     C                   MOVEL     EMMZIP        XXZIP5            5
     C                   MOVE      EMMZIP        XXZIP4            4
     C     EMMCTY        CAT(P)    ',':0         XXMCSZ
     C                   CAT       EMMST:1       XXMCSZ
     C                   CAT       XXZIP5:1      XXMCSZ
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXMCSZ
     C                   CAT       XXZIP4:0      XXMCSZ
     C                   ENDIF
     C                   ENDIF
     C*
     C     EMPCTY        IFNE      *BLANKS
     C     EMPST         ORNE      *BLANKS
     C     EMPZIP        ORNE      *ZEROS
     C                   MOVEL     EMPZIP        XXZIP5
     C                   MOVE      EMPZIP        XXZIP4
     C     EMPCTY        CAT(P)    ',':0         XXPCSZ
     C                   CAT       EMPST:1       XXPCSZ
     C                   CAT       XXZIP5:1      XXPCSZ
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXPCSZ
     C                   CAT       XXZIP4:0      XXPCSZ
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     EMMAD1        ENMAD1
     C                   MOVEL     EMMAD2        ENMAD2
     C                   MOVEL     XXMCSZ        ENMAD3
     C*
     C     EMMAD2        IFEQ      *BLANKS
     C                   MOVEL     XXMCSZ        ENMAD2
     C                   MOVEL     *BLANKS       ENMAD3
     C                   ENDIF
     C*
     C                   MOVEL     EMPAD1        ENPAD1
     C                   MOVEL     EMPAD2        ENPAD2
     C                   MOVEL     XXPCSZ        ENPAD3
     C     EMPAD2        IFEQ      *BLANKS
     C                   MOVEL     XXPCSZ        ENPAD2
     C                   MOVEL     *BLANKS       ENPAD3
     C                   ENDIF
     C*
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     RPCAP#        ENCPL#
     C                   END
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DtMDYYold
     C                   MOVEL     SRDTO         ENPFD8
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DtMDYYold
     C                   MOVEL     SRDTO         ENPTD8
     C                   END
     C*
     C                   LEAVE
     C                   END
     C*
     C     EMKY1         READE     PEM01                                  50
     C                   ENDDO
     C*
     C                   MOVE      *ZEROS        ENAGT#
     C                   MOVE      *BLANKS       ENAGNM
     C*
     C*  GET AGENT NAME VIA AGENT HISTORY AND AGENT MASTER FILES
     C*
     C     AHKY          SETGT     PAH01
     C                   READP     PAH01                                  50
     C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  50
     C                   END
     C*
     C     *IN50         IFEQ      '0'
     C     ENCO#         ANDEQ     AHCO#
     C     ENFND         ANDEQ     AHFND
     C     ENEMP#        ANDEQ     AHEMP#
     C     ENDIV         ANDEQ     AHDIV
     C                   MOVEL     AHAGT#        ENAGT#
     C*
     C     AAKY          CHAIN     PAA01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     AAFEIN        XXFEIN
     C                   MOVEL     AAMOD         XXMOD
     C                   ELSE
     C                   MOVE      *ZEROS        XXFEIN
     C                   MOVE      *ZEROS        XXMOD
     C                   END
     C*
     C     AGKY          CHAIN     PAG01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     AGNAME        ENAGNM
     C                   END
     C                   END
      *
      * Get PEO Information if PEO Group
      *
     C     FNPEOF        IFNE      *BLANK
      *
     C                   CALL      'SRADD1'
     C                   PARM      XXCO#         #5CO#             3 0
     C                   PARM      XXFND         #5FND             3 0
     C                   PARM      XXEMP#        #5EMP#            9 0
     C                   PARM      XXDIV         #5DIV             5 0
     C                   PARM      XXFYR         #5FYR             3 0
     C                   PARM                    #5NAME          100
     C                   PARM                    #5FNAM          100
     C     ENMAD1        PARM                    #5EAD1           40
     C     ENMAD2        PARM                    #5EAD2           40
     C     ENMAD3        PARM                    #5ECSZ           40
     C                   PARM                    #5CTY            25
     C                   PARM                    #5ST              2
     C                   PARM                    #5ZIP             9 0
     C     ENFEID        PARM                    #5FEIN            9 0
     C                   PARM                    #5EADR           90
      *
     C                   MOVEL     #5NAME        ENEN1
     C                   MOVE      #5NAME        ENEN2            50
     C                   MOVEL     #5FNAM        ENFD1
     C                   MOVE      #5FNAM        ENFD2            50
     C                   ENDIF
      *
     C     ENMAD2        IFEQ      *BLANK
     C                   MOVEL     ENMAD3        ENMAD2
     C                   MOVEL     *BLANKS       ENMAD3
     C                   ENDIF
      *
      * Get correct description for Entity/Location processing
      *
     C     ENPGM         IFEQ      'W0076R02'
     C     LDMYRR        CHAIN     PMY01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C                   ENDSR
      *
      *
     C**************************************************************************
     C*  DARTS  - WRITE A DARTS RECORD
     C**************************************************************************
     C     DARTS         BEGSR
     C*
     C     FIRSTD        IFEQ      *ON
     C                   MOVE      *OFF          FIRSTD
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXEMP#        QCEMP#
     C                   MOVE      XXDIV         QCDIV
     C*
     C*  ESTABLISH RECORD
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   EXSR      SRBCMT
     C                   MOVEL(P)  XXCOMM        Q#COMM          256
     C                   MOVE      XXCOMM        Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  Q2DCID        Q#DCID           12
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
      *
     C                   SELECT
     C     ENADDF        WHENEQ    'Y'
     C     SVIUSR        ANDNE     *BLANKS
     C                   MOVEL(P)  SVIUSR        Q#USER
     C                   OTHER
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   ENDSL
      *
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  Q2FLR         Q#FOLD           80
     C     ENRMCD        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE(P)   CTFL01        Q#DIFL            1
     C                   ENDIF
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVEL(P)  ENRMCD        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  'WC'          Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANKS       Q#IMGF            1
     C     XXPDF         IFEQ      'Y'
     C                   MOVEL(P)  'O'           Q#IMGF
     C                   ENDIF
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANKS       Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C                   ENDIF
     C*
     C                   ENDSR
      *
     C*====================================================================
     C* Print HCN Program endorsement
     C*====================================================================
     C     PRTHCN        BEGSR
     C*
     C     *LIKE         DEFINE    *IN99         SVIN99
     C     *LIKE         DEFINE    QQDCID        ##DCID
     C*
     C     ENKY0         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFORM
     C*
     C                   MOVE      *IN99         SVIN99
     C*
     C                   MOVEL     'WC420408'    XXFORM
     C*
     C* Print document to AMTEMP
     C                   MOVEL(P)  Q2FLR         #FLR81           81
     C                   MOVE      '*'           #FLR81
     C                   MOVEL     Q1JOBN        #AJOBN            6
     C                   CALL      'SA039R'
     C                   PARM      'AMTEMP'      ##FLR            80
     C     ##FLR         CAT(P)    '/':0         #TFL81           81
     C                   CAT       #AJOBN:0      #TFL81
     C                   MOVE      '*'           #TFL81
     C*
     C                   CALL      'W7051C2'                            99
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQFYR
     C                   PARM      XXFORM        ##FORM
     C                   PARM                    QQOUTQ
     C                   PARM      Q2DCID        ##DCID
     C                   PARM                    #FLR81
     C                   PARM                    #TFL81
     C                   PARM                    QQOutPutType
     C*
     C                   MOVE      SVIN99        *IN99
     C*
     C                   ENDSR
      *
     C*****************************************************************
     C* SRBCMT SUBROUTINE              ** SUBROUTINE **   S R B C M T *
     C*****************************************************************
     C     SRBCMT        BEGSR
     C*
     C                   MOVEL     *BLANKS       XXCOMM
     C*
     C                   SELECT
     C*----------------------------------------------------------------
     C*  COMMENT FOR NAME CHANGE
     C*----------------------------------------------------------------
     C     ENNCFL        WHENEQ    'Y'
     C     XXCM01        CAT       ';':0         XXCOMM
     C                   CAT       XXCM02:1      XXCOMM
     C                   CAT       ENFDN1:1      XXCOMM
     C     ENNAM1        IFNE      *BLANKS
     C                   CAT       '/':0         XXCOMM
     C                   CAT       ENNAM1:0      XXCOMM
     C                   ENDIF
     C                   CAT       ';':0         XXCOMM
     C                   CAT       XXCM03:1      XXCOMM
     C                   CAT       ENNAM2:1      XXCOMM
     C     ENFDN2        IFNE      *BLANKS
     C                   CAT       '/':0         XXCOMM
     C                   CAT       ENFDN2:0      XXCOMM
     C                   ENDIF
     C*----------------------------------------------------------------
     C*  COMMENT FOR FEDERAL ID CHANGE
     C*----------------------------------------------------------------
     C     ENFIFL        WHENEQ    'Y'
     C     XXCM04        CAT       ';':0         XXCOMM
     C                   CAT       XXCM05:1      XXCOMM
     C                   MOVE      ENFEN1        XXWK9A
     C                   CAT       XXPR1:1       XXCOMM
     C                   CAT       '-':0         XXCOMM
     C                   CAT       XXSF1:0       XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       XXCM06:1      XXCOMM
     C                   MOVE      ENFEN2        XXWK9A
     C                   CAT       XXPR1:1       XXCOMM
     C                   CAT       '-':0         XXCOMM
     C                   CAT       XXSF1:0       XXCOMM
     C*----------------------------------------------------------------
     C*  COMMENT FOR MISC NAME CHANGE
     C*----------------------------------------------------------------
     C     ENCNFL        WHENEQ    'Y'
     C     'Misc'        CAT       XXCM01:1      XXCOMM
     C                   CAT       '; OLD:':0    XXCOMM
     C                   CAT       ENCON1:1      XXCOMM
     C                   CAT       'NEW:':1      XXCOMM
     C                   CAT       ENCON2:1      XXCOMM
     C*
     C     ENATFL        WHENEQ    'Y'
     C     'Misc'        CAT       XXCM01:1      XXCOMM
     C                   CAT       '; OLD:':0    XXCOMM
     C                   CAT       ENATT1:1      XXCOMM
     C                   CAT       'NEW:':1      XXCOMM
     C                   CAT       ENATT2:1      XXCOMM
     C*
     C     ENC3FL        WHENEQ    'Y'
     C     'Misc'        CAT       XXCM01:1      XXCOMM
     C                   CAT       '; OLD:':0    XXCOMM
     C                   CAT       ENCN31:1      XXCOMM
     C                   CAT       'NEW:':1      XXCOMM
     C                   CAT       ENCN32:1      XXCOMM
     C*
     C*----------------------------------------------------------------
     C*  TYPE OF BUSINESS CHANGE
     C*----------------------------------------------------------------
     C     ENTBFL        WHENEQ    'Y'
     C     XXCM07        CAT       ':':0         XXCOMM
     C                   CAT       'OLD:':1      XXCOMM
     C                   CAT       ENTOB1:1      XXCOMM
     C                   CAT       'NEW:':1      XXCOMM
     C                   CAT       ENTOB2:1      XXCOMM
     C*
     C*----------------------------------------------------------------
     C*  Comment for Addition of Owner/Officer
     C*----------------------------------------------------------------
     C     ENNOFL        WHENEQ    'A'
     C                   MOVE      *ON           XXOOCG
     C                   CAT       XXCM08:0      XXCOMM
      *----------------------------------------------------------------
      *  Comment for Deletion of Owner/Officer
      *----------------------------------------------------------------
     C     ENNOFL        WHENEQ    'D'
     C                   MOVE      *ON           XXOOCG
     C                   CAT       XXCM41:0      XXCOMM
     C*----------------------------------------------------------------
     C*  Comment for Nominal Change
     C*----------------------------------------------------------------
     C     ENNOFL        WHENEQ    'C'
     C                   MOVE      *ON           XXOOCG
     C                   MOVEL     XXCM10        XXCOMM
     C                   CAT       'OLD:':1      XXCOMM
     C                   CAT       ENBLIN:1      XXCOMM
     C                   CAT       ',':0         XXCOMM
     C                   CAT       'NEW:':1      XXCOMM
     C     ENALIN        IFNE      *BLANK
     C                   CAT       ENALIN:1      XXCOMM
     C                   ELSE
     C                   CAT       'Delete':1    XXCOMM
     C                   CAT       'd':0         XXCOMM
     C                   ENDIF
     C*
     C*----------------------------------------------------------------
     C*  COMMENTS FOR ADDRESS CHANGES
     C*----------------------------------------------------------------
     C     ENADFL        WHENEQ    'Y'
     C*
     C                   SELECT
     C*  COMMENT FOR BOTH ADDRESS CHANGE
     C     ENCBAF        WHENEQ    'Y'
     C                   MOVEL     XXCM11        XXCOMM
      *
     C     ENUMFL        IFEQ      'Y'
     C                   CAT       ';':0         XXCOMM
     C                   CAT       XXCM43:1      XXCOMM
     C                   ENDIF
      *
     C*  COMMENT FOR MAILING ADDRESS CHANGE
     C     ENMAFL        WHENEQ    'Y'
     C                   MOVEL     XXCM12        XXCOMM
      *
     C     ENUMFL        IFEQ      'Y'
     C                   CAT       ';':0         XXCOMM
     C                   CAT       XXCM43:1      XXCOMM
     C                   ENDIF
      *
     C*  COMMENT FOR PHYSICAL ADDRESS CHANGE
     C     ENPAFL        WHENEQ    'Y'
     C                   MOVEL     XXCM13        XXCOMM
     C                   ENDSL
     C*  COMMENT FOR COUNTY CHANGE
     C     ENCNTF        IFEQ      'Y'
     C*
     C     ENCBAF        IFEQ      'Y'
     C     ENMAFL        OREQ      'Y'
     C     ENPAFL        OREQ      'Y'
     C                   CAT       XXCM25:1      XXCOMM
     C                   ELSE
     C                   MOVEL     XXCM25        XXCOMM
     C                   ENDIF
     C*
     C                   CAT       ENCNT1:1      XXCOMM
     C                   CAT       ',':0         XXCOMM
     C                   CAT       XXCM26:1      XXCOMM
     C                   CAT       ENCNT2:1      XXCOMM
     C                   ENDIF
     C*----------------------------------------------------------------
     C*  COMMENT FOR DELETION OF LOCATION
     C*----------------------------------------------------------------
     C     ENDELF        WHENEQ    'Y'
     C     ENELTP        ANDEQ     'L'
     C                   MOVE      *ON           XXOWCG
     C                   MOVEL     ENELID        XXELID
     C     XXCM14        CAT       ';':0         XXCOMM
     C                   CAT       'Loc:':1      XXCOMM
     C                   CAT       XXELID:1      XXCOMM
     C                   CAT       XXCM15:1      XXCOMM
     C*
     C                   Z-ADD     ENELDD        SRDTI
     C                   CALL      'SRCVTDAT'    DtMDYYold
     C                   MOVEL     SRDTO         XXWKDT
     C*
     C                   CAT       XXWKMM:1      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKDD:0      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKCY:0      XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       XXCM16:1      XXCOMM
     C                   CAT       ENELDR:1      XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       XXCM09:1      XXCOMM
     C                   CAT       ENOEN1:1      XXCOMM
     C*----------------------------------------------------------------
     C*  COMMENT FOR DELETION OF ENTITY
     C*----------------------------------------------------------------
     C     ENDELF        WHENEQ    'Y'
     C     ENELTP        ANDNE     'L'
     C                   MOVE      *ON           XXOWCG
     C                   MOVEL     ENELID        XXELID
     C     XXCM17        CAT       ';':0         XXCOMM
     C                   CAT       'Ent:':1      XXCOMM
     C                   CAT       XXELID:1      XXCOMM
     C                   CAT       XXCM15:1      XXCOMM
     C*
     C                   Z-ADD     ENELDD        SRDTI
     C                   CALL      'SRCVTDAT'    DtMDYYold
     C                   MOVEL     SRDTO         XXWKDT
     C*
     C                   CAT       XXWKMM:1      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKDD:0      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKCY:0      XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       XXCM16:1      XXCOMM
     C                   CAT       ENELDR:1      XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       XXCM09:1      XXCOMM
     C                   CAT       ENOEN1:1      XXCOMM
     C*----------------------------------------------------------------
     C*  COMMENT FOR ADDITION OF LOCATION
     C*----------------------------------------------------------------
     C     ENAELF        WHENEQ    'Y'
     C     ENELTP        ANDEQ     'L'
     C                   MOVE      *ON           XXOWCG
     C     XXCM18        CAT       ENOEN1:1      XXCOMM
     C*----------------------------------------------------------------
     C*  COMMENT FOR ADDITION OF ENTITY
     C*----------------------------------------------------------------
     C     ENAELF        WHENEQ    'Y'
     C     ENELTP        ANDNE     'L'
     C                   MOVE      *ON           XXOWCG
     C     XXCM19        CAT       ENOEN1:1      XXCOMM
     C*----------------------------------------------------------------
     C*  COMMENT FOR CHANGE OF LOCATION
     C*----------------------------------------------------------------
     C     ENCELF        WHENEQ    'Y'
     C     ENELTP        ANDEQ     'L'
     C                   MOVE      *ON           XXOWCG
     C                   MOVEL     ENELID        XXELID
     C     XXCM21        CAT       'LOC:':1      XXCOMM
     C                   CAT       XXELID:1      XXCOMM
     C*
     C     ENOEN1        IFNE      ENOEN2
     C                   CAT       XXCM01:1      XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       'OLD:':1      XXCOMM
     C                   CAT       ENOEN1:1      XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       'NEW:':1      XXCOMM
     C                   CAT       ENOEN2:1      XXCOMM
     C                   ENDIF
     C*
     C     ENOFN1        IFNE      ENOFN2
     C                   CAT       'FED':1       XXCOMM
     C                   CAT       'NAME':1      XXCOMM
     C                   CAT       'CHANGE':1    XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       'OLD:':1      XXCOMM
     C                   CAT       ENOFN1:1      XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       'NEW:':1      XXCOMM
     C                   CAT       ENOFN2:1      XXCOMM
     C                   ENDIF
     C*
     C     ENOFI1        IFNE      ENOFI2
     C                   CAT       XXCM04:1      XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       XXCM05:1      XXCOMM
     C                   MOVE      ENOFI1        XXWK9A
     C                   CAT       XXPR1:1       XXCOMM
     C                   CAT       '-':0         XXCOMM
     C                   CAT       XXSF1:0       XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       XXCM06:1      XXCOMM
     C                   MOVE      ENOFI2        XXWK9A
     C                   CAT       XXPR1:1       XXCOMM
     C                   CAT       '-':0         XXCOMM
     C                   CAT       XXSF1:0       XXCOMM
     C                   ENDIF
     C*
     C     ENOM11        IFNE      ENOM12
     C     ENOM21        ORNE      ENOM22
     C     ENOMC1        ORNE      ENOMC2
     C     ENOMS1        ORNE      ENOMS2
     C     ENOMZ1        ORNE      ENOMZ2
     C                   CAT       XXCM12:1      XXCOMM
     C                   ENDIF
     C*
     C     ENOP11        IFNE      ENOP12
     C     ENOP21        ORNE      ENOP22
     C     ENOPC1        ORNE      ENOPC2
     C     ENOPS1        ORNE      ENOPS2
     C     ENOPZ1        ORNE      ENOPZ2
     C     ENOCN1        ORNE      ENOCN2
     C                   CAT       XXCM13:1      XXCOMM
     C                   ENDIF
     C*----------------------------------------------------------------
     C*  COMMENT FOR CHANGE OF ENTITY
     C*----------------------------------------------------------------
     C     ENCELF        WHENEQ    'Y'
     C     ENELTP        ANDNE     'L'
     C                   MOVE      *ON           XXOWCG
     C                   MOVEL     ENELID        XXELID
     C     XXCM20        CAT       'ENT:':1      XXCOMM
     C                   CAT       XXELID:1      XXCOMM
     C*
     C     ENOEN1        IFNE      ENOEN2
     C                   CAT       XXCM01:1      XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       'OLD:':1      XXCOMM
     C                   CAT       ENOEN1:1      XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       'NEW:':1      XXCOMM
     C                   CAT       ENOEN2:1      XXCOMM
     C                   ENDIF
     C*
     C     ENOFN1        IFNE      ENOFN2
     C                   CAT       'FED':1       XXCOMM
     C                   CAT       'NAME':1      XXCOMM
     C                   CAT       'CHANGE':1    XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       'OLD:':1      XXCOMM
     C                   CAT       ENOFN1:1      XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       'NEW:':1      XXCOMM
     C                   CAT       ENOFN2:1      XXCOMM
     C                   ENDIF
     C*
     C     ENOFI1        IFNE      ENOFI2
     C                   CAT       XXCM04:1      XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       XXCM05:1      XXCOMM
     C                   MOVE      ENOFI1        XXWK9A
     C                   CAT       XXPR1:1       XXCOMM
     C                   CAT       '-':0         XXCOMM
     C                   CAT       XXSF1:0       XXCOMM
     C                   CAT       ';':0         XXCOMM
     C                   CAT       XXCM06:1      XXCOMM
     C                   MOVE      ENOFI2        XXWK9A
     C                   CAT       XXPR1:1       XXCOMM
     C                   CAT       '-':0         XXCOMM
     C                   CAT       XXSF1:0       XXCOMM
     C                   ENDIF
     C*
     C     ENOM11        IFNE      ENOM12
     C     ENOM21        ORNE      ENOM22
     C     ENOMC1        ORNE      ENOMC2
     C     ENOMS1        ORNE      ENOMS2
     C     ENOMZ1        ORNE      ENOMZ2
     C                   CAT       XXCM12:1      XXCOMM
     C                   ENDIF
     C*
     C     ENOP11        IFNE      ENOP12
     C     ENOP21        ORNE      ENOP22
     C     ENOPC1        ORNE      ENOPC2
     C     ENOPS1        ORNE      ENOPS2
     C     ENOPZ1        ORNE      ENOPZ2
     C     ENOCN1        ORNE      ENOCN2
     C                   CAT       XXCM13:1      XXCOMM
     C                   ENDIF
     C*----------------------------------------------------------------
     C*  COMMENT FOR MISCELLANEOUS CHANGE
     C*----------------------------------------------------------------
     C     ENMSFL        WHENEQ    'Y'
     C                   CAT       ENDSC1:1      XXCOMM
     C     ENDSC2        IFNE      *BLANK
     C                   CAT       ENDSC2:1      XXCOMM
     C                   ENDIF
     C* HCN Program endorsement
     C     ENMSFL        WHENEQ    'H'
     C                   CAT       ENDSC1:1      XXCOMM
     C     ENDSC2        IFNE      *BLANK
     C                   CAT       ENDSC2:1      XXCOMM
     C                   ENDIF
     C                   EXSR      PRTHCN
     C*----------------------------------------------------------------
     C*  COMMENT FOR WAIVER OF SUBRO CHANGE
     C*----------------------------------------------------------------
     C     ENMSFL        WHENEQ    'S'
     C                   MOVE      *ON           XX313
      *
     C     XXEMST        IFEQ      'CA'
     C                   MOVE      *ON           X04306
     C                   ENDIF
      *
     C     ENOEN1        IFEQ      *BLANK
     C     ENOEN2        ANDNE     *BLANK
     C                   MOVEL     XXCM38        XXCOMM
     C                   MOVE      ENOFI2        XA05              5
     C                   CAT       XA05:1        XXCOMM
     C                   CAT       '-':1         XXCOMM
     C                   CAT       ENOEN2:1      XXCOMM
     C                   ENDIF
     C     ENOEN1        IFNE      *BLANK
     C     ENOEN2        ANDEQ     *BLANK
     C                   MOVEL     XXCM39        XXCOMM
     C                   MOVE      ENOFI1        XA05              5
     C                   CAT       XA05:1        XXCOMM
     C                   CAT       '-':1         XXCOMM
     C                   CAT       ENOEN1:1      XXCOMM
     C                   ENDIF
     C     ENOEN1        IFNE      *BLANK
     C     ENOEN2        ANDNE     *BLANK
     C                   MOVEL     XXCM40        XXCOMM
     C                   MOVE      ENOFI1        XA05              5
     C                   CAT       XA05:1        XXCOMM
     C                   CAT       '-':1         XXCOMM
     C                   CAT       ENOEN1:1      XXCOMM
     C                   ENDIF
     C*----------------------------------------------------------------
     C*  COMMENT FOR ADDITION OF A DIVISION
     C*----------------------------------------------------------------
     C     ENADDF        WHENEQ    'Y'
     C                   MOVE      *ON           XXOWCG
     C     XXCM23        CAT(P)    ':':0         XXCOMM
     C                   MOVE      ENDIV         XADIV             5
     C                   CAT       XADIV:1       XXCOMM
     C                   CAT       '-':1         XXCOMM
     C                   CAT       ENNAM1:1      XXCOMM
     C                   CAT       '/':1         XXCOMM
     C                   CAT       ENFDN1:1      XXCOMM
     C*----------------------------------------------------------------
     C*  COMMENT FOR DELETION OF A DIVISION
     C*----------------------------------------------------------------
     C     ENDLDF        WHENEQ    'Y'
     C                   MOVE      *ON           XXOWCG
     C     XXCM24        CAT(P)    ':':0         XXCOMM
     C                   MOVE      ENDIV         XADIV             5
     C                   CAT       XADIV:1       XXCOMM
     C                   CAT       '-':1         XXCOMM
     C                   CAT       ENNAM1:1      XXCOMM
     C                   CAT       '/':1         XXCOMM
     C                   CAT       ENFDN1:1      XXCOMM
      *
      *----------------------------------------------------------------
      *  COMMENT FOR RESCISSION DIVISION DELETION
      *----------------------------------------------------------------
     C     ENRDDF        WHENEQ    'Y'
     C                   MOVE      *ON           XXOWCG
     C     XXCM44        CAT(P)    ':':0         XXCOMM
     C                   MOVE      ENDIV         XADIV             5
     C                   CAT       XADIV:1       XXCOMM
     C                   CAT       '-':1         XXCOMM
     C                   CAT       ENNAM1:1      XXCOMM
     C                   CAT       '/':1         XXCOMM
     C                   CAT       ENFDN1:1      XXCOMM
      *
     C*----------------------------------------------------------------
     C*  COMMENT FOR INCREASED LIMITS CHANGE
     C*----------------------------------------------------------------
     C     ENILFL        WHENEQ    'Y'
     C                   MOVE      *ON           XXPBCG
     C     XXCM27        CAT(P)    XXCM28:1      XXCOMM
     C                   CAT       ENIC11:1      XXCOMM
     C                   CAT       '-':1         XXCOMM
     C                   CAT       ENID11:1      XXCOMM
     C     ENIC21        IFNE      *BLANK
     C                   CAT       ENIC21:1      XXCOMM
     C                   CAT       '-':1         XXCOMM
     C                   CAT       ENID21:1      XXCOMM
     C                   ENDIF
     C*
     C                   CAT(P)    XXCM29:1      XXCOMM
     C                   CAT       ENIC12:1      XXCOMM
     C                   CAT       '-':1         XXCOMM
     C                   CAT       ENID12:1      XXCOMM
     C     ENIC22        IFNE      *BLANK
     C                   CAT       ENIC22:1      XXCOMM
     C                   CAT       '-':1         XXCOMM
     C                   CAT       ENID22:1      XXCOMM
     C                   ENDIF
     C*----------------------------------------------------------------
     C*  COMMENT FOR EXPERIENCE MOD CHANGE
     C*----------------------------------------------------------------
     C     ENERFL        WHENEQ    'Y'
     C                   MOVE      *ON           XXPBCG
     C                   MOVEL     ENEM11        XXWK5A
     C     XXCM30        CAT(P)    XXEM1:1       XXCOMM
     C                   CAT       '.':0         XXCOMM
     C                   CAT       XXEM2:1       XXCOMM
     C                   CAT       '-':1         XXCOMM
     C                   MOVEL     ENED11        SRDTI
     C                   CALL      'SRCVTDAT'    DtMDYYold
     C                   MOVEL     SRDTO         XXWKDT
     C                   CAT       XXWKMM:1      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKDD:0      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKCY:0      XXCOMM
     C*
     C     ENEM21        IFNE      *ZERO
     C                   MOVEL     ENEM21        XXWK5A
     C                   CAT       '/':1         XXCOMM
     C                   CAT       XXEM1:1       XXCOMM
     C                   CAT       '.':0         XXCOMM
     C                   CAT       XXEM2:1       XXCOMM
     C                   MOVEL     ENED21        SRDTI
     C                   CALL      'SRCVTDAT'    DtMDYYold
     C                   MOVEL     SRDTO         XXWKDT
     C                   CAT       XXWKMM:1      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKDD:0      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKCY:0      XXCOMM
     C                   ENDIF
     C*
     C     ENEM31        IFNE      *ZERO
     C                   MOVEL     ENEM31        XXWK5A
     C                   CAT       '/':1         XXCOMM
     C                   CAT       XXEM1:1       XXCOMM
     C                   CAT       '.':0         XXCOMM
     C                   CAT       XXEM2:1       XXCOMM
     C                   MOVEL     ENED31        SRDTI
     C                   CALL      'SRCVTDAT'    DtMDYYold
     C                   MOVEL     SRDTO         XXWKDT
     C                   CAT       XXWKMM:1      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKDD:0      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKCY:0      XXCOMM
     C                   ENDIF
     C*
     C                   MOVEL     ENEM12        XXWK5A
     C     XXCM31        CAT       XXEM1:1       XXCOMM
     C                   CAT       '.':0         XXCOMM
     C                   CAT       XXEM2:1       XXCOMM
     C                   MOVEL     ENED12        SRDTI
     C                   CALL      'SRCVTDAT'    DtMDYYold
     C                   MOVEL     SRDTO         XXWKDT
     C                   CAT       XXWKMM:1      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKDD:0      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKCY:0      XXCOMM
     C*
     C     ENEM22        IFNE      *ZERO
     C                   MOVEL     ENEM22        XXWK5A
     C                   CAT       '/':1         XXCOMM
     C                   CAT       XXEM1:1       XXCOMM
     C                   CAT       '.':0         XXCOMM
     C                   CAT       XXEM2:1       XXCOMM
     C                   MOVEL     ENED22        SRDTI
     C                   CALL      'SRCVTDAT'    DtMDYYold
     C                   MOVEL     SRDTO         XXWKDT
     C                   CAT       XXWKMM:1      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKDD:0      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKCY:0      XXCOMM
     C                   ENDIF
     C*
     C     ENEM32        IFNE      *ZERO
     C                   MOVEL     ENEM32        XXWK5A
     C                   CAT       '/':1         XXCOMM
     C                   CAT       XXEM1:1       XXCOMM
     C                   CAT       '.':0         XXCOMM
     C                   CAT       XXEM2:1       XXCOMM
     C                   MOVEL     ENED32        SRDTI
     C                   CALL      'SRCVTDAT'    DtMDYYold
     C                   MOVEL     SRDTO         XXWKDT
     C                   CAT       XXWKMM:1      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKDD:0      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKCY:0      XXCOMM
     C                   ENDIF
     C*----------------------------------------------------------------
     C*  COMMENT FOR PAYROLL/CLASS CODE CHANGE
     C*----------------------------------------------------------------
     C     ENCRFL        WHENEQ    'Y'
     C                   MOVE      *ON           XXPBCG
     C                   MOVEL(P)  XXCM32        XXCOMM
     C*----------------------------------------------------------------
     C*  COMMENT FOR INTERIM ADJUSTMENT OF PREMIUM
     C*----------------------------------------------------------------
     C     ENIAPF        WHENEQ    'Y'
     C                   MOVE      *ON           XXPBCG
     C                   MOVEL(P)  XXCM33        XXCOMM
     C*----------------------------------------------------------------
     C*  COMMENT FOR NATURE OF BUSINESS CHANGE
     C*----------------------------------------------------------------
     C     ENNOBF        WHENEQ    'Y'
     C     XXCM34        CAT(P)    XXCM35:1      XXCOMM
     C                   CAT       'Old':1       XXCOMM
     C                   CAT       ENNOB1:1      XXCOMM
     C                   CAT       'New':1       XXCOMM
     C                   CAT       ENNOB2:1      XXCOMM
     C*----------------------------------------------------------------
     C*  COMMENT FOR RISK ID CHANGE
     C*----------------------------------------------------------------
     C     ENRIDF        WHENEQ    'Y'
     C     XXCM34        CAT(P)    XXCM36:1      XXCOMM
     C                   CAT       'Old':1       XXCOMM
     C                   CAT       ENRSK1:1      XXCOMM
     C                   CAT       'New':1       XXCOMM
     C                   CAT       ENRSK2:1      XXCOMM
     C*----------------------------------------------------------------
     C*  COMMENT FOR OTHER CERT OF INSURANCE CHANGE
     C*----------------------------------------------------------------
     C     ENCIFL        WHENEQ    'Y'
     C     ENAGT1        ANDEQ     ENAGT2
     C     'Other'       CAT(P)    XXCM34:1      XXCOMM
     C*----------------------------------------------------------------
     C*  COMMENT FOR CONTINGENT EXPERIENCE MOD CHANGE
     C*----------------------------------------------------------------
     C     ENCMFL        WHENEQ    'Y'
     C                   MOVEL     ENEM12        XXWK5A
     C     XXCM37        CAT(P)    XXEM1:1       XXCOMM
     C                   CAT       '.':0         XXCOMM
     C                   CAT       XXEM2:0       XXCOMM
     C                   CAT       '-':1         XXCOMM
     C                   MOVEL     ENED12        XXWKDT
     C                   CAT       XXWKMM:1      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKDD:0      XXCOMM
     C                   CAT       '/':0         XXCOMM
     C                   CAT       XXWKCY:0      XXCOMM
     C*----------------------------------------------------------------
     C* COMMENT FOR AGENT CHANGE
     C*----------------------------------------------------------------
     C     ENCIFL        WHENEQ    'Y'
     C     ENAGT1        ANDNE     ENAGT2
     C                   MOVEL     ENBLIN        XXBLIN           50
     C                   MOVEL     ENALIN        XXALIN           50
     C     XXCM42        CAT(P)    'Old:':1      XXCOMM
     C                   CAT       XXBLIN:1      XXCOMM
     C                   CAT       'New:':1      XXCOMM
     C                   CAT       XXALIN:1      XXCOMM
     C*----------------------------------------------------------------
     C* COMMENT FOR CLT139 MISC ENDORSE CHANGE
     C*----------------------------------------------------------------
     C     ENMISC        WHENNE    *BLANK
     C                   MOVEL(P)  XXCM22        XXCOMM
      *
     C*
     C                   ENDSL
     C*
     C                   CAT       ';':0         XXCOMM
     C                   CAT       XXCM00:1      XXCOMM
     C                   CAT       ENRUSR:1      XXCOMM
     C                   ENDSR
     C**************************************************************************
