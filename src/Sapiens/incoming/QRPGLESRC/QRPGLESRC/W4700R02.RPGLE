     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4700R02 - AUDIT TRACKING INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*  CALLED FROM: W4700R                                          *
     H*                                                               *
     H*  CALLS TO: WA002R - AGTAPI                                    *
     H*            SC014R - ADDITIONAL DARTS REMARKS                  *
     H*            W2290R - AUDIT COMPANY SEARCH                      *
     H*            S0005R - AUDIT REP SEARCH                          *
     H*                                                               *
     H*  CREATED: 03/04/98  PROGRAMMER: C:87  PROJECT: WC630          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/03/11 C:136 SUPPORT: WA045 75712                           *
     H*                CORRECT USER NAME WHEN DARTS REMARK IS         *
     H*                WRITTEN IN UPDATE MODE (DRT2)                  *
     H*                                                               *
     H* 08/18/10 C:257 SUPPORT SEQ73904                               *
     H*                Do not pass D1 for all @@MODE other than 'C'  *
     H*                                                               *
     H* 04/06/10 C:61  SUPPORT WA133 71431                            *
     H*                Do not write DARTS remark with remark code     *
     H*                that has value of blanks.                      *
     H*                                                               *
     H* 01/08/10 C:234 Seq#: 71262  WA099                             *
     H*                Decimal data error when subset to "outstanding *
     H*                only" audits are edited(opt 2)                 *
     H*                                                               *
     H* 03/23/09 C:220 Seq#: 63608                                    *
     H*                 Add record when audit company or audit type   *
     H*                 field updated.                                *
     H*                                                               *
     H* 01/21/09 C:220 Project: WC1664                                *
     H*                AUDIT TRACKING - TRACKING FROM EXPIRATION AUDIT*
     H*                 TO CANCELLATION AUDIT                         *
     H*                                                               *
     H* 02/11/08 C:33  Seq#: WA133/62173                              *
     H*                Remove 'FUND' from DARTS comment.              *
     H*                                                               *
     H*  07/16/07  C:190 Project: 93WGX
     H*                  Add prompt for 'Ticket Number' for KRMS(Client # 93)
     H*                                                               *
     H*  07/12/07 C:33 Project: WC1196                                *
     H*                Clear 'Mass Audit Request' flag when 'Audit    *
     H*                 Tracking' record deleted.                     *
     H*                                                               *
     H*  11/13/06 C:132 Project: 71WHM                                *
     H*                Added fields: SCACDT: Audit Calculation Date & *
     H*                SCAPDT: Audit Print Date.                      *
     H*                Change SCPRDT to be "Final Audit Print Date"   *
     H*                Add corresponding fields to WMAUP file.        *
     H*                                                               *
     H*  03/06/06 C:132 Support: WC071 51160                          *
     H*                Change Code to load DSDTTM and use DSDATE      *
     H*                instead of *DATE anywhere in the program.      *
     H*
     H* 03/23/05 C:53  WC1196
     H*                ADD TYPE OF AUDIT
     H*                                                               *
     H* 02/03/04 C:132 SUPPORT WA099 41056                            *
     H*                REMOVED: Chgs 09/19/03 to clear screen fields, *
     H*                This wiped Top lines on screen and cleared the *
     H*                Display indicator *IN09. The 09/19/03 code to  *
     H*                set on LR was not changed.                     *
     H*                                                               *
     H* 09/19/03 C:61  SUPPORT WA121 38813                            *
     H*                Clear screen field values before display and   *
     H*                close files on exit.                           *
     H*                                                               *
     H* 07/01/02 C:61  PROJECT 91WGS WC1086                           *
     H*                When called in delete mode, attach DARTS       *
     H*                remark to employer indicating the Audit        *
     H*                Tracking record was deleted.                   *
     H*                                                               *
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H*  09/23/99  C:33  SEQ#: WA033/19543                            *
     H*                  RELEASE WDELP RECORD IF NOT USING.  ALSO,    *
     H*                  UPDATE WDELP MAINTENANCE FIELDS.             *
     H*                                                               *
     H*  08/25/99  C:87  PROJECT: SUPP DR048 #19350                   *
     H*                  The time and initial pgm were not being      *
     H*                  assigned for DARTS write.                    *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H*  06/07/99  C:87  PROJECT: WC577                               *
     H*                  F10=Search was not appearing for option 21.  *
     H*                  Added code to correct.                       *
     H*                                                               *
     H*  04/23/99  C:118 PROJECT: WC630                               *
     H*                  Conditioned OTHFKS *INKJ so F10=Search is    *
     H*                  not allowed in DISPLAY mode. Changed display *
     H*                  file so F5=Refresh and F10=Search are not    *
     H*                  displayed in display mode.                   *
     H*                                                               *
     H*  03/02/99  C:87  PROJECT: SUPP DR081 #16756                   *
     H*                  Audit was shown as not contested if contested*
     H*                  date was cleared through option 2=Change yet *
     H*                  the contested flag in the file = 'Y'.  Chgd  *
     H*                  code to show as contested if flag = 'Y'      *
     H*                  regardless of contested date field.  Also,   *
     H*                  changed so that contested date and flag must *
     H*                  correspond (display error msg if they don't) *
     H*                                                               *
     H*  11/05/98  C:87  PROJECT: SUPP WA048 #14820                   *
     H*                  For options 21-24, if a date already exists  *
     H*                  in the appropriate field and is not changed  *
     H*                  before pressing enter, don't write to DARTS. *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  20-26 - OPTIONS PASSED FROM W4700R                           *
     H*  60 - GENERAL CHAIN                                           *
     H*  66 - IF *ON THEN CANX DATE EXISTS                            *
     H*  67 - DART remark for Aud Trk Record deletion                 *
     H*  85 - AUDIT COMPANY CHAIN                                     *
     H*  89 - INDICATES THAT A DARTS REMARK SHOULD BE WRITTEN         *
     H*                                                               *
     H*****************************************************************
     H*
     FW4700F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMAUP     UF A E             DISK
     F                                     RENAME(PAU01:DBRCDF)
     F*
     F*  Employee master physical file
     FWMEML     IF   E           K DISK
     F*
     F*  Employee detail logical files
     FWDELL     UF   E           K DISK
     F*
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     F*
     F*  Fund master logical file
     FWMFNL     IF   E           K DISK
     F*
     F*  User parameter logical and physical file
     FSMUPL1    IF   E           K DISK
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP02)
     F*** BEG ADD *** 01/21/09 **************************************
     FSMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP03)
     F*** END ADD *** 01/21/09 **************************************
     F*
     F*  Audit tracking DARTS file
     FWMAXL     IF   E           K DISK
     F*
     FWTACL     IF   E           K DISK
     FWTACP     IF   E             DISK
     F                                     RENAME(PAC01:PAC02)
     F*** BEGIN DEL *** 04/26/00 **************************************
     F********SDCML   O   E           K        DISK
     F********SDCMLA  IF  E           K        DISK
     F********    PCM01                             KRENAMEPCM02
     F*** END DEL *** 04/26/00 ****************************************
     FSTCTL1    IF   E           K DISK
      ***** BEG ADD ******* 07/01/02 **********************************
      *
      *  Admin/DARTS control
     FWMEXL     IF   E           K DISK
      ***** END ADD ******* 07/01/02 **********************************
     F*** BEG ADD *** 03/23/05 *****************************
     FWTTAL     IF   E           K DISK
     FWTTAP     IF   E             DISK
     F                                     RENAME(PTA01:PTA02)
     F*** END ADD *** 03/23/05 *****************************
     F*** BEG ADD *** 03/23/09 *****************************
     FWMHAP     O    E           K DISK
     FWTAAL1    IF   E           K DISK
     FWDELL8    IF   E           K DISK
     F                                     RENAME(PEL01:PEL011)
     F*** END ADD *** 03/23/09 *****************************
      *
      *
      *
      /COPY SCOPYMT01D
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
      ***zzz
      ***** BEG ADD ******* 07/01/02 **********************************
     D D1              C                   CONST('deleted Audit Trk r-
     D                                     ecord.')
      ***** BEG ADD ******* 01/21/09 **********************************
     D D11             C                   CONST('updated Audit Trk r-
     D                                     ecord.')
      ***** END ADD ******* 01/21/09 **********************************
     D D2              C                   CONST('Audit Company:')
     D D3              C                   CONST('Audit Rep:')
     D D4              C                   CONST('Audit Req Date:')
      ***** END ADD ******* 07/01/02 **********************************
     D OPTNM           C                   CONST('Audit Tracking')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D #OFSCR          C                   CONST(1)
     D*
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)                               Effective dates
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D*
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               Canx dates
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0

     I*
      *** BEG ADD ***** 02/03/11 ***************************************
     IPUP03
     I              UPUSNM                      U3USNM
      *** END ADD ***** 02/03/11 ***************************************
     I*
     I***PY SCOPYMT01I
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@OPT             2            Option number
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM      AUCO#         ##CO#
     C                   PARM      AUFND         ##FND
     C                   PARM      AUEMP#        ##EMP#            9 0
     C                   PARM      AUDIV         ##DIV             5 0
     C                   PARM      AUFYR         ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPDC#        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*
     C*  Additional DARTS remark program parameter list
     C*
     C     RMKPL         PLIST
     C                   PARM                    QCKEY
     C*** BEGIN CHG *** 04/26/00 **************************************
     C********             PARM           QCSEQ#
     C                   PARM                    QCSEQ#            5 0
     C********             PARM CMPNAM    QCPNAM
     C                   PARM                    QCPNAM           10
     C*** END CHG *** 04/26/00 ****************************************
     C*
     C*  Compress emp#, div, and emp name program parm list
     C*
     C     EDNPL         PLIST
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##NAME
     C                   PARM                    ##EDN            80
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
      ***** BEG ADD ******* 07/01/02 **********************************
      *
      *  Admin/DARTS Control
      *
     C     EXKY          KLIST
     C                   KFLD                    AUCO#
     C                   KFLD                    AUFND
      *
      ***** END ADD ******* 07/01/02 **********************************
     C*
     C*  Employer master key list
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*  Employer detail key list
     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C*  Employer detail key list to count # of divisions
     C     ELKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C*
     C*
     C***  Begin Add  ***  07/12/07  **************************************
     C     ELKYAU        KLIST
     C                   KFLD                    AUCO#
     C                   KFLD                    AUFND
     C                   KFLD                    AUEMP#
     C                   KFLD                    AUDIV
     C                   KFLD                    AUFYR
     C***  End   Add  ***  07/12/07  **************************************
     C*  Fund key list
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  User parameter key list
     C     UPKY          KLIST
     C                   KFLD                    KKSTAF
     C*
     C*  Audit company key list
     C     ACKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C*  Remark type table file
     C     CTKY          KLIST
     C                   KFLD                    QQRMCD            5
     C*
     C*  Adt trk/DARTS file
     C     AXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*** BEG ADD *** 03/23/05 *****************************
     C     TAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTYPA
     C*** END ADD *** 03/23/05 *****************************
     C*** BEG ADD *** 03/23/09 *****************************
     C     KYAAST        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTYPA
     C                   KFLD                    KKST              2
     C                   KFLD                    KKCNTY            2
     C                   KFLD                    KKPRML            9 0
     C     KYAA          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTYPA
     C                   KFLD                    KKST
     C                   KFLD                    KKCNTY
     C*
     C     KYEL          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C*
     C*** END ADD *** 03/23/09 *****************************
     C***  BEG   ADD  ***  01/21/09  **********************************
     C     Q1USER        CHAIN     PUP03                              60
     C***  END   ADD  ***  01/21/09  **********************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    AUCO#         KKCO#
     C     *LIKE         DEFINE    AUFND         KKFND
     C     *LIKE         DEFINE    AUEMP#        KKEMP#
     C     *LIKE         DEFINE    AUDIV         KKDIV
     C     *LIKE         DEFINE    AUFYR         KKFYR
     C     *LIKE         DEFINE    AUREP         KKSTAF
     C     *LIKE         DEFINE    AURCVD        X2RCVD
     C     *LIKE         DEFINE    AURQDT        X2RQDT
     C     *LIKE         DEFINE    AUPRDT        X2PRDT
     C     *LIKE         DEFINE    ACKEY         KKKEY
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********   *LIKE     DEFN CMSEQ#    QCSEQ#
     C********   *LIKE     DEFN CMPNAM    QCPNAM
     C*** BEGIN ADD *** 04/26/00 **************************************
     C     *LIKE         DEFINE    EMNAME        ##NAME
     C***  BEGIN ADD  ***  09/23/99  *********************************
     C     *LIKE         DEFINE    ELDT          SYSDAT
     C***  END   ADD  ***  09/23/99  *********************************
     C*** BEG ADD *** 03/23/05 *****************************
     C     *LIKE         DEFINE    TATYPA        KKTYPA
     C     *LIKE         DEFINE    SCCXDT        ##CXDT
     C     *LIKE         DEFINE    SCPPFR        ##PPFR
     C     *LIKE         DEFINE    SCPPTO        ##PPTO
     C     *LIKE         DEFINE    SC#DIV        ###DIV
     C     *LIKE         DEFINE    SCAGTA        ##AGTA
     C     *LIKE         DEFINE    SCPDC#        ##PDC#
     C     *LIKE         DEFINE    SCPDCA        ##PDCA
     C*** END ADD *** 03/23/05 *****************************
     C*** BEG ADD *** 03/23/09 *****************************
     C     *LIKE         DEFINE    SCACO         XXACO
     C*** END ADD *** 03/23/09 *****************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C*  DARTS SUBROUTINE  -  CREATE DARTS RECORD
     C*================================================================
     C     DARTS         BEGSR
     C*
     C*** BEGIN ADD *** 04/26/00 **************************************
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVE      *BLANK        QCKEY
     C                   MOVE      AUCO#         QCCO#
     C                   MOVE      AUFND         QCFND
     C                   MOVE      AUEMP#        QCEMP#
     C                   MOVE      AUDIV         QCDIV
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVEL(P)  QQRMCD        Q#RMCD            5
     C                   MOVE      AUFYR         XAFYR             2
      ***** BEG CHG ******* 07/01/02 **********************************
     C*********            EXSR DRTCOM
     C  N67              EXSR      DRTCOM
      ***** END CHG ******* 07/01/02 **********************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  *BLANK        Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
      ***** BEG ADD ******* 04/06/10 **********************************
      *
     C     Q#RMCD        IFNE      *BLANKS
      ***** END ADD ******* 04/06/10 **********************************
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
     C                   MOVE      Q#SEQ#        QCSEQ#
     C                   MOVE      Q#PNAM        QCPNAM
     C*** END ADD *** 04/26/00 ****************************************
     C*
      ***** BEG CHG ******* 07/01/02 **********************************
     C*********            CALL 'SC014R'  RMKPL
     C  N67              CALL      'SC014R'      RMKPL
      ***** END CHG ******* 07/01/02 **********************************
      ***** BEG ADD ******* 04/06/10 **********************************
     C                   ENDIF
      *
      ***** END ADD ******* 04/06/10 **********************************
     C*
     C                   ENDSR                                                  DARTS
      *
      *
      *
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
      ***** BEG ADD ******* 07/01/02 **********************************
      *
      *  Attach DARTS remark to employer on deletion of record
      *  IF indicated in DARTS control record.
      *
     C     EXKY          CHAIN     PEX01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     EXAUDL        ANDNE     *BLANKS
     C                   EXSR      DRT2
     C                   ENDIF
      *
      ***** END ADD ******* 07/01/02 **********************************
     C***  Begin Add  ***  07/12/07  **************************************
     C     ELKYAU        CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   CLEAR                   ELMRFL
     C                   EXSR      MNTFLD
     C                   UPDATE    PEL01
     C                   ENDIF
     C***  End   Add  ***  07/12/07  **************************************
     C                   ENDSR                                                  DLTPRE
      ***** BEG ADD ******* 07/01/02 **********************************
      *
      *
      *
      *================================================================
      * DRT2 - Write DARTS remark on deletion of emp Audit Trk record
      *================================================================
     C     DRT2          BEGSR
      *
     C                   MOVEL(P)  EXAUDL        QQRMCD
      *
      *  User Name
      *
      ***** BEG CHG ******* 01/21/09 **********************************
     C*************        MOVELQ1USER    Q#COMM256 P
      *** BEG CHG ***** 02/03/11 **************************************
     C***********          MOVELUPUSNM    Q#COMM256 P
     C                   MOVEL(P)  U3USNM        Q#COMM          256
      *** END CHG ***** 02/03/11 **************************************
      ***** BEG CHG ******* 08/18/10 **********************************
     C*********  @@MODE    IFEQ 'C'
     C*********  @@OPT     ANDEQ'2 '
     C*********            CAT  D11:1     Q#COMM    P
     C*********            ELSE
      ***** END CHG ******* 01/21/09 **********************************
     C*********            CAT  D1:1      Q#COMM    P
      ***** BEG ADD ******* 01/21/09 **********************************
     C*********            ENDIF
      ***** END ADD ******* 01/21/09 **********************************
     C                   SELECT
     C     @@MODE        WHENEQ    'C'
     C                   CAT(P)    D11:1         Q#COMM
     C     @@MODE        WHENEQ    'E'
     C                   CAT(P)    D1:1          Q#COMM
     C                   ENDSL
     C*
      ***** END CHG ******* 08/18/10 **********************************
      *
      *  Audit company name
      *
     C     AUACO         IFNE      *BLANKS
     C                   Z-ADD     AUCO#         KKCO#
     C                   MOVE      AUACO         KKKEY
     C     ACKY          CHAIN     PAC01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     ACNAME        ANDNE     *BLANKS
     C                   CAT(P)    D2:2          Q#COMM
     C                   CAT(P)    ACNAME:1      Q#COMM
     C                   ENDIF
      *
     C                   ENDIF                                                  AUACO
      *
      *  Audit Representative Name
      *
     C     AUREP         IFNE      *BLANKS
     C                   MOVE      AUREP         KKSTAF
     C     UPKY          CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   CAT(P)    D3:2          Q#COMM
     C                   CAT(P)    UPUSNM:1      Q#COMM
     C                   ENDIF
      *
     C                   ENDIF                                                  AUREP
      *
      *  Audit Request Date
      *
     C     AURQDT        IFNE      *ZERO
     C                   Z-ADD     AURQDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
      *
     C     SRERR         IFEQ      *OFF
     C                   CAT(P)    D4:2          Q#COMM
     C                   MOVEL(P)  SRDTO         ##NUM
      *
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      'DATE'        ##MASK           10
      *
     C                   CAT(P)    ##NUM:1       Q#COMM
     C                   ENDIF                                                  SRERR
      *
     C                   ENDIF                                                  AURQDT
      *
      * zzz
     C                   MOVE(P)   *BLANKS       Q#COM2           19
     C                   SETON                                        67
     C                   TIME                    DSDTTM
     C                   EXSR      DARTS
     C                   SETOFF                                       67
      *
     C                   ENDSR                                                  DRT2
      *
      ***** END ADD ******* 07/01/02 **********************************
      *
      *
      *
     C*================================================================
     C* DRTCOM - Fill in comment for DARTS
     C*================================================================
     C     DRTCOM        BEGSR
     C*
     C     2             SUBST     XXDTE:1       XXMDTE            2
     C     2             SUBST     XXDTE:3       XXDDTE            2
     C     4             SUBST     XXDTE:5       XXYDTE            4
     C*
     C                   MOVEL(P)  XXMDTE        QQDTE            10
     C                   CAT       '/':0         QQDTE
     C                   CAT       XXDDTE:0      QQDTE
     C                   CAT       '/':0         QQDTE
     C                   CAT       XXYDTE:0      QQDTE
     C*
     C     CTKY          CHAIN     PCT01                              60
     C*
     C*** BEGIN ADD *** 04/26/00 **************************************
     C                   MOVEL(P)  CTDESC        Q#COMM          256
     C***  Begin Del  ***  02/11/08  **************************************
     C**********           CAT  'FUND':1  Q#COMM
     C***  End   Del  ***  02/11/08  **************************************
     C                   CAT       'YEAR:':1     Q#COMM
     C                   CAT       XAFYR:1       Q#COMM
     C   21              CAT       QQDTE:1       Q#COMM
     C   22              CAT       QQDTE:1       Q#COMM
     C   23              CAT       QQDTE:1       Q#COMM
     C   24              CAT       QQDTE:1       Q#COMM
     C   25              CAT       QQDTE:1       Q#COMM
     C   26              CAT       XXORNR:1      Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
     C*** END ADD *** 04/26/00 ****************************************
     C*
     C                   ENDSR                                                  DRTCOM
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
     C*       If audit company not blanks check that it is valid.
     C     SCACO         IFNE      *BLANKS
     C                   MOVE      SCACO         KKKEY
     C     ACKY          CHAIN     PAC01                              85
     C                   MOVE      *IN85         *IN90
     C                   ENDIF                                                  SCACO IFNE *BLANKS
     C*
     C*
     C  N90
     CAN 26AUREP         IFEQ      SCREP#
     C                   SETON                                        81  90
     C                   ENDIF
     C*
     C*  If audit rep is not blank then check that it is valid.
     C  N90SCREP#        IFNE      *BLANK
     C                   MOVE      SCREP#        KKSTAF
     C     UPKY          CHAIN     PUP01                              60
     C   60              SETON                                        82  90
     C                   ENDIF
     C*
     C*** BEG ADD *** 03/23/05 *****************************
     C*
     C*  If type of audit is not blank then check that it is valid.
     C  N90SCTYPA        IFNE      *BLANK
     C                   MOVE      SCTYPA        KKTYPA
     C     TAKY          CHAIN     PTA01                              60
     C*  60                SETON                     82  90
     C                   ENDIF
     C*** END ADD *** 03/23/05 *****************************
     C*
     C*  Check that dates are valid and format for 8 digits
     C*
     C  N90*IN09         IFEQ      *OFF
     C     SCRQDT        ANDNE     *ZERO
     C                   Z-ADD     SCRQDT        SRDTI                          Request date
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRQDT
     C     SRERR         IFNE      '0'
     C                   SETON                                        71  90
     C                   ENDIF
     C                   ENDIF                                                  *IN09 = *OFF
     C*
     C  N90*IN09         IFEQ      *OFF
     C     SCRCVD        ANDNE     *ZERO
     C     *IN21         OREQ      *ON
     C                   Z-ADD     SCRCVD        SRDTI                          Received dt
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRCVD
     C     SRERR         IFNE      '0'
     C                   SETON                                        72  90
     C                   ENDIF
     C                   ENDIF                                                  *IN21 = *ON
     C*
     C  N90*IN09         IFEQ      *OFF
     C     SCATDT        ANDNE     *ZERO
     C     *IN22         OREQ      *ON
     C                   Z-ADD     SCATDT        SRDTI                          Attempt dt
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCATDT
     C     SRERR         IFNE      '0'
     C                   SETON                                        73  90
     C                   ENDIF
     C                   ENDIF                                                  *IN22 = *ON
     C*
      ***** BEG ADD ***** 11/13/06 ***************************************
     C* Audit Calculated Date
     C*
     C  N90*IN09         IFEQ      *OFF
     C     SCACDT        ANDNE     *ZERO
     C                   Z-ADD     SCACDT        SRDTI                          Calculate date
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCACDT
     C     SRERR         IFNE      '0'
     C                   SETON                                        69  90
     C                   ENDIF
     C                   ENDIF                                                  *IN09 = *OFF
     C*
     C  N90*IN09         IFEQ      *OFF
     C     SCAPDT        ANDNE     *ZERO
     C                   Z-ADD     SCAPDT        SRDTI                          Audit Print Dt
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCAPDT
     C  N90SRERR         IFNE      '0'
     C                   SETON                                        87  90
     C                   ENDIF
     C                   ENDIF                                                  *IN09 = *OFF
      ***** END ADD ***** 11/13/06 ***************************************
     C*
     C  N90*IN09         IFEQ      *OFF
     C     SCCNTD        ANDNE     *ZERO
     C     *IN24         OREQ      *ON
     C                   Z-ADD     SCCNTD        SRDTI                          Contested dt
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCNTD
     C     SRERR         IFNE      '0'
     C                   SETON                                        76  90
     C                   ENDIF
     C                   ENDIF                                                  *IN24 = *ON
     C*
     C* Final Audit Print Date
     C  N90*IN09         IFEQ      *OFF
     C     SCPRDT        ANDNE     *ZERO
     C                   Z-ADD     SCPRDT        SRDTI                          Final Audit Prt
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPRDT
     C  N90SRERR         IFNE      '0'
     C                   SETON                                        74  90
     C                   ENDIF
     C                   ENDIF                                                  *IN09 = *OFF
     C*
     C  N90*IN09         IFEQ      *OFF
     C     SCMAIL        ANDNE     *ZERO
     C     *IN23         OREQ      *ON
     C                   Z-ADD     SCMAIL        SRDTI                          Mail date
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMAIL
     C     SRERR         IFNE      '0'
     C                   SETON                                        75  90
     C                   ENDIF
     C                   ENDIF                                                  *IN23 = *ON
     C*
     C*  Convert dates for comparisons
     C*
     C                   Z-ADD     SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2RCVD
     C*
     C                   Z-ADD     SCRQDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2RQDT
     C*
     C*  Verify audit received date >= audit request date
     C*
     C  N90
     CAN 21X2RCVD        IFLT      X2RQDT
     C                   SETON                                        77  90
     C                   ENDIF
     C*
     C*  Check flags are 'Y' or 'N'
     C*
     C  N90SCRSGN        IFNE      'Y'
     C     SCRSGN        ANDNE     'N'
     C                   SETON                                        78  90
     C                   ENDIF
     C*
     C  N90SCREFL        IFNE      'Y'
     C     SCREFL        ANDNE     'N'
     C                   SETON                                        79  90
     C                   ENDIF
     C*
     C  N90SCRAFL        IFNE      'Y'
     C     SCRAFL        ANDNE     'N'
     C                   SETON                                        80  90
     C                   ENDIF
     C*
     C  N90SCCTST        IFNE      'Y'
     C     SCCTST        ANDNE     'N'
     C                   SETON                                        88  90
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  03/02/99  **********************************
     C* IF WE ARE IN CHANGE MODE THEN CHECK THAT THE CONTESTED
     C*   DATE AND CONTESTED FLAG AGREE
     C     @@MODE        IFEQ      'C'
     C*
     C  N90SCCNTD        IFEQ      *ZERO
     C     SCCTST        ANDEQ     'Y'
     C     SCCNTD        ORNE      *ZERO
     C     SCCTST        ANDEQ     'N'
     C                   SETON                                        91  90
     C                   ELSE
     C                   SETOFF                                       91  90
     C                   ENDIF                                                  SCCNTD=*BLANK
     C*
     C                   ENDIF                                                  @@MODE='C'
     C***  END   ADD  ***  03/02/99  **********************************
      *
      * CHECK TO BE SURE RECORD HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N90AUDT          IFGT      SAVEDT
     C     AUDT          OREQ      SAVEDT
     C     AUTIME        ANDGT     SAVETM
     C                   SETON                                        86  90
     C                   ENDIF                                                  AUDT IFGT SAVEDT
      *
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
     C*** BEG ADD *** 03/23/05 *****************************
     C*===============================================================
     C* KGDO - SUBROUTINE FOR F7
     C*===============================================================
      *
      *  F7=SEARCH
      *
     C     KGDO          BEGSR
     C***
     C*** AUDIT CO ASSIGNMENT HISTORY
     C***
     C                   CALL      'W2284R'
     C                   PARM                    ##CALL
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      AUCO#         ##CO#
     C                   PARM      SCFND         ##FND
     C                   PARM      SCFNNM        #2FNAM           50
     C                   PARM      SCEMP#        ##EMP#
     C                   PARM      SCDIV         ##DIV
     C                   PARM      SCEDN         ##ENAM           50
     C                   PARM      SCFYR         ##FYR
     C                   PARM      SCCXDT        ##CXDT
     C                   PARM      SCPPFR        ##PPFR
     C                   PARM      SCPPTO        ##PPTO
     C                   PARM      SC#DIV        ###DIV
     C                   PARM      SCAGT#        ##AGT#
     C                   PARM      SCAGTA        ##AGTA
     C                   PARM      SCPDC#        ##PDC#
     C                   PARM      SCPDCA        ##PDCA
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *** 03/23/05 *****************************
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
      *
      *  F10=SEARCH
      *
     C     KJDO          BEGSR
      *
     C                   SETOFF                                       60
      *
     C                   SELECT
      *
      * AUDIT COMPANY SEARCH
      *
     C     HFLD          WHENEQ    'SCACO'
      *
     C                   CALL      'W2290R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      0             ##RRN             9 0
      *
     C     ##RRN         IFNE      0
     C     ##RRN         CHAIN     PAC02                              60
     C                   ENDIF                                                  ##RRN IFNE 0
      *
     C     *INKL         IFEQ      *OFF
     C     *IN60         ANDEQ     *OFF
     C     ##RRN         ANDNE     0
      *
     C*** BEG ADD *** 03/23/09 *****************************
     C                   MOVE      *BLANK        XXFLG             1
     C     ACKEY         IFNE      SCACO
     C                   MOVE      'Y'           XXFLG
     C                   ENDIF
     C*** END ADD *** 03/23/09 *****************************
      *
     C                   MOVE      ACKEY         SCACO
     C                   MOVE      ACNAME        SCACNM
     C                   SETON                                        83
      *
     C                   ENDIF                                                  *INKL IFEQ *OFF
      *
     C     HFLD          WHENEQ    'SCREP#'
      *
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
      *
     C     ##RRN         IFNE      0
     C     ##RRN         CHAIN     PUP02                              60
     C                   ENDIF                                                  ##RRN IFNE 0
      *
     C     *IN60         IFEQ      *OFF
     C     ##RRN         ANDNE     0
     C                   MOVE      UPSTAF        SCREP#
     C                   MOVEL     UPUSNM        SCREPA
     C                   SETON                                        84
      *
     C                   ELSE
      *
     C                   SETON                                        83
     C                   ENDIF                                                  *INKL IFEQ *OFF
      *
     C*** BEG ADD *** 03/23/05 *****************************
     C     HFLD          WHENEQ    'SCTYPA'
     C                   CALL      'W2281R'
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      0             ##RRN
      *
     C     ##RRN         IFNE      0
     C     *INKC         ANDNE     *ON
     C     *INKL         ANDNE     *ON
     C     ##RRN         CHAIN     PTA02                              60
     C                   MOVE      TATYPA        SCTYPA
     C                   MOVEL(P)  TATYPD        SCTYPD
     C                   ENDIF
      *
     C*** END ADD *** 03/23/05 *****************************
     C                   ENDSL                                                  HFLD WHEQ 'SCACO'
      *
     C                   SETON                                        90
      *
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      ***** BEG DEL ******* 02/03/04 **********************************
      ***** BEG ADD ******* 09/19/03 **********************************
     C***************      CLEARSCR01
      ***** END ADD ******* 09/19/03 **********************************
      ***** END DEL ******* 02/03/04 **********************************
     C*
     C                   MOVE      AUCO#         KKCO#
     C                   MOVE      AUFND         KKFND
     C                   MOVE      AUEMP#        KKEMP#
     C                   MOVE      AUDIV         KKDIV
     C                   MOVE      AUFYR         KKFYR
     C*
     C*  Get agent/producer information
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCAGTA
     C                   MOVEL     ##PNAM        SCPDCA
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C***** BEG ADD ******* 03/23/09 **********************************
     C                   MOVE      EMCNTY        KKCNTY
     C     EMPST         IFNE      *BLANKS
     C                   MOVE      EMPST         KKST
     C                   ELSE
     C                   MOVE      EMMST         KKST
     C                   ENDIF
     C                   MOVE      AUACO         XXACO
     C***** END ADD ******* 03/23/09 **********************************
     C*
     C     ELKY          CHAIN(N)  PEL01                              61
     C*
     C     FNKY          CHAIN     PFN01                              62
     C*
     C*  Find policy effective date
     C                   Z-ADD     1             XI                1 0
     C     *ZERO         LOOKUP    EF(XI)                                 70
     C*
     C     EF(XI)        IFNE      *ZERO
     C                   Z-ADD     5             XI
     C                   ELSE
     C                   SUB       1             XI
     C                   ENDIF
     C*
     C     XI            DOWGT     *ZERO
     C     EF(XI)        IFGT      ELPFDT
     C                   SUB       1             XI
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C*  Determine if cancellation date exists
     C     XI            IFEQ      *ZERO
     C                   MOVE      *OFF          *IN66
     C                   ELSE
     C     CX(XI)        IFEQ      *ZEROS
     C                   MOVE      *OFF          *IN66
     C                   ELSE
     C                   MOVE      *ON           *IN66
     C                   ENDIF                                                  CX,XI=*ZEROS
     C                   ENDIF                                                  XI IFEQ *ZERO
     C*
     C*  Move values into screen fields
     C                   MOVE      AUFND         SCFND
     C                   MOVEL     FNNAME        SCFNNM
     C                   MOVE      AUEMP#        SCEMP#
     C                   MOVE      AUDIV         SCDIV
     C                   MOVE      AUFYR         SCFYR
     C*
     C*  Call program to compress emp#, div, name
     C                   MOVE      AUEMP#        ##EMP#
     C                   MOVE      AUDIV         ##DIV
     C                   MOVE      EMNAME        ##NAME
     C*
     C                   CALL      'WA005R'      EDNPL
     C*
     C                   MOVEL     ##EDN         SCEDN
     C*
     C                   MOVE      AUACO         SCACO
     C                   MOVEL     AUACO         KKKEY
     C*
     C*  Chain to audit company file to get name
     C                   MOVE      *BLANKS       SCACNM
     C     ACKY          CHAIN     PAC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     ACNAME        SCACNM
     C                   ENDIF
     C*
     C     *IN66         IFEQ      *ON
     C                   Z-ADD     CX(XI)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   ENDIF
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPPFR
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPPTO
     C*
     C*
     C*  Begin calculation for division count
     C*
     C                   Z-ADD     0             SC#DIV                         COUNT DIVISIONS
     C*
     C     ELKY1         CHAIN     PEL02                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ELPFDT        IFNE      ELPTDT
     C                   ADD       1             SC#DIV
     C                   ENDIF                                                  ELPFDT<>ELPTDT
     C     ELKY1         READE     PEL02                                  60
     C                   ENDDO                                                  *IN60 = OFF
     C*  End calculation for division count
     C*
     C                   Z-ADD     AURQDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRQDT
     C*
     C                   Z-ADD     AURCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRCVD
     C*
     C                   Z-ADD     AUPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPRDT
     C*
     C                   Z-ADD     AUATDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCATDT
     C*
     C                   Z-ADD     AUMAIL        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMAIL
     C*
     C                   Z-ADD     AUCNTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCNTD
     C*
      ***** BEG ADD ***** 11/13/06 ***************************************
     C*  Audit Calculate date
     C                   Z-ADD     AUACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C*  Audit Print date
     C                   Z-ADD     AUAPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCAPDT
      ***** END ADD ***** 11/13/06 ***************************************
     C*
     C                   MOVE      AURSGN        SCRSGN
     C                   MOVE      AUREFL        SCREFL
     C                   MOVE      AURVFL        SCRAFL
      *****BEG ADD **** 07/16/07 **********************************************
     C*****
     C                   MOVEL     AUCOM4        SCTCK#
      *****END ADD **** 07/16/07 **********************************************
     C*
     C     AUCNTD        IFEQ      *ZEROS                                       Contested
     C***  BEGIN ADD  ***  03/02/99  **********************************
     C     AUCNFL        ANDNE     'Y'
     C***  END   ADD  ***  03/02/99  **********************************
     C                   MOVE      'N'           SCCTST                           flag
     C                   ELSE
     C                   MOVE      'Y'           SCCTST
     C                   ENDIF
     C*
     C*  Get names for staff codes from user parameter file
     C*
     C                   MOVE      AUREP         SCREP#                         Audit rep
     C                   MOVE      AUREP         KKSTAF
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        SCREPA
     C                   ELSE
     C                   MOVE      *BLANKS       SCREPA
     C                   ENDIF
     C*
     C*** BEG ADD *** 03/23/05 *****************************
     C*
     C                   MOVE      AUTYPA        SCTYPA
     C                   MOVE      AUTYPA        KKTYPA
     C     TAKY          CHAIN     PTA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  TATYPD        SCTYPD
     C                   ELSE
     C                   MOVE      *BLANKS       SCTYPD
     C                   ENDIF
     C*
     C*** END ADD *** 03/23/05 *****************************
     C*
     C                   MOVE      EMUWTR        SCUDWT                         Underwriter
     C                   MOVE      EMUWTR        KKSTAF
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        SCUWNM
     C                   ELSE
     C                   MOVE      *BLANKS       SCUWNM
     C                   ENDIF
     C*
     C                   MOVE      EMARVW        SCRVWR                         Reviewer
     C                   MOVE      EMARVW        KKSTAF
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        SCRVNM
     C                   ELSE
     C                   MOVE      *BLANKS       SCRVNM
     C                   ENDIF
     C*
     C                   MOVE      EMPSRV        SCPSC#                         Pol svc clk
     C                   MOVE      EMPSRV        KKSTAF
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        SCPSCA
     C                   ELSE
     C                   MOVE      *BLANKS       SCPSCA
     C                   ENDIF
     C*
     C                   MOVE      EMPACC        SCPAC#                         Premium
     C                   MOVE      EMPACC        KKSTAF                          acctg clk
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        SCPACA
     C                   ELSE
     C                   MOVE      *BLANKS       SCPACA
     C                   ENDIF
     C*
     C                   MOVE      EMLREP        SCLCR#                         Loss control
     C                   MOVE      EMLREP        KKSTAF                          rep
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        SCLCRA
     C                   ELSE
     C                   MOVE      *BLANKS       SCLCRA
     C                   ENDIF
     C*
     C                   MOVE      EMAMGR        SCMGR#                         Manager
     C                   MOVE      EMAMGR        KKSTAF
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        SCMGRA
     C                   ELSE
     C                   MOVE      *BLANKS       SCMGRA
     C                   ENDIF
     C*
     C*
     C     @@MODE        IFEQ      'O'
     C*
     C                   MOVE      *ON           *IN09
     C*
     C     @@OPT         CASEQ     '21'          OPT21
     C     @@OPT         CASEQ     '22'          OPT22
     C     @@OPT         CASEQ     '23'          OPT23
     C     @@OPT         CASEQ     '24'          OPT24
     C     @@OPT         CASEQ     '26'          OPT26
     C                   ENDCS
     C*
     C                   ENDIF                                                  @@MODE IFEQ 'O'
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*  Convert dates to *CYMD and update database
     C*
     C                   Z-ADD     SCRQDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AURQDT
     C*
     C                   Z-ADD     SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AURCVD
     C*
     C                   Z-ADD     SCATDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUATDT
     C*
     C                   Z-ADD     SCPRDT        SRDTI                          Final Audit prt
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUPRDT
     C*
     C                   Z-ADD     SCMAIL        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUMAIL
     C*
     C                   Z-ADD     SCCNTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUCNTD
     C*
      ***** BEG ADD ***** 11/13/06 ***************************************
     C*  Audit Calculate date
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUACDT
     C*
     C*  Audit Print date
     C                   Z-ADD     SCAPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUAPDT
      ***** END ADD ***** 11/13/06 ***************************************
     C*
     C*  Move other screen fields to database
     C*
     C                   MOVEL     SCACO         AUACO
     C                   MOVEL     SCREP#        AUREP
     C*** BEG ADD *** 03/23/05 *****************************
     C                   MOVEL     SCTYPA        AUTYPA
     C*** END ADD *** 03/23/05 *****************************
     C                   MOVEL     SCRSGN        AURSGN
     C                   MOVEL     SCREFL        AUREFL
     C                   MOVEL     SCRAFL        AURVFL
     C                   MOVEL     SCCTST        AUCNFL
      *****BEG ADD **** 07/16/07 **********************************************
     C                   MOVEL     SCTCK#        AUCOM4
      *****END ADD **** 07/16/07 **********************************************
     C*
     C                   ENDSR
     C*
     C***  BEGIN ADD  ***  09/23/99  *********************************
     C*================================================================
     C* MNTFLD - Update maintenance fields
     C*================================================================
     C*
     C     MNTFLD        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDAT
     C*
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSTIME        ELTIME
     C                   Z-ADD     SYSDAT        ELDT
     C*
     C                   ENDSR                                                  MNTFLD
     C***  END   ADD  ***  09/23/99  *********************************
     C*
     C*================================================================
     C* OPT21 - Process option 21 (Audit received)
     C*================================================================
     C     OPT21         BEGSR
     C*
     C                   MOVE      *ON           *IN21
     C*
     C***  BEGIN ADD  ***  11/05/98  **********************************
     C*  Assign received date to a hold date
     C                   Z-ADD     SCRCVD        HLDDTE            8 0
     C***  END   ADD  ***  11/05/98  **********************************
     C*
     C*  If date = 0 then default to current date
     C     SCRCVD        IFEQ      *ZERO
      ***** BEG CHG ***** 03/06/06 ************************************
     C***************      Z-ADD*DATE     SCRCVD
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SCRCVD
      ***** END CHG ***** 03/06/06 ************************************
     C                   ENDIF
     C*
     C                   ENDSR                                                  OPT21
     C*================================================================
     C* OPT22 - Process option 22 (Audit Attempt)
     C*================================================================
     C     OPT22         BEGSR
     C*
     C                   MOVE      *ON           *IN22
     C*
     C***  BEGIN ADD  ***  11/05/98  **********************************
     C*  Assign attempted date to a hold date
     C                   Z-ADD     SCATDT        HLDDTE
     C***  END   ADD  ***  11/05/98  **********************************
     C*
     C*  If date = 0 then default to current date
     C     SCATDT        IFEQ      *ZERO
      ***** BEG CHG ***** 03/06/06 ************************************
     C************         Z-ADD*DATE     SCATDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SCATDT
      ***** END CHG ***** 03/06/06 ************************************
     C                   ENDIF
     C*
     C                   ENDSR                                                  OPT22
     C*================================================================
     C* OPT23 - Process option 23 (Final audit mailed)
     C*================================================================
     C     OPT23         BEGSR
     C*
     C                   MOVE      *ON           *IN23
     C*
     C***  BEGIN ADD  ***  11/05/98  **********************************
     C*  Assign mail date to a hold date
     C                   Z-ADD     SCMAIL        HLDDTE
     C***  END   ADD  ***  11/05/98  **********************************
     C*
     C*  If date = 0 then default to current date
     C     SCMAIL        IFEQ      *ZERO
      ***** BEG CHG ***** 03/06/06 ************************************
     C**************       Z-ADD*DATE     SCMAIL
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SCMAIL
      ***** END CHG ***** 03/06/06 ************************************
     C                   ENDIF
     C*
     C     SCRCVD        IFEQ      *ZERO
     C                   Z-ADD     SCPRDT        SCRCVD
     C                   ENDIF
     C*
     C                   ENDSR                                                  OPT23
     C*================================================================
     C* OPT24 - Process option 24 (Contested)
     C*================================================================
     C     OPT24         BEGSR
     C*
     C                   MOVE      *ON           *IN24
     C*
     C***  BEGIN ADD  ***  11/05/98  **********************************
     C*  Assign contested date to a hold date
     C                   Z-ADD     SCCNTD        HLDDTE
     C***  END   ADD  ***  11/05/98  **********************************
     C*
     C*  If date = 0 then default to current date
     C     SCCNTD        IFEQ      *ZERO
      ***** BEG CHG ***** 03/06/06 ************************************
     C**************       Z-ADD*DATE     SCCNTD
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SCCNTD
      ***** END CHG ***** 03/06/06 ************************************
     C                   ENDIF
     C*
     C                   MOVE      'Y'           SCCTST
     C*
     C                   ENDSR                                                  OPT24
     C*================================================================
     C* OPT25 - Process option 25 (Clear contested info)
     C*================================================================
     C     OPT25         BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     *ZEROS        AUCNTD
     C                   MOVE      'N'           AUCNFL
     C*
     C                   MOVEL     Q1USER        AUUSER
     C                   MOVEL     Q1PGM         AUPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   MOVEL     SRDTO         AUDT
     C                   MOVEL     DSTIME        AUTIME
     C*
     C                   UPDATE    DBRCDF
      *
      ***  BEGIN ADD  ***  10/06/98  **********************************
     C*  Initialize key fields
     C                   Z-ADD     AUCO#         KKCO#
     C                   Z-ADD     AUFND         KKFND
     C                   Z-ADD     AUEMP#        KKEMP#
     C                   Z-ADD     AUDIV         KKDIV
     C                   Z-ADD     AUFYR         KKFYR
      ***  END   ADD  ***  10/06/98  **********************************
     C*
     C     ELKY          CHAIN     PEL01                              61
      ***  BEGIN ADD  ***  10/06/98  **********************************
     C     *IN61         IFEQ      *OFF
      ***  END   ADD  ***  10/06/98  **********************************
     C                   MOVE      ' '           ELCTST
     C***  BEGIN ADD  ***  09/23/99  *********************************
     C                   EXSR      MNTFLD
     C***  END   ADD  ***  09/23/99  *********************************
     C                   UPDATE    PEL01
      ***  BEGIN ADD  ***  10/06/98  **********************************
     C                   ENDIF                                                  *IN61=*OFF
      ***  END   ADD  ***  10/06/98  **********************************
     C*
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C                   ENDSR                                                  OPT25
     C*================================================================
     C* OPT26 - Process option 26 (Reassign audit rep)
     C*================================================================
     C     OPT26         BEGSR
     C*
     C                   MOVE      *ON           *IN26
     C*
     C                   MOVE      AUREP         XXREP             3
     C*
     C                   MOVE      'Y'           SCRSGN
     C*
     C                   ENDSR                                                  OPT26
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C*
     C*  Update file if options 21-26 are chosen
     C*
     C     @@MODE        CASEQ     'O'           UPDATR
     C                   ENDCS
     C*
     C*  Update contested flag in employer detail if option 24 or 25 chosen
     C*
     C     @@MODE        IFEQ      'O'
     C     @@OPT         IFEQ      '24'
     C     ELKY          CHAIN     PEL01                              61
     C                   MOVE      'Y'           ELCTST
     C***  BEGIN ADD  ***  09/23/99  *********************************
     C                   EXSR      MNTFLD
     C***  END   ADD  ***  09/23/99  *********************************
     C                   UPDATE    PEL01
     C                   ENDIF                                                  @@OPT='24'
     C     @@OPT         IFEQ      '25'
     C     ELKY          CHAIN     PEL01                              61
     C                   MOVE      ' '           ELCTST
     C***  BEGIN ADD  ***  09/23/99  *********************************
     C                   EXSR      MNTFLD
     C***  END   ADD  ***  09/23/99  *********************************
     C                   UPDATE    PEL01
     C                   ENDIF                                                  @@OPT='25'
     C                   ENDIF                                                  @@MODE='O'
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
      *
     C***** BEGIN CHG **** 04/23/99 ***********************************
     C********** *INKJ     CASEQ*ON       KJDO           90
     C  N09*INKJ         CASEQ     *ON           KJDO                     90
     C***  BEGIN ADD  ***  06/07/99  **********************************
     C   09
     CAN 21*INKJ         CASEQ     *ON           KJDO                     90
     C   09
     CAN 26*INKJ         CASEQ     *ON           KJDO                     90
     C***  END   ADD  ***  06/07/99  **********************************
     C***** END CHG ****** 04/23/99 ***********************************
     C*** BEG ADD *** 03/23/05 **********************************
     C     *INKG         CASEQ     *ON           KGDO                     90
     C*** END ADD *** 03/23/05 **********************************
      *
     C                   ENDCS                                                  *INKJ CASEQ *ON
      *
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *OFF          *IN89
     C*
     C*  Check if enter was pressed to update DARTS
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C     AXKY          CHAIN     PAX01                              60
     C     *IN60         IFEQ      *OFF
     C                   SELECT
     C*
     C     @@OPT         WHENEQ    '21'
     C     AXRCCD        IFNE      *BLANK
     C***  BEGIN ADD  ***  11/05/98  **********************************
     C     SCRCVD        ANDNE     HLDDTE
     C***  END   ADD  ***  11/05/98  **********************************
     C                   MOVEL     AXRCCD        QQRMCD
     C                   MOVEL     SCRCVD        XXDTE             8
     C                   MOVE      *ON           *IN89
     C                   ENDIF
     C*
     C     @@OPT         WHENEQ    '22'
     C     AXAACD        IFNE      *BLANK
     C***  BEGIN ADD  ***  11/05/98  **********************************
     C     SCATDT        ANDNE     HLDDTE
     C***  END   ADD  ***  11/05/98  **********************************
     C                   MOVEL     AXAACD        QQRMCD
     C                   MOVEL     SCATDT        XXDTE
     C                   MOVE      *ON           *IN89
     C                   ENDIF
     C*
     C     @@OPT         WHENEQ    '23'
     C     AXFMCD        IFNE      *BLANK
     C***  BEGIN ADD  ***  11/05/98  **********************************
     C     SCMAIL        ANDNE     HLDDTE
     C***  END   ADD  ***  11/05/98  **********************************
     C                   MOVEL     AXFMCD        QQRMCD
     C                   MOVEL     SCMAIL        XXDTE
     C                   MOVE      *ON           *IN89
     C                   ENDIF
     C*
     C     @@OPT         WHENEQ    '24'
     C     AXACCD        IFNE      *BLANK
     C***  BEGIN ADD  ***  11/05/98  **********************************
     C     SCCNTD        ANDNE     HLDDTE
     C***  END   ADD  ***  11/05/98  **********************************
     C                   MOVEL     AXACCD        QQRMCD
     C                   MOVEL     SCCNTD        XXDTE
     C                   MOVE      *ON           *IN89
     C                   ENDIF
     C*
     C     @@OPT         WHENEQ    '25'
     C     AXCCCD        IFNE      *BLANK
     C                   MOVEL     AXCCCD        QQRMCD
      ***** BEG CHG ***** 03/06/06 ************************************
     C**************       Z-ADD*DATE     XXDTE
     C                   TIME                    DSDTTM
     C                   MOVEL     DSDATE        XXDTE
      ***** END CHG ***** 03/06/06 ************************************
     C                   MOVE      *ON           *IN89
     C                   ENDIF
     C*
     C     @@OPT         WHENEQ    '26'
     C     AXREAR        IFNE      *BLANK
     C                   MOVEL     AXREAR        QQRMCD
      ***** BEG CHG ***** 03/06/06 ************************************
     C**************       Z-ADD*DATE     XXDTE
     C                   TIME                    DSDTTM
     C                   MOVEL     DSDATE        XXDTE
      ***** END CHG ***** 03/06/06 ************************************
     C                   MOVE      *ON           *IN89
     C     'Old rep:'    CAT       XXREP:1       XXORNR           27
     C                   CAT       ', Ne':0      XXORNR
     C                   CAT       'w rep:':0    XXORNR
     C                   CAT       SCREP#:1      XXORNR
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C     *IN89         IFEQ      *ON
     C                   EXSR      DARTS
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  *INKC=*OFF
     C*
     C                   MOVE      *OFF          *IN21
     C                   MOVE      *OFF          *IN22
     C                   MOVE      *OFF          *IN23
     C                   MOVE      *OFF          *IN24
     C                   MOVE      *OFF          *IN25
     C                   MOVE      *OFF          *IN26
     C*
     C***  BEGIN ADD  ***  03/02/99  **********************************
     C*  Check if enter was pressed to update employer detail
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     @@MODE        ANDEQ     'C'
     C*
     C***  BEGIN CHG  ***  09/23/99  *********************************
     C********** ELKY      CHAINPEL01                60
     C     ELKY          CHAIN(N)  PEL01                              60
     C***  END   CHG  ***  09/23/99  *********************************
     C*
     C     ELCTST        IFNE      SCCTST
     C*
     C***  BEGIN ADD  ***  09/23/99  *********************************
     C     ELKY          CHAIN     PEL01                              60
     C***  END   ADD  ***  09/23/99  *********************************
     C*
     C     SCCTST        IFEQ      'Y'
     C                   MOVE      'Y'           ELCTST
     C                   ELSE
     C                   MOVE      ' '           ELCTST
     C                   ENDIF                                                  SCCTST='Y'
     C*
     C***  BEGIN ADD  ***  09/23/99  *********************************
     C                   EXSR      MNTFLD
     C***  END   ADD  ***  09/23/99  *********************************
     C*
     C                   UPDATE    PEL01
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  *INKC=*OFF
     C***  END   ADD  ***  03/02/99  **********************************
     C*
      ***** BEG ADD ******* 09/19/03 **********************************
     C                   SETON                                        LR
      ***** END ADD ******* 09/19/03 **********************************
     C                   ENDSR                                                  PGMEND
      *
      *
      *
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
      *****BEG ADD **** 07/16/07 **********************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C     CLTID         IFEQ      '093'
     C                   MOVE      *ON           *IN41
     C                   ELSE
     C                   MOVE      *OFF          *IN41
     C                   ENDIF
      *****END ADD **** 07/16/07 **********************************************
     C                   MOVE      *OFF          *IN20
     C                   MOVE      *OFF          *IN21
     C                   MOVE      *OFF          *IN22
     C                   MOVE      *OFF          *IN23
     C                   MOVE      *OFF          *IN24
     C                   MOVE      *OFF          *IN25
     C                   MOVE      *OFF          *IN26
      ***** BEG ADD ******* 07/01/02 **********************************
     C                   SETOFF                                       67
      ***** END ADD ******* 07/01/02 **********************************
     C*
      *****BEG ADD **** 01/08/10 **********************************************
     C                   CLEAR                   EF
     C                   CLEAR                   CX
      *****END ADD **** 01/08/10 **********************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'O'
     C     @@OPT         ANDNE     '25'
     C                   MOVE      *ON           *IN20
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   MOVE(P)   MODEC         SCMODE
     C                   EXSR      MVDBSC
     C*
     C     @@MODE        WHENEQ    'O'
     C     @@OPT         ANDEQ     '25'
     C*
     C                   EXSR      OPT25
     C                   EXSR      PGMEND
     C                   RETURN
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        AUUSER
     C                   MOVEL     Q1PGM         AUPGM
     C                   Z-ADD     DSTIME        AUTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUDT
     C*
     C                   UPDATE    DBRCDF
     C*** BEG ADD *** 03/23/09 *****************************
     C     XXFLG         IFEQ      'Y'
     C*          AUTYPA    ORNE SCTYPA
    C*
     C                   MOVE      *ZEROS        KKPRML
    C*
    C                   MOVE      AUCO#         KKCO#
    C                   MOVE      AUFND         KKFND
    C                   MOVE      AUEMP#        KKEMP#
    C                   MOVE      AUFYR         KKFYR
    C*
     C     KYEL          CHAIN     PEL011                             99
    C*
     C     *IN99         DOWEQ     *OFF
    C*
     C     ELFAF         IFEQ      'A'
     C     KKPRML        ADD       ELAAPR        KKPRML
     C                   ELSE
     C     KKPRML        ADD       ELANPR        KKPRML
     C                   ENDIF
     C     KYEL          READE     PEL011                                 99
     C                   ENDDO
    C*
     C     KYAAST        SETGT     PAA01
     C     KYAA          READPE    PAA01                                  99
    C*
     C                   EXSR      WRTAH
     C                   ENDIF                                                  AUACO <> SCACO
     C*** END ADD *** 03/23/09 *****************************
E01  C                   ENDIF
      ***** BEG ADD ******* 04/06/10 **********************************
     C     EXKY          CHAIN     PEX01                              60
      ***** END ADD ******* 04/06/10 **********************************
      ***** BEG ADD ******* 01/21/09 **********************************
     C                   EXSR      DRT2
      ***** END ADD ******* 01/21/09 **********************************
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      WMAUP
     C                   ENDSR
     C*** BEG ADD *** 03/23/09 *****************************
     C*================================================================
     C* WRTAH  - WRITE AUDIT HISTORY RECORD
     C*================================================================
     C     WRTAH         BEGSR
     C     EMPZIP        IFNE      *ZEROS
     C                   MOVE      EMPZIP        XXPZIP            9 0
     C                   ELSE
     C                   MOVE      EMMZIP        XXPZIP            9 0
     C                   ENDIF                                                   PHYSICAL ZIP NOT EX
     C     ELFAF         IFEQ      'A'
     C                   MOVE      ELAAPR        XXAAPR           11 2
     C                   ELSE
     C                   MOVE      ELANPR        XXAAPR           11 2
     C                   ENDIF                                                   NORMAL PREM VAS EST
     C                   CLEAR                   PHA01
     C                   Z-ADD     AUCO#         HACO#
     C                   Z-ADD     AUFND         HAFND
     C                   Z-ADD     AUEMP#        HAEMP#
     C                   Z-ADD     AUDIV         HADIV
     C                   Z-ADD     AUFYR         HAFYR
     C                   MOVE      XXACO         HAPAC
     C                   MOVE      SCACO         HANAC
     C                   MOVE      AATYPA        HATYPA
     C                   MOVEL     AAST          HAST
     C                   MOVEL     XXPZIP        HAZIP
     C                   MOVE      EMCNTY        HACNTY
     C                   MOVE      ELTYPB        HATYPB
     C                   MOVEL     KKPRML        HAPREM
     C                   MOVEL     AASTAT        HASTAT
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        HAUSER
     C                   MOVEL     Q1PGM         HAPGM
     C                   Z-ADD     DSTIME        HATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HADT
     C                   MOVEL     Q1USER        HAIUSR
     C                   MOVEL     Q1PGM         HAIPGM
     C                   Z-ADD     DSTIME        HAITM
     C                   MOVE      HADT          HAIDT
     C                   WRITE     PHA01
     C                   ENDSR
      *** END ADD **** 03/23/09 ***************************************
