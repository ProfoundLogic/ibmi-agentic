     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE DA700R - API TO WRITE A DARTS REMARK
     H*****************************************************************
      *  Description:                                                   *
      *
      *  This API will write a DARTS remark.
      *  It will not do any validation of the passed parameters.
      *  SDCMP/CMCOMM is 275 CHAR LONG.  The max length of a parm is
      *  256. If the comment is longer than 256, the additional 19
      *  bytes are passed in QQCOM2: i.e. QQCOMM and QQCOM2 will
      *  concatenate into CMCOMM.
      *  If QQFL01 is passed in a blanks and QQDIFL is passed in as 'Y'
      *  QQFL01 will be overwritten with the intended value from STCTP.
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SEND TO THIS PGM:  WHATEVER YOU WANT WRITTEN TO DARTS.
      *
      *  SEND BACK       :  NEXT AVAILIBLE SEQUENCE# FOR A DARTS REMARK.
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:                                                   *
      *    VARIOUS PROGRAMS                                             *
      *                                                                 *
      *  Calls To:                                                      *
      *    NONE                                                         *
      *                                                                 *
      *  Created:   4/01/98  C:47  Project:  87MCB                      *
      *                                                                 *
      * *****************************************************************
      ** REVISIONS:                                                     *
      *                                                                 *
      * 02/13/23 C:62  Project: WC2838                                  *
      *                Add path parameter that can hold a full path.    *
      *                                                                 *
      * 06/07/17 C:62  Support: 47410(114)                              *
      *                Use darts security level instead of user.        *
      *                                                                 *
      * 04/02/15 C:65  Project : 93QKN/Phase#33994                      *
      *                Check SBYL parameter to see if remark should be  *
      *                excluded from data queue. Condition it on        *
      *                parameter being passed.                          *
      *                                                                 *
      *  04/01/14 C:282 Support FQ099 85204                             *
      *                 Added the handling for data center client.      *
      *                                                                 *
      * 08/27/13 C:190 Project : 93QKN                                  *
      *                Write DATAQ for FQ.                              *
      *
      * 08/09/13 C:33  Redmine
      *         #3538  Ensure document path is transferred, as well
      *         #3557  Allow for the environment (CMIFSP) to be passed in
      *
      * 04/02/13 C:062 REDMINE 2596
      *                CORRECT HOW THE UNC PATH IS BROKEN OUT.
      *
      * 11/16/12 C:282 PROJECT SY195 Priss 88
      *                Update the path for data center client.
      *                                                               *
      * 11/05/12 C:191 PROJECT SY195
      *                Error in compile time data chars UVWXYZ missing
      *
      * 10/17/11 C:279 PROJECT SY195
      *                Modified for building FSF and UNCN path.
      *
      * 10/12/11 C:180 SUPPORT 77474
      *                Do not write records with a blank key.
      *
      * 04/08/11 C:062 PROJECT : 74834                                *
      *                DEFAULT CMCDAT TO THE INIT DATE IF 0.          *
     H*                                                               *
     H* 11/02/10 C:183 Project : SY195                                *
     H*                Add two more parms for the path and whether to *
     H*                add root directory (Y/N). Retrieve the document*
     H*                name and folder name from the path and save it *
     H*                to CMFOLD and CMDCID.                          *
      *                                                               *
      * 05/15/09 C:189 PROJECT : 139WLJ                               *
      *                WORK WITH MULTIPLE DARTS DIARY RECIPIENTS      *
      *                                                               *
      * 03/25/08 C:33  Seq#: ID071/61386                              *
      *                Update CMFL01 based upon CTDEPG.               *
      *                                                               *
      * 01/10/08 C:062 ID135 61634                                    *
      *                Condition CMSBYL usage if iDarts               *
      *                                                               *
      * 01/25/07 C:002 DR214                                          *
      *                Added code to PUT CMIUSR INTO THE AIIUSR FLD   *
      *                 AND CMUSER INTO AIUSER INSTEAD OF Q1 VALUES   *
      *                                                               *
      * 04/21/06 C:015 Support: VI099/43272                           *
      *                Added code to pull the diary flag from STCTP   *
      *                if the incoming diary flag is blank.           *
      *                                                               *
      * 03/07/05 C:062 PROJECT: DR025 45413                           *
      *                Load the corrected comment into cmcomm         *
      *                                                               *
      * 07/23/04 C:062 PROJECT: SY100                                 *
      *                Add Data center processing                     *
      *                                                               *
      * 06/14/04 C:062 PROJECT: 129WCS                                *
      *                Add process to allow both a data entry pgm     *
      *                and reference an image                         *
      *
      *  02/05/04 C:128  Support:                                       *
      *                  Changed pgm to replace any character less      *
      *                  than Hex 40 and greater than Hex F9 with       *
      *                  Hex 40. These characters can not be displayed  *
      *                  on the AS400.
      *                                                                 *
      *  10/16/03 C:062  Project: DRMNT                                 *
      *                  Add additional parms.                          *
      *                                                                 *
      *  06/19/02 C:128  Support: 33066                                 *
      *                  Changed pgm to check for Hex 15. This un-      *
      *                  displayable character is being keyed by users. *
      *                  Hex 15 is being replaced by Hex 40(blank).     *
      *                                                                 *
      *  03/26/02 C:69   Support: 31781                                 *
      *                  Changed pgm to check for error when writing    *
      *                  record; retry if occurs                        *
      *                                                                 *
     H*  04/25/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Add parms
     H*                 2. Correct STCTP chain logic
     H*
      *  11/01/99 C:15   Support: DA087/20234                           *
      *                  Changed pgm to retrieve the next available     *
      *                  seq# immediately prior to writing the record.  *
      *                                                                 *
      *  08/31/99 C:53   Project:  DRMNT
      *                  SWAP SMUPL FOR WMUPL
      *                                                                 *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
      *  05/21/99 C:118   Project:  DR066
      *                   Revise the *ENTRY parm list to include the
      *                   Security flag. Modified the Secure by Level
      *                   logic to handle increase number of parms.
      *                   Logic was added to account for QQSBYL=blank.
      *
      *  05/15/98 C:53    Project:  MC145
      *                   REMOVE LR INDICATOR
      *                                                                 *
      * *****************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      *    60 - General File Access                                     *
      * *****************************************************************
      *
     FSDCMLI    IF   E           K DISK
     F                                     RENAME(PCM01:PCM0I)
     FSTCTL1    IF   E           K DISK
     FSDCMP     O    E             DISK
      *
     FSDAIP     O    E             DISK
     FSMUPL     IF   E           K DISK
     FWMDRL     IF   E           K DISK
     F*** Begin Add *** 06/07/17 ********************************
     Fsmsll     IF   E           K DISK    Rename(psl01:psl00)
     F*** End   Add *** 06/07/17 ********************************
      *

     D CM              S              1    DIM(275)
     D AL              S             64    DIM(1) CTDATA PERRCD(1)
      *
     D                 DS
     D  TFOLD                  1     80
     D  TEST                   1      4
     D  FOLDER                 9     80
     D RPGM            C                   CONST('RTVDOCROOT')
     D RPGM1           C                   CONST('RTVDOCENV')
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     DisUseDataEntPgm  s              1n   inz
     D@@env            s                   like(CMifsP)
     d***  Begin Add  ***  08/27/13  **************************************
     d flagDATAQ       s             50    DTAARA(DRTDTAARA)
     d pDataq          s             10a   inz('BILFQDTAQ')
     **** begin chg ***** 04/01/14 ************************************
     d***** pDataQLib       s             10a   inz('PDSOBJ')
     d pDataQLib       s             10a
     **** end   chg ***** 04/01/14 ************************************
     d pEntLength      s              5  0 inz(512)
     d pData           s            512a
     d pKeyLen         s              3  0 inz(25)
     d pKeyID          s             25
     d intCount        s              5  0 inz
     d strChr          s              5
     d outName         s             10
     d outType         s              7
     d outRtcd         s              1

      * Check Object
     dGetObject        pr                  extpgm('SRCHKOBJ')
     d outName                             like(outName)
     d outType                             like(outType)
     d outRtcd                             like(outRtcd)

     dSendToDtaq       pr                  extpgm('QSNDDTAQ')
     d pDataq                        10a
     d pDataqLib                     10a
     d pEntLength                     5  0
     d pData                        512a
     d pKeyLen                        3  0
     d pKeyID                        25
     d***  End   Add  ***  08/27/13  **************************************
      *

     IPCM0I
     I              CMKEY                       C2KEY
     I              CMSEQ#                      C2SEQ#
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

     C*
     C*   KEY LIST FOR THE REMARK TYPE TABLE FILE
     C*
     C     CTKEY         KLIST
     C                   KFLD                    XXRMCD
      *
      *  DARTS FILE
      *
     C     C2KY          KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    XXSEQ#
     C*  KEY LIST FOR USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    KKUSER
      *
     C     *LIKE         DEFINE    CMDT          XXDT
     C     *LIKE         DEFINE    CMRMCD        XXRMCD                         5A
     C     *LIKE         DEFINE    CMSEQ#        XXSEQ#
     C     *LIKE         DEFINE    QQUSER        KKUSER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40            01
     C                   PARM                    QQCOMM          256            02
     C                   PARM                    QQCDAT            8            03 MDCY
     C                   PARM                    QQDCID           12            04
     C                   PARM                    QQPNAM           10            05
     C                   PARM                    QQSECL            3            06
     C                   PARM                    QQUSER           10            07
     C                   PARM                    QQPGM            10            08
     C                   PARM                    QQFOLD           80            09
     C                   PARM                    QQDIFL            1            10
     C                   PARM                    QQIPGM           10            11
     C                   PARM                    QQFL01            1            12
     C                   PARM                    QQFL02            1            13
     C                   PARM                    QQRMCD            5            14
     C                   PARM                    QQSEQ#            5            15
     C                   PARM                    QQSBYL            1            16
     C                   PARM                    QQCOM2           19            17
     C                   PARM                    QQAPID            2            18
     C                   PARM                    QQIDOC           12            19
     C                   PARM                    QQIQUE           10            20
     C                   PARM                    QQIMGF            1            21
     C                   PARM                    QQIPTH           10            22
     C                   PARM                    QQPIDX            5            23
     C                   PARM                    QQQPTY            1            24
     C                   PARM                    QQDCTP            5            25
     C                   PARM                    QQCSID            2            26
     C                   PARM                    QQCODE            2            27
     C                   PARM                    QQTRDT            7            28
     C                   PARM                    QQTRTM            6            29
     C                   PARM                    QQSTAT            1            30
     C                   PARM                    QQIUSR           10            31
     C                   PARM                    QQDCDT            7            32
     C                   PARM                    QQFL03            1            33
     C                   PARM                    QQFL04            1            34
     C                   PARM                    QQFL05            1            35
     C                   PARM                    QQIDT             7            36
     C                   PARM                    QQVCFN            1            37
     C                   PARM                    QQDPGM           10            38
     C                   PARM                    QQDKEY           40            39
     C                   PARM                    QQPATH           80            40
     C                   PARM                    QQRPYN            1            41 (Add Root
      *                                                    to Path Y/N)
     C                   PARM                    @@Env             1            42  Pass as blank
     C*** Begin Add *** 02/13/23 **************************************
     C                   PARM                    @@uncn          255            43
     C*** End   Add *** 02/13/23 **************************************
     C*

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C     QQKEY         IFEQ      *BLANKS
     C                   MOVE      *OFF          MULTI
     C                   GOTO      INACT
     C                   ENDIF
     C                   CALL      'SA001R'
01   C                   PARM      *BLANKS       ##WCIF            1            WC INST
02   C                   PARM      *BLANKS       ##MCIF            1            MC INST
03   C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
04   C                   PARM      *BLANKS       ##AR              1            MC AR INST
05   C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
06   C                   PARM      *BLANKS       ##UR              1            PRECERT INST
07   C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
08   C                   PARM      *BLANKS       ##EDI             1            EDI INST
09   C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
10   C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
11   C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
12   C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
13   C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILLS
14   C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
15   C                   PARM      *BLANKS       ##OS              1            OUTOME/SAVINGS
16   C                   PARM      *BLANKS       ##ARLS            1            Auto Release Flag
17   C                   PARM      *BLANKS       ##CCOD            1            Correct Coding Flag
18   C                   PARM      *BLANKS       ##CRVW            1            Code Review Flag
19   C                   PARM      *BLANKS       ##PROM            1            ProMatch Flag
20   C                   PARM      *BLANKS       ##MCRV            1            Medicare CR Flag
21   C                   PARM      *BLANKS       ##FREQ            1            Frequency Flag
22   C                   PARM      *BLANKS       ##CRAC            1            CR Auto Complete
23   C                   PARM      *BLANKS       ##PREO            1            CR Auto Complete
24   C                   PARM      *BLANKS       ##PARN            1            Par/Non-Par/Both
25   C                   PARM      *BLANKS       ##MGRI            1            Multi Group Restr
26   C                   PARM      *BLANKS       ##KCIF            1            Keep Claimant info
27   C                   PARM      *BLANKS       ##CURS            1            PBILLS entry cursor
28   C                   PARM      *BLANKS       ##PCIB            1            Prov Check in Batch
29   C                   PARM      *BLANKS       ##CLT#            3            Client Number
30   C                   PARM      *BLANKS       ##UCPF            1            UCR Percentile
31   C                   PARM      *BLANKS       ##DCF             1            Data Center
32   C                   PARM      *BLANKS       ##AUTB            1            Auto Batch   Flag
33   C                   PARM      *BLANKS       ##DEIF            1            DE/Image Flag
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C                   CLEAR                   PCM01                          ADD 05/15/98
      ***  Begin Add  ***  08/27/13  **************************************
      /free
       intCount = 0;
      /end-free
      ***  End   Add  ***  08/27/13  **************************************
      ***  Begin Add  ***  04/02/15  **************************************
     C                   Clear                   excludeRec        1
     C
     C     Q1PRMS        IFGT      15
      *
     C                   If        QQSBYL = '$'
     C                   Eval      excludeRec = 'Y'
     C                   Eval      QQSBYL = 'N'
     C                   Endif
      *
     C                   Endif
      ***  End   Add  ***  04/02/15  **************************************
      *
      * GET TODAY'S DATE
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
      *
      *   ESTABLISH RECORD
     C                   MOVEL     QQKEY         CMKEY
     C                   MOVE      *BLANKS       CMCSID                         CUSTOM GROUP ID
     C     Q1PRMS        IFGE      26
     C                   MOVE      QQCSID        CMCSID                         CUSTOM GROUP ID
     C                   ENDIF                                                  PRMS >= 26
     C                   MOVEL(P)  QQRMCD        XXRMCD
     C     CTKEY         CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       CTDESC
     C                   MOVE      *BLANKS       CTSMCD
     C                   MOVE      *BLANK        CTFL02
     C                   Z-ADD     0             CTDSLV
     C                   MOVE      'N'           CTFL01
     C                   MOVE      *BLANKS       CTDEPG
     C                   ENDIF                                                  *IN60 = On
      *
     C     QQCOMM        IFEQ      *BLANK
     C                   MOVEL(P)  CTDESC        CMCOMM                         COMMENT
      *
     C                   ELSE
     C                   MOVEL(P)  QQCOMM        CMCOMM                         COMMENT
     C                   END
      *
     C                   MOVE      QQCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCDAT                         REMARK DATE
     C     CMCDAT        IFEQ      0
     C                   Z-ADD     XXDT          CMCDAT
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      27
     C                   MOVEL     QQCODE        CMCODE                         REMARK TYPE
     C                   ENDIF                                                  PRMS >= 27
     C                   MOVEL     QQDCID        CMDCID                         DOC ID
     C                   MOVEL     QQPNAM        CMPNAM                         KEYED PGM
     C                   Z-ADD     XXDT          CMTRDT                         DATE
     C                   Z-ADD     DSTIME        CMTRTM                         TIME
      *
     C                   MOVE      QQSECL        CMSECL                         SECURITY LEVEL
      *
      ***  Begin Add  ***  08/27/13  **************************************
      /free
        outName = 'DRTDTAARA';
        outType = '*DTAARA';
        outRtcd = ' ';
        GetObject(outName:outType:outRtcd);
        If outRtcd <> '1';
           In *lock flagDATAQ;
           strChr = %subSt(flagDATAQ:46:5);
           If strChr <> *blanks;
              intCount = %Int(strChr);
           Endif;
        Endif;
      /end-free
      ***  End   Add  ***  08/27/13  **************************************
      * SECURE BY LEVEL
     C*
     C                   SELECT
     C     QQUSER        WHENNE    *BLANKS
     C                   MOVE      *OFF          *IN60
     C     QQUSER        WHENEQ    *BLANKS
     C     CTKEY         SETLL     PDR01
     C     CTKEY         READE     PDR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           MULTI             1
     C                   ENDIF
     C                   ENDSL
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     DRSTAT        IFEQ      'I'
     C                   GOTO      INACT
     C                   ENDIF
     C*
     C     MULTI         IFEQ      *ON
     C                   MOVEL(P)  DRRCID        QQUSER
     C                   ENDIF
      *
     C     Q1PRMS        IFGT      15
     C     QQIPGM        IFNE      'IDARTS'
     C*
     C     QQSBYL        IFNE      'Y'
     C*
     C     QQSECL        IFEQ      *ZERO
     C                   MOVE      'N'           CMSBYL
     C                   ELSE
     C                   MOVE      'Y'           CMSBYL
     C                   ENDIF                                                  QQSECL=0
     C*
     C                   ELSE
     C*
     C                   MOVE      'Y'           CMSBYL
     C*
     C     QQSECL        IFEQ      *ZERO
     C                   MOVEL(P)  QQRMCD        XXRMCD
     C*
     C     CTDSLV        IFEQ      *ZERO
     C                   MOVE      QQUSER        KKUSER
     C*** Begin Chg *** 06/07/17 *******************************************
     C*****KKUSER        CHAIN     PUP01                              60
     C     KKUSER        CHAIN     PSL00                              60
     C*** End   Chg *** 06/07/17 *******************************************
     C     *IN60         IFEQ      *OFF
     C*** Begin Chg *** 06/07/17 *******************************************
     C**********         MOVE      UPLVL         CMSECL
     C                   MOVE      SLSECL        CMSECL
     C*** End   Chg *** 06/07/17 *******************************************
     C                   ENDIF                                                  *IN60=*OFF
     C                   ELSE
     C                   MOVE      CTDSLV        CMSECL
     C                   ENDIF                                                  CTDSLV=0
     C                   ENDIF                                                  QQSECL=0
     C                   ENDIF                                                  QQSBYL NE Y
     C                   ELSE
     C                   MOVE      QQSBYL        CMSBYL
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     CMSECL        IFEQ      *ZERO
     C                   MOVE      'N'           CMSBYL
     C                   ELSE
     C                   MOVE      'Y'           CMSBYL
     C                   END
     C                   ENDIF                                                  Q1PRMS>15
     C*
     C                   MOVEL(P)  'A'           CMSTAT
     C                   MOVEL(P)  QQUSER        CMUSER
     C                   MOVEL(P)  QQPGM         CMPGM
     C                   Z-ADD     DSTIME        CMTIME
     C                   Z-ADD     XXDT          CMDT
     C                   MOVEL(P)  Q1USER        CMIUSR
     C                   MOVEL(P)  QQFOLD        TFOLD
     C     TEST          IFEQ      'PDS/'
     C                   MOVEL(P)  FOLDER        CMFOLD
     C                   ELSE
     C                   MOVEL(P)  QQFOLD        CMFOLD
     C                   ENDIF
     C                   MOVE      *ZERO         CMDCDT                         COMPLETE DATE
     C                   MOVEL(P)  QQDIFL        CMDIFL                         DIARY FLAG
     C     CMDIFL        IFEQ      *BLANKS
     C     CTFL01        IFNE      *BLANKS
     C                   MOVE      CTFL01        CMDIFL
     C                   ELSE
     C                   MOVE      'N'           CMDIFL
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE      *BLANK        CMAPID                         IMAGE APP PROD
     C                   MOVE      *BLANK        CMIDOC                         IMG DOC ID
     C                   MOVE      *BLANK        CMIQUE                         IMG QUEUE
     C                   MOVE      *BLANK        CMIMGF                         IMG RMK FLAG
     C                   MOVE      *BLANK        CMIPTH                         IMG PATH ID
     C                   MOVE      *ZERO         CMPIDX                         IMG PATH INDEX
     C                   MOVE      *BLANK        CMQPTY                         IMG QUEUE PTY
     C                   MOVE      *BLANK        CMDCTP                         IMG DOC TYPE
     C     Q1PRMS        IFGE      17
     C                   MOVE      QQCOM2        CMCOMM
     C                   MOVE      QQAPID        CMAPID                         IMAGE APP PROD
     C                   MOVE      QQIDOC        CMIDOC                         IMG DOC ID
     C                   MOVE      QQIQUE        CMIQUE                         IMG QUEUE
     C                   MOVE      QQIMGF        CMIMGF                         IMG RMK FLAG
     C                   MOVE      QQIPTH        CMIPTH                         IMG PATH ID
     C                   MOVE      QQPIDX        CMPIDX                         IMG PATH INDEX
     C                   MOVE      QQQPTY        CMQPTY                         IMG QUEUE PTY
     C                   MOVE      QQDCTP        CMDCTP                         IMG DOC TYPE
     C                   ENDIF                                                  PRMS >= 17
      *
     C                   MOVEL(P)  QQIPGM        CMIPGM                         INIT PGM
     C                   MOVEL(P)  CTSMCD        CMSMCD                         SUMMARY CODE
      *
     C                   MOVEL(P)  QQFL01        CMFL01                         DTA ENTRY INTFACE
     C     CMFL01        IFEQ      *BLANKS
     C                   SELECT
     C     CMDIFL        WHENEQ    'Y'
     C     CTDEPG        ANDNE     *BLANKS
     C                   MOVE      'Y'           CMFL01
     C     CMDCID        IFEQ      *BLANKS
     C                   MOVEL(P)  CTDEPG        CMDCID
     C***  Begin Add  ***  08/09/13  **************************************
      /Free
       isUseDataEntPgm = *on;
      /End-free
     C***  End   Add  ***  08/09/13  **************************************
     C                   ENDIF
     C                   ENDSL
     C                   ENDIF
     C                   MOVEL(P)  QQFL02        CMFL02                         PURGABLE ITEM
     C                   MOVEL(P)  CTFL02        CMFL03                         DIARY CATEGORY
     C                   MOVE      *BLANK        CMFL04                         HILITE DIARY ITEM
     C                   MOVE      *BLANK        CMFL05                         OPT STORAGE LOC
     C                   MOVEL(P)  QQRMCD        CMRMCD                         REMARK CODE
     C                   Z-ADD     XXDT          CMIDT
     C     Q1PRMS        IFGT      27
     C                   MOVE      QQTRDT        XXTRDT            7 0
     C                   MOVE      QQIDT         XXIDT             7 0
     C*
     C     XXTRDT        IFNE      *ZEROS
     C     XXIDT         ORNE      *ZEROS
     C     QQIUSR        ORNE      *BLANKS
     C*
     C                   MOVE      QQTRDT        CMTRDT
     C                   MOVE      QQTRTM        CMTRTM
     C                   MOVEL(P)  QQSTAT        CMSTAT
     C                   MOVEL(P)  QQIUSR        CMIUSR
     C                   MOVE      QQDCDT        CMDCDT
     C                   MOVEL(P)  QQFL03        CMFL03
     C                   MOVEL(P)  QQFL04        CMFL04
     C                   MOVEL(P)  QQFL05        CMFL05
     C                   MOVE      QQIDT         CMIDT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
      *
      * Filter out characters that can not be displayed on the AS400
     C                   CLEAR                   CM
     C                   MOVEA     CMCOMM        CM
      *
     C                   DO        275           X                 3 0
     C     CM(X)         IFLT      X'40'
      *
     C     CM(X)         IFNE      X'08'
     C     CM(X)         ANDNE     X'09'
     C                   MOVE      X'40'         CM(X)
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     CM(X)         IFGT      X'F9'
     C                   MOVE      X'40'         CM(X)
     C                   ENDIF
     C                   ENDDO
     C                   MOVEA     CM            CMCOMM
     C     *IN99         DOUEQ     *OFF
     C                   Z-ADD     *HIVAL        XXSEQ#
     C     C2KY          SETGT     PCM0I
     C     QQKEY         READPE    PCM0I                                  60
     C     *IN60         IFEQ      *OFF
     C     C2SEQ#        ADD       1             CMSEQ#
     C                   ELSE
     C                   Z-ADD     1             CMSEQ#
     C                   END
      *
     C     Q1PRMS        IFGT      36
     C     ##DEIF        IFEQ      'Y'
     C     QQDPGM        ANDNE     '*NO'
     C                   EXSR      WRTAIP
     C                   ENDIF
     C                   ENDIF
      * Get Root
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
      * Find IFS or QDLS
     C                   CALL      RPGM1
     C                   PARM                    ENV               1
     C*
     C***  Begin Add  ***  08/09/13  **************************************
      /Free
       If %parms >= 42;
          Select;
          When @@Env = 'I' or @@Env = 'Q';
             env = @@Env;
          Endsl;
       Endif;
      /End-free
     C***  End   Add  ***  08/09/13  **************************************
     C*
     C* CHECK IF PROCESSING A DOC
     C     '.'           SCAN      CMDCID        FNDDOC            2 0    60
     C  N60'.'           SCAN      CMIDOC        FNDDOC            2 0    60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     1             FNDDOC
     C                   ELSE
     C                   Z-ADD     0             FNDDOC
     C                   ENDIF
     C                   Call      'SA030R'
     C                   Parm      *BLANKS       QQSUFX            3
     C                   Parm      *BLANKS       QQFLRP           80
      *
     C                   Movel     *BLANKS       XXFLRP           81
     C                   If        QQFLRP <> *BLANKS
     C                   Eval      XXFLRP = '/' + %trim(QQFLRP)
     C                   EndIf
      *
     C     Q1PRMS        IFGT      40
      * If the path is not blanks and the document and folder passed are blanks
     C*** BEGIN CHG *** 04/02/13 *******************************************
     C**** QQPATH        IFNE      ' '
     C**** QQDCID        ANDEQ     ' '
     C**** QQFOLD        ANDEQ     ' '
     C***  Begin Chg  ***  08/09/13  **************************************
     C**********         If        qqPath<>' ' And qqDcId=' ' And
     C**********                   (qqFold=' ' Or qqFold=qqKey)
      /Free
       If qqPath <> *blanks;
      /End-free
     C***  End   Chg  ***  08/09/13  **************************************
     C*** END   CHG *** 04/02/13 *******************************************
      *
      * This step will get you the position of last '/' in the path.
     C     AL(1)         CHECKR    QQPATH        SST               5 0
      *
     C     QQRPYN        IFEQ      'Y'
     C     '/'           CAT       ROOT          CMUNCN
     C     CMUNCN        CAT       XXFLRP:0      CMUNCN
     C     CMUNCN        CAT       '/':0         CMUNCN
     C     CMUNCN        CAT       QQPATH:0      CMUNCN
     C                   ELSE
     C                   MOVEL     QQPATH        CMUNCN
     C                   ENDIF                                                  QQRPYN IFEQ 'Y'
     C*** Begin Add *** 02/13/23 **************************************
       If %Parms() >= 43;
          If @@uncn <> ' ';
             CMuncn = @@uncn;
          EndIf;
       EndIf;
     C*** End   Add *** 02/13/23 **************************************
     C                   MOVEL     ENV           CMIFSP
      *
     C***  Begin Add  ***  08/09/13  **************************************
      /Free
       If (qqDcId = *blanks or isUseDataEntPgm)
        and (qqFold=' ' Or qqFold=qqKey);
      /End-free
     C***  End   Add  ***  08/09/13  **************************************
      * Subtract 1 to get the position of the character before last '/'.
     C                   SUB       1             SST
     C     SST           SUBST     QQPATH:1      CMFOLD
      *
      * Add 2 to get the position of the character after last '/'.
     C                   ADD       2             SST
     C                   SUBST     QQPATH:SST    CMDCID
     C***  Begin Add  ***  08/09/13  **************************************
      /Free
       Endif;
      /End-free
     C***  End   Add  ***  08/09/13  **************************************
      *
     C                   ENDIF                                                  QQPATH IFNE ' '
      *
     C                   ENDIF                                                  Q1PRMS IFGT 40
      * Get Root
      * Find IFS or QDLS
      *
     C     CMUNCN        IFEQ      *BLANKS
     C     FNDDOC        ANDEQ     1
     C     CMFOLD        IFNE      ' '
     C     CMDCID        ORNE      ' '
     C                   MOVEL     '/'           CMUNCN
     C     CMUNCN        CAT       ROOT:0        CMUNCN
     C     CMUNCN        CAT       XXFLRP:0      CMUNCN
     C     CMUNCN        CAT       '/':0         CMUNCN
     C     CMUNCN        CAT       CMFOLD:0      CMUNCN
     C     CMUNCN        CAT       '/':0         CMUNCN
     C     CMUNCN        CAT       CMDCID:0      CMUNCN
     C                   MOVE      ENV           CMIFSP
     C                   ENDIF
     C                   ENDIF
     C                   WRITE     PCM01                                99
      ***  Begin Add  ***  08/27/13  **************************************
      /free
        //*** begin chg ***  04/02/15  ***********************************
        //*********If flagDATAQ <> ' ';
        If flagDATAQ <> ' ' and excludeRec <> 'Y';
        //*** end chg *****  04/02/15  ***********************************
           clear pData;
           pKeyID = flagDATAQ;
           pData = flagDATAQ + cmKEY +
                   %editc(CMseq#:'X') + CMdcid +CMfold + CMuncn;
           //  beg add ***** 04/01/14 ************************************
               pDataqLib = 'PDSOBJ' + QQSUFX;
           //  end add ***** 04/01/14 ************************************
           callp SendToDtaq(pDataq:pDataqLib:pEntLength:pData:pKeyLen:
            pKeyID);
           intCount=  intCount+ 1;
        Endif;
      /end-free
      ***  End   Add  ***  08/27/13  **************************************
     C                   ENDDO
     C                   FEOD      SDCMP
     C*
     C                   MOVE      CMSEQ#        QQSEQ#                         RETURN PARM
      *
     C     INACT         TAG
     C                   SELECT
     C     MULTI         WHENEQ    *ON
     C     CTKEY         READE     PDR01                                  60
     C                   OTHER
     C                   MOVE      *ON           *IN60
     C                   ENDSL
     C                   ENDDO
      ***  Begin Add  ***  08/27/13  **************************************
      /free
        If flagDATAQ <> ' ';
           //move numeric value into a character string
           strChr = %editC(intCount:'X');
           //move character string at last position 46-50 of flagDATAQ
           flagDATAQ = %trim(flagDATAQ);
           %SubSt(flagDATAQ:46:5) = strChr;
           Out flagDATAQ;
        Endif;
      /end-free
      ***  End   Add  ***  08/27/13  **************************************
     C                   RETURN
     C*================================================================
     C* WRTAIP - WRITE SDAIP RECORD
     C*================================================================
     C     WRTAIP        BEGSR
     C*
     C                   CLEAR                   PAI01
     C*
     C                   MOVEL(P)  CMKEY         AIKEY
     C                   Z-ADD     CMSEQ#        AISEQ#
     C*
     C                   MOVEL(P)  QQVCFN        AIVCFN
     C                   MOVEL(P)  QQDPGM        AIDPGM
     C                   MOVEL(P)  QQDKEY        AIDKEY
     C*
     C                   TIME                    DSDTTM
     C*
     *** CHANGE 01/25/07 - USER CMUSER FOR USER DATA **********************
     C                   MOVEL(P)  CMUSER        AIUSER
     C                   MOVEL(P)  Q1PGM         AIPGM
     C                   Z-ADD     DSTIME        AITIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AIDT
     C*
     C                   MOVEL(P)  CMIUSR        AIIUSR
     C                   MOVEL(P)  Q1PGM         AIIPGM
     C                   Z-ADD     DSTIME        AIITIM
     C                   Z-ADD     AIDT          AIIDT
     C*
     C                   WRITE     PAI01
     C*
     C                   ENDSR
**
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz .0123456789                 /* 11/02/10 */
