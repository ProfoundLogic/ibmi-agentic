     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H* ***************************************************************
     H/TITLE F4112R - PRINT CLAIM DEPT LETTER & MAINTAIN DARTS
     H* ***************************************************************
     H* PROGRAM DESCRIPTION                                           *
     H*                                                               *
     H* This program reads file FDD1P created in the prior step.  It  *
     H* then accesses other files to assign Fund, Employee, Claim     *
     H* data to the FDD1P record and updates the file prior to the    *
     H* PRTDOC command.  The program also writes a DARTS remark       *
     H* which points to the Claim letter that was generated.          *
     H* Also prior to the PRTDOC command, the document ID is checked  *
     H* to determine if an overlay is necessary and if so, an         *
     H* OVRPRTF command is executed.                                  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     F4111C                                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     *NONE                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:62                                             *
     H*                                                               *
     H*  CREATED:    05/20/98               PROJECT: MC177            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/22/07 C:33  Project: 114WMF (SY059)                        *
     H*                Back to base                                   *
     H*       06/08/05 Project: 114IGM                                *
     H*                DTM changes.                                   *
     H*                                                               *
     H*  07/01/04 C:62 PROJECT: SY100                                 *
     H*                DATA CENTER LOGIC                              *
     H*                                                               *
     H* 01/30/04 C:136 Project: SY059                                 *
     H*                Replace SYCHGDOC with SYCHGDA for DTM.         *
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H* 03/05/01 C:62  Project: 114MAZ                                *
     H*                ADDED LOAD OF D1OCDS - CLAIMANT OCCUPATION     *
     H*                                                               *
     H* 03/02/01 C:69  Project: SY033                                 *
     H*                Change OV commands to use external APIs        *
     H*                                                               *
     H* 02/23/01 C:062 PROJECT: MC184                                 *
     H*                CORRECT PROCESSING OF CANM                     *
     H*                                                               *
     H* 02/21/01 C:062 PROJECT: MC184                                 *
     H*                ADD ADDITIONAL PROVIDER PROCESSING             *
     H*                                                               *
     H* 02/20/01 C:062 PROJECT: MCMNT                                 *
     H*                CHANGE THE ORDER OF THE NAME.                  *
     H*                                                               *
     H* 04/03/00 C:118 PROJECT: WC854                                 *
     H*                LOAD FMCMP/FMIMP INTO FDD1P1/D1IMP             *
     H*                                                               *
     H* 02/24/00 C:62 PROJECT: 48MEM/MEN                              *
     H*               -ADD CALL TO WA042R, TO OBTAIN WCMS OFFICE INFO *
     H*               -POPULATE FDD1PS OFFICE INFO FIELDS             *
     H*                                                               *
     H* 05/26/99 C:62 PROJECT: MC145                                  *
     H*               LOAD DIDT IFNE *ZEROS.                          *
     H*                                                               *
     H* 05/04/99 C:62 PROJECT: DR025 17872                            *
     H*               CHANGE PARM OF FA703R FROM ##WDCK TO ##WCDK     *
     H*                                                               *
     H*****************************************************************
     FFDD1L     UF   E           K DISK
     FFDD1L1    UF   E           K DISK
     F                                     RENAME(PD101:PD102)
     FFADAL1    IF   E           K DISK
     FFMC1L     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCMP     IF   E             DISK
     FFMDIL     IF   E           K DISK
     FFMEML     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMSFL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFTABL     IF   E           K DISK
     F                                     IGNORE(PCS01)
     FFTGTL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMDOL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK

     D*
     D CMD             S              1    DIM(225) CTDATA PERRCD(75)           QCMDEXC COMMAND
     D CM2             S              1    DIM(225) CTDATA PERRCD(75)           QCMDEXC COMMAND
     D OVR             S              1    DIM(75) CTDATA PERRCD(75)            OVRPRTF COMMAND
     D FLR             S              1    DIM(80)                              FOLDER ARRAY
     D CNB             S              1    DIM(40) ASCEND                       COMPANY NAME W/ BLAN
     D CN              S              1    DIM(40)                              COMPANY NAME
     D FNB             S              1    DIM(40) ASCEND                       FUND NAME W/ BLANKS
     D FN              S              1    DIM(40)                              FUND NAME
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D ED              S              8  0 DIM(5) ASCEND                        EFFECT. DATE
     D CD              S              8  0 DIM(5) ASCEND                        CANCEL DATE
     D DEN             S             60    DIM(16)                              DENIAL INFO
     D DT              S              8  0 DIM(16)                              WL/TP DT RANGES
     D*** BEGIN ADD *** 11/24/98 **************************************
     D*** BEGIN CHG *** 02/21/01 **************************************
     D**********          FL          5  1               RECIPIENT FLAGS
     D*** END   CHG *** 02/21/01 **************************************
     D*** END   ADD *** 11/24/98 **************************************
     D***  Begin Add  ***  06/08/05  **************************************
     D JOBQ            C                   CONST('JOBQ(*NO)')
     D*** END   ADD *** 02/23/01 **************************************
     D*** BEGIN ADD *** 07/01/04 **************************************
     D                 DS
     D  TFOLD                  1     80
     D  TEST                   1      4
     D  FOLDER                 9     80
     D*** END   ADD *** 07/01/04 **************************************
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  CM                     1    256
     D                                     DIM(256)                             DARTS COMMENTS
     D  ##COMM                 1    256
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  CX                     1     30
     D  XXCLEX                 1     30
     D                 DS
     D  XXSS#                  1      9
     D  CFSS1                  1      3
     D  CFSS2                  4      5
     D  CFSS3                  6      9
     D                 DS
     D  AR                     1     75
     D                                     DIM(75)                              ADDT'L REMARKS
     D  ARCM75                 1     75
     D                 DS
     D  XXBLNK                 1      3
     D  XXDG1                  1      1
     D  XXDG2                  2      2
     D  XXDG3                  3      3
     D                 DS
     D  DC                     1     12
     D                                     DIM(12)                              CLDCID
     D  CLDCID                 1     12
     D*** BEGIN ADD *** 11/24/98 **************************************
     D                 DS
     D*** BEGIN CHG *** 02/21/01 **************************************
     D**********                              1   5 FL
     D**********                              1   1 D1RCFL
     D**********                              2   2 D1RSFL
     D**********                              3   3 D1RGFL
     D**********                              4   4 D1REFL
     D**********                              5   5 D1ROFL
     D  FL                     1      6
     D                                     DIM(6)                               RECIPIENT FLAGS
     D  D1RCFL                 1      1
     D  D1RSFL                 2      2
     D  D1RGFL                 3      3
     D  D1REFL                 4      4
     D  D1ROFL                 5      5
     D  D1RPFL                 6      6
     D*** END   CHG *** 02/21/01 **************************************
     D*** END   ADD *** 11/24/98 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I***  End   Add  ***  06/08/05  **************************************
     I*** BEGIN ADD *** 02/23/01 **************************************
     IPD102
     I              D1CANM                      D2CANM

     C*
     C*  COMPANY AUXILIARY FILE
     C*
     C     C1KY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  DARTS FILE
     C*
     C     C3KY          KLIST
     C                   KFLD                    #1KEY
     C                   KFLD                    KKSEQ#            5 0
     C*
     C     C3KY2         KLIST
     C                   KFLD                    #1KEY
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C*  COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  DEPT LETTERS WORKFILE - SCREEN
     C*
     C     D1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKREQ#
     C*
     C     D1KY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKREQ#
     C                   KFLD                    KKDPCD
     C                   KFLD                    KKFLDR
     C                   KFLD                    KKDCID
     C*
     C*  DIAGNOSIS CODE INFO FILE
     C*
     C     DAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  DIAGNOSIS CODE FILE
     C*
     C     DIKY          KLIST
     C                   KFLD                    DAICD9
     C                   KFLD                    DALOST
     C*
     C*  DEPT/DOC MASTER FILE
     C*
     C     DOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    D1DPCD
     C*** BEGIN CHG *** 07/01/04 **************************************
     C**********           KFLD           D1FLDR
     C                   KFLD                    K1FLDR
     C*** END   CHG *** 07/01/04 **************************************
     C                   KFLD                    D1DCID
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C*   KEY LIST FOR PROVIDER AUXILIARY FILE.
     C*
     C     P1KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C*   KEY LIST FOR PROVIDER MASTER FILE.
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C*  SUBSCRIBER STAFF CODE FILE
     C*
     C     SFKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKSTAF
     C*
     C*  SUBSCRIBER FILE
     C*
     C     SMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C*  KEY LIST FOR SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    ##KEY             2
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C*  USER PARM FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    KKSTAF
     C*
     C     *LIKE         DEFINE    D1CO#         KKCO#
     C     *LIKE         DEFINE    D1SUB#        KKSUB#
     C     *LIKE         DEFINE    D1FND         KKFND
     C     *LIKE         DEFINE    D1FYR         KKFYR
     C     *LIKE         DEFINE    D1CASE        KKCASE
     C     *LIKE         DEFINE    D1DPCD        KKDPCD
     C     *LIKE         DEFINE    D1FLDR        KKFLDR
     C*** BEGIN ADD *** 07/01/04 **************************************
     C     *LIKE         DEFINE    D1FLDR        K1FLDR
     C*** BEGIN ADD *** 07/01/04 **************************************
     C     *LIKE         DEFINE    D1DCID        KKDCID
     C     *LIKE         DEFINE    D1SS#         KKSS#
     C     *LIKE         DEFINE    D1REQ#        KKREQ#
     C     *LIKE         DEFINE    CMEMP#        KKEMP#
     C     *LIKE         DEFINE    CMDIV         KKDIV
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    UPSTAF        KKSTAF
     C     *LIKE         DEFINE    DODEPT        KKDEPT
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    CMDT          KKDATE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN            15 5
     C                   PARM                    @@REQ#           15 5
     C                   PARM                    @@DATE            8
     C*
     C                   Z-ADD     @@RRN         ##RRN             9 0
     C                   Z-ADD     @@REQ#        ##REQ#            5 0
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** BEGIN ADD *** 02/24/00 **************************************
     C*
     C**  GET SYSTEM INFO
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
     C*** END   ADD *** 02/24/00 **************************************
     C*
     C     ##RRN         CHAIN     PCM01                              99
     C*
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C                   MOVE      ##REQ#        KKREQ#
     C*
     C                   MOVE      @@DATE        SRDTI                          MDCY --> CYMD
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKDATE
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     *HIVAL        KKSEQ#
     C                   MOVE      'CS'          KKDEPT
     C*
     C*   READ THE WORK FILE
     C*
     C     D1KY          CHAIN     PD101                              99
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C                   EXSR      UPDATE
     C*** BEGIN ADD *** 11/24/98 **************************************
     C                   Z-ADD     1             CON               2 0
     C*
     C*** BEGIN CHG *** 02/21/01 **************************************
     C********** CON       DOWLE5
     C     CON           DOWLE     6
     C*** END   CHG *** 02/21/01 **************************************
     C*
     C     FL(CON)       IFEQ      '1'
     C*
     C                   EXSR      RECIP
     C*** END   ADD *** 11/24/98 **************************************
     C                   EXSR      PRTDOC
     C*** BEGIN ADD *** 11/24/98 **************************************
     C                   EXSR      SRDART
     C*
     C                   ENDIF
     C*
     C                   ADD       1             CON
     C*
     C                   ENDDO
     C*** END   ADD *** 11/24/98 **************************************
     C*
     C*  READ NEXT LETTER REQUEST RECORD
     C*
     C     D1KY          READE     PD101                                  99
     C*
     C                   ENDDO                                                  *IN99
     C*
     C*  END OF PROGRAM
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C*   INFCLM - GET CLAIM INFORMATION                              *
     C*****************************************************************
     C     INFCLM        BEGSR
     C*
     C*  GET CLAIM DEMOGRAPHIC INFORMATION (FMCDP).
     C*
     C                   MOVEL     CMSS#         KKSS#
     C*
     C     CDKY          CHAIN     PCD01                              98
     C*
     C     *IN98         IFEQ      '0'
     C*
     C                   MOVEL     CDSS#         D1SS#
     C*
     C*** BEGIN CHG *** 02/20/01 **************************************
     C**********           MOVELCDLSN     ##LAST    P
     C**********           MOVELCDFSN     ##FRST    P
     C*
     C**********           CALL 'SRNAME'
     C**********           PARM           ##LAST 56
     C**********           PARM           ##FRST 15
     C**********           PARM '1'       ##TERM  1
     C*
     C**********           MOVEL##LAST    D1NAME    P
     C     CDFSN         CAT(P)    CDLSN:1       D1NAME
     C*** END   CHG *** 02/20/01 **************************************
     C*
     C                   MOVEL(P)  CDADR1        D1ADR1
     C                   MOVEL(P)  CDADR2        D1ADR2
     C*
     C                   MOVE      CDST          XXST
     C                   Z-ADD     CDZIP         XXZIP
     C                   MOVEL     CDCTY         XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C*** BEG ADD *** 10/20/03 *****************************
     C     CDCTRY        IFNE      *BLANKS
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDADR3        XXCSZ
     C                   MOVE      CDCTRY        XXCSZ
     C                   END
     C*** END ADD *** 10/20/03 *****************************
     C*
     C                   MOVEL(P)  XXCSZ         D1CSZ
     C*
     C*** BEGIN CHG *** 02/20/01 **************************************
     C**********           Z-ADDCDDTOB    D1DTOB
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         D1DTOB
     C*** END   CHG *** 02/20/01 **************************************
     C*
     C                   Z-ADD     CDAREA        D1AREA
     C                   Z-ADD     CDPHN#        D1PHN#
     C                   Z-ADD     CDWARA        D1WARA
     C                   Z-ADD     CDWPH#        D1WPH#
     C*
     C                   MOVEL     CDMSTS        D1MSTS
     C                   MOVEL(P)  'MSTS'        KKTBID
     C                   MOVEL(P)  CDMSTS        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              98
     C*
     C     *IN98         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C*
     C                   MOVEL(P)  GTDESC        D1MSTD
     C*
     C                   ENDIF                                                  GTKY
     C*
     C                   MOVEL(P)  CDSEX         D1SEX
     C                   MOVEL(P)  'GNDR'        KKTBID
     C                   MOVEL(P)  CDSEX         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              98
     C*
     C     *IN98         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C*
     C                   MOVEL(P)  GTDESC        D1SEXD
     C*
     C                   ENDIF                                                  GTKY
     C*
     C                   MOVEL(P)  CMCNTY        D1CNTY
     C     CDST          CAT(P)    CMCNTY:0      KKKEY
     C*
     C     TBKY          CHAIN     PCC01                              98
     C*
     C     *IN98         IFEQ      *OFF
     C     CCSTAT        ANDEQ     'A'
     C*
     C                   MOVEL(P)  CCDESC        D1CNTD
     C*
     C                   ENDIF                                                  TBKY
     C*
     C                   ENDIF                                                  CDKY
     C*
     C* GET CLAIM MASTER INFORMATION (FMCMP).
     C                   MOVEL     CMSCL#        D1SCL#
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         D1ACDT
     C*
     C                   Z-ADD     CMACTM        D1ACTM
     C*
     C                   MOVEL(P)  CMSTAT        D1CSTS
     C                   MOVEL(P)  CMACDS        D1ACDS
     C*** BEGIN ADD *** 03/05/01 **************************************
     C                   MOVEL(P)  CMOCDS        D1OCDS
     C*** END   ADD *** 03/05/01 **************************************
     C                   MOVEL(P)  CMACD2        D1ACD2
     C                   MOVEL(P)  CMMCA         D1MCA
     C                   MOVEL(P)  CMLTFL        D1LTFL
     C                   MOVEL(P)  CMCVFL        D1CVRT
     C                   MOVEL(P)  CMCCL#        D1CCL#
     C*
     C                   Z-ADD     CMMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         D1MMID
     C*
     C                   MOVEL(P)  CMMMIP        D1MMIP
     C***** BEGIN ADD **** 04/03/00 ***********************************
     C                   MOVEL(P)  CMIMP         D1IMP
     C***** END   ADD **** 04/03/00 ***********************************
     C                   MOVEL(P)  CMCMNU        D1CMNU
     C                   MOVEL(P)  CMCMNU        KKSTAF
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C  N99              MOVEL(P)  UPUSR         D1NUPR
     C  N99              MOVEL(P)  UPUSNM        D1NUNM
     C   99              MOVEL(P)  *BLANKS       D1NUPR
     C   99              MOVEL(P)  *BLANKS       D1NUNM
     C*
     C                   MOVEL(P)  CMSTF3        D1STF3
     C                   MOVEL(P)  CMSTF3        KKSTAF
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C  N99              MOVEL(P)  UPUSR         D1S3PR
     C  N99              MOVEL(P)  UPUSNM        D1S3NM
     C   99              MOVEL(P)  *BLANKS       D1S3PR
     C   99              MOVEL(P)  *BLANKS       D1S3NM
     C*
     C                   MOVEL(P)  CMCMSU        D1CMSU
     C                   MOVEL(P)  CMCMSU        KKSTAF
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C  N99              MOVEL(P)  UPUSR         D1SUPR
     C  N99              MOVEL(P)  UPUSNM        D1SUNM
     C   99              MOVEL(P)  *BLANKS       D1SUPR
     C   99              MOVEL(P)  *BLANKS       D1SUNM
     C*
     C                   MOVEL(P)  CMCLSU        D1CLSU
     C*
     C     F1OUTA        IFEQ      'Y'
     C*
     C                   MOVEL(P)  CMCLSU        KKSTAF
     C*
     C     SFKY          CHAIN     PSF01                              99
     C*
     C  N99              MOVEL(P)  *BLANKS       D1CSPR
     C  N99              MOVEL(P)  SFNAME        D1CSNM
     C   99              MOVEL(P)  *BLANKS       D1CSPR
     C   99              MOVEL(P)  *BLANKS       D1CSNM
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  CMCLSU        KKSTAF
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C  N99              MOVEL(P)  UPUSR         D1CSPR
     C  N99              MOVEL(P)  UPUSNM        D1CSNM
     C   99              MOVEL(P)  *BLANKS       D1CSPR
     C   99              MOVEL(P)  *BLANKS       D1CSNM
     C*
     C                   ENDIF                                                  F1OUTA
     C*
     C                   MOVEL(P)  CMCLAD        D1CLAD
     C*
     C     F1OUTA        IFEQ      'Y'
     C*
     C                   MOVEL(P)  CMCLAD        KKSTAF
     C*
     C     SFKY          CHAIN     PSF01                              99
     C*
     C  N99              MOVEL(P)  *BLANKS       D1CAPR
     C*** BEGIN CHG *** 02/23/01 **************************************
     C**N99*****           MOVELSFNAME    D1CANM    P
     C  N99              MOVEL(P)  SFNAME        D2CANM
     C*** END   CHG *** 02/23/01 **************************************
     C   99              MOVEL(P)  *BLANKS       D1CAPR
     C*** BEGIN CHG *** 02/23/01 **************************************
     C***99*****           MOVEL*BLANKS   D1CANM    P
     C   99              MOVEL(P)  *BLANKS       D2CANM
     C*** END   CHG *** 02/23/01 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  CMCLAD        KKSTAF
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C  N99              MOVEL(P)  UPUSR         D1CAPR
     C*** BEGIN CHG *** 02/23/01 **************************************
     C**N99*****           MOVELUPUSNM    D1CANM    P
     C  N99              MOVEL(P)  UPUSNM        D2CANM
     C*** END   CHG *** 02/23/01 **************************************
     C   99              MOVEL(P)  *BLANKS       D1CAPR
     C*** BEGIN CHG *** 02/23/01 **************************************
     C***99*****           MOVEL*BLANKS   D1CANM    P
     C   99              MOVEL(P)  *BLANKS       D2CANM
     C*** END   CHG *** 02/23/01 **************************************
     C*
     C                   ENDIF                                                  F1OUTA
     C*
     C                   MOVEL(P)  CMCLFA        D1CLFA
     C*
     C     F1OUTA        IFEQ      'Y'
     C*
     C                   MOVEL(P)  CMCLFA        KKSTAF
     C*
     C     SFKY          CHAIN     PSF01                              99
     C*
     C  N99              MOVEL(P)  *BLANKS       D1CFPR
     C  N99              MOVEL(P)  SFNAME        D1CFNM
     C   99              MOVEL(P)  *BLANKS       D1CFPR
     C   99              MOVEL(P)  *BLANKS       D1CFNM
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  CMCLFA        KKSTAF
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C  N99              MOVEL(P)  UPUSR         D1CFPR
     C  N99              MOVEL(P)  UPUSNM        D1CFNM
     C   99              MOVEL(P)  *BLANKS       D1CFPR
     C   99              MOVEL(P)  *BLANKS       D1CFNM
     C*
     C                   ENDIF                                                  F1OUTA
     C*
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         D1DTRP
     C*
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         D1RCVD
     C*
     C                   Z-ADD     CMENTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         D1ENTD
     C*
     C                   Z-ADD     CMRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         D1DTRE
     C*
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         D1DBDT
     C*
     C                   Z-ADD     CMDEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         D1DEDT
     C*
     C                   MOVEL(P)  CMPREX        D1PREX
     C                   MOVEL(P)  CMODIS        D1ODIS
     C                   Z-ADD     CMYRLX        D1YRLX
     C*
     C                   ENDSR
     C*****************************************************************
     C*   INFCOM - SUBROUTINE TO GET COMPANY INFORMATION              *
     C*****************************************************************
     C     INFCOM        BEGSR
     C*
     C*  GET COMPANY INFORMATION
     C*
     C     COKY          CHAIN     PCO01                              98
     C*
     C     *IN98         IFEQ      '0'
     C*
     C                   MOVEA     CONAME        CNB
     C                   Z-ADD     1             X                 3 0
     C     *BLANK        LOOKUP    CNB(X)                             98
     C*
     C     *IN98         IFEQ      '1'
     C*
     C                   MOVEA     CNB(X)        CN
     C*
     C                   ELSE
     C*
     C                   MOVEA     CONAME        CN
     C*
     C                   ENDIF                                                  *IN98
     C*
     C                   MOVEA     CN            D1CNME
     C*
     C                   MOVEL     COADR1        D1CPA1
     C                   MOVEL     *BLANKS       D1CPA2
     C                   MOVEL     COADR2        D1CPA2
     C*
     C                   MOVE      COST          XXST              2
     C                   Z-ADD     COZIP         XXZIP
     C                   MOVEL     COCTY         XXCTY            25
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL     XXCSZ         D1CPCZ
     C*
     C                   MOVEL(P)  COMAD1        D1CMA1
     C                   MOVEL(P)  COMAD2        D1CMA2
     C*
     C                   MOVE      COMST         XXST              2
     C                   Z-ADD     COMZIP        XXZIP
     C                   MOVEL     COMCTY        XXCTY            25
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         D1CMCZ
     C*
     C                   Z-ADD     COAREA        D1CARA
     C                   Z-ADD     COPHN#        D1CPHN
     C                   Z-ADD     COFARA        D1CFAR
     C                   Z-ADD     COFAX#        D1CFAX
     C                   Z-ADD     COFEIN        D1CFEI
     C*
     C     C1KY          CHAIN     PC101                              98
     C*
     C     *IN98         IFEQ      *OFF
     C*
     C                   MOVEL(P)  C1NAME        D1CANM
     C                   MOVEL(P)  C1ADR1        D1CAA1
     C                   MOVEL(P)  C1ADR2        D1CAA2
     C*
     C                   MOVE      C1ST          XXST
     C                   Z-ADD     C1ZIP         XXZIP
     C                   MOVEL     C1CTY         XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         D1CACZ
     C*
     C                   Z-ADD     C1AREA        D1CAAR
     C                   Z-ADD     C1PHN#        D1CAPH
     C                   Z-ADD     C1FEIN        D1CAFE
     C*
     C                   ENDIF                                                  C1KY
     C*
     C                   ENDIF                                                  COKY
     C*
     C                   ENDSR
     C*****************************************************************
     C*    INFEMP - MOVE EMPLOYER INFORMATION                         *
     C*****************************************************************
     C     INFEMP        BEGSR
     C*
     C*  GET EMPLOYER INFORMATION
     C*
     C                   Z-ADD     CMEMP#        D1EEMP
     C                   Z-ADD     CMDIV         D1EDIV
     C                   Z-ADD     CMEMP#        KKEMP#
     C                   Z-ADD     CMDIV         KKDIV
     C*
     C     EMKY          CHAIN     PEM01                              98
     C*
     C     *IN98         IFEQ      '0'
     C*
     C                   MOVEL(P)  EMNAME        D1ENME
     C                   MOVEL(P)  EMDBA         D1EDBA
     C*
     C*  EMPLOYER MAILING ADDRESS
     C*
     C                   MOVEL(P)  EMMAD1        D1EMA1
     C                   MOVEL(P)  EMMAD2        D1EMA2
     C*
     C                   MOVE      EMMST         XXST
     C                   Z-ADD     EMMZIP        XXZIP
     C                   MOVEL     EMMCTY        XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         D1EMCS
     C*
     C*  ENPLOYER PHYSICAL ADDRESS
     C*
     C                   MOVEL     EMPAD1        D1EPA1
     C                   MOVEL     EMPAD2        D1EPA2
     C*
     C                   MOVE      EMPST         XXST
     C                   Z-ADD     EMPZIP        XXZIP
     C                   MOVEL     EMPCTY        XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL     XXCSZ         D1EPCS
     C*
     C*  EMPLOYER PHONE NUMBERS
     C*
     C                   Z-ADD     EMAREA        D1EARA
     C                   Z-ADD     EMPHN#        D1EPH#
     C                   Z-ADD     EMFARA        D1EFAR
     C                   Z-ADD     EMFAX#        D1EFAX
     C                   Z-ADD     EMFEIN        D1EFID
     C                   MOVEL     EMNOB         D1ENOB
     C*
     C                   ENDIF                                                  *IN98
     C*
     C                   ENDSR
     C*****************************************************************
     C*    INFGRP - MOVE GROUP INFORMATION                            *
     C*****************************************************************
     C     INFGRP        BEGSR
     C*
     C*  GET GROUP INFORMATION
     C*
     C     FNKY          CHAIN     PFN01                              98
     C*
     C     *IN98         IFEQ      '0'
     C*
     C                   MOVEA     FNNAME        FNB
     C                   Z-ADD     1             X                 3 0
     C*
     C     *BLANK        LOOKUP    FNB(X)                             98
     C*
     C     *IN98         IFEQ      '1'
     C*
     C                   MOVEA     FNB(X)        FN
     C*
     C                   ELSE
     C*
     C                   MOVEA     FNNAME        FN
     C*
     C                   ENDIF                                                  *IN98
     C*
     C                   MOVEA     FN            D1FNME
     C                   MOVEL     FNSHNM        D1FSHN
     C*
     C*  FUND MAILING ADDRESS
     C*
     C                   MOVEL     FNMAD1        D1FMA1
     C                   MOVEL     FNMAD2        D1FMA2
     C*
     C                   Z-ADD     FNEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         D1FEFD
     C*
     C                   MOVE      FNMST         XXST
     C                   Z-ADD     FNMZIP        XXZIP
     C                   MOVEL     FNMCTY        XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL     XXCSZ         D1FMCS
     C*
     C*  FUND PHYSICAL ADDRESS
     C*
     C                   MOVEL     FNPAD1        D1FPA1
     C                   MOVEL     FNPAD2        D1FPA2
     C*
     C                   MOVE      FNPST         XXST
     C                   Z-ADD     FNPZIP        XXZIP
     C                   MOVEL     FNPCTY        XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL     XXCSZ         D1FPCS
     C*
     C*  FUND PHONE NUMBERS
     C*
     C                   Z-ADD     FNARA1        D1FAR1
     C                   Z-ADD     FNPH1         D1FPH1
     C                   Z-ADD     FNAREA        D1FAR2
     C                   Z-ADD     FNPH2         D1FPH2
     C                   Z-ADD     FNFARA        D1FARA
     C                   Z-ADD     FNFAX         D1FAX
     C*
     C     F1KY          CHAIN     PF101                              98
     C*
     C     *IN98         IFEQ      *OFF
     C*
     C                   MOVEL     F1ANME        D1ANAM
     C                   MOVEL     F1AAD1        D1FAA1
     C                   MOVEL     F1AAD2        D1FAA2
     C*
     C                   MOVE      F1AST         XXST
     C                   Z-ADD     F1AZIP        XXZIP
     C                   MOVEL     F1ACTY        XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL     XXCSZ         D1FACZ
     C*
     C                   Z-ADD     F1AARA        D1FAAR
     C                   Z-ADD     F1APHN        D1FAPH
     C*
     C                   ENDIF                                                  F1KY
     C*
     C                   ENDIF                                                  FNKY
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 02/28/00 **************************************
     C*****************************************************************
     C*   INFOFF - SUBROUTINE TO GET WCMS OFFICE INFORMATION          *
     C*****************************************************************
     C     INFOFF        BEGSR
     C*
     C*  GET WCMS GROUP NUMBER
     C*
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMSUB#        ##SUB#
     C                   MOVE      CMFND         ##FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##WCF#            3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##FND             3
     C*
     C     ##RTCD        IFNE      'F'
     C*
     C* GET WCMS OFFICE INFORMATION
     C*
     C                   MOVE      ##WCF#        #1WCF#
     C*
     C                   CALL      'WA042R'
     C                   PARM      CMCO#         #2CO#             3 0
     C                   PARM                    #1WCF#            3 0
     C                   PARM      CMFYR         #1FYR             3 0
     C                   PARM      CMCASE        #1CASE            7 0
     C                   PARM      *BLANKS       ##CRNM           40
     C                   PARM      *BLANKS       ##CRA1           40
     C                   PARM      *BLANKS       ##CRA2           40
     C                   PARM      *BLANKS       ##CRCZ           40
     C                   PARM      *ZEROS        ##CRAR            3 0
     C                   PARM      *ZEROS        ##CRPH            7 0
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C*
     C                   MOVEL(P)  ##CRNM        D1CRNM
     C                   MOVEL(P)  ##CRA1        D1CRA1
     C                   MOVEL(P)  ##CRA2        D1CRA2
     C                   MOVEL(P)  ##CRCZ        D1CRCZ
     C                   Z-ADD     ##CRAR        D1CRAR
     C                   Z-ADD     ##CRPH        D1CRPH
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       D1CRNM
     C                   MOVEL(P)  *BLANKS       D1CRA1
     C                   MOVEL(P)  *BLANKS       D1CRA2
     C                   MOVEL(P)  *BLANKS       D1CRCZ
     C                   Z-ADD     *ZEROS        D1CRAR
     C                   Z-ADD     *ZEROS        D1CRPH
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       D1CRNM
     C                   MOVEL(P)  *BLANKS       D1CRA1
     C                   MOVEL(P)  *BLANKS       D1CRA2
     C                   MOVEL(P)  *BLANKS       D1CRCZ
     C                   Z-ADD     *ZEROS        D1CRAR
     C                   Z-ADD     *ZEROS        D1CRPH
     C*
     C                   ENDIF                                                  ##RTCD
     C*
     C                   ENDSR
     C*** END   ADD *** 02/28/00 **************************************
     C*****************************************************************
     C*   INFPDC - GET PRIMARY DIAGNOSIS CODE INFORMATION             *
     C*****************************************************************
     C     INFPDC        BEGSR
     C*
     C* PRIMARY DIAGNOSIS CODE INFORMATION
     C*
     C*
     C                   MOVEL(P)  *BLANKS       D1PDCD
     C                   MOVEL(P)  *BLANKS       D1PDDS
     C*
     C     DAKY          CHAIN     PDA01                              98
     C*
     C     *IN98         DOWEQ     *OFF
     C*
     C     DAPRIM        IFEQ      'Y'
     C*
     C     DIKY          CHAIN     PDI01                              98
     C*
     C                   MOVEL(P)  DIDS1         D1PDCD
     C                   MOVEL(P)  DIDSC1        D1PDDS
     C*
     C                   SETON                                        98
     C*
     C                   ELSE
     C*
     C     DAKY          READE     PDA01                                  98
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*****************************************************************
     C*    INFPRO - MOVE PROVIDER INFORMATION                         *
     C*****************************************************************
     C     INFPRO        BEGSR
     C*
     C* PROVIDER INFORMATION
     C*
     C                   Z-ADD     D1PFEI        KKFEIN
     C                   Z-ADD     D1PMOD        KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              98
     C*
     C     *IN98         IFEQ      *OFF
     C*
     C**********           MOVELPDNAME    D1PMNM    P      DEL 03/24/99
     C                   MOVEL(P)  PDFSN         D1PMFN
     C*
     C*** BEG ADD *** 03/24/99 ****************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        D1PMNM
     C                   MOVEL(P)  PDADR2        D1PMA1
     C                   MOVE      *BLANKS       D1PMA2
     C                   ELSE
     C                   MOVEL(P)  PDNAME        D1PMNM
     C                   MOVEL(P)  PDADR1        D1PMA1
     C                   MOVEL(P)  PDADR2        D1PMA2
     C                   ENDIF                                                  PDDBA1 = 'Y'
     C*** END ADD *** 03/24/99 ****************************************
     C*
     C*** BEG DEL *** 03/24/99 ****************************************
     C********** PDDBA1    IFEQ 'Y'
     C**********
     C**********           MOVELPDADR1    D1PMNM    P
     C**********
     C**********           ELSE
     C**********
     C**********           MOVELPDADR1    D1PMA1    P
     C**********
     C**********           ENDIF
     C**********
     C**********           MOVELPDADR2    D1PMA2    P
     C*** END DEL *** 03/24/99 ****************************************
     C*
     C                   MOVE      PDST          XXST
     C                   Z-ADD     PDZIP         XXZIP
     C                   MOVEL     PDCTY         XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         D1PMCZ
     C*
     C                   MOVE      PDAREA        D1PMAR
     C                   MOVE      PDPHN#        D1PMPH
     C                   MOVE      PDFXAR        D1PMFA
     C                   MOVE      PDFAX#        D1PMFP
     C                   MOVEL(P)  PDSPCD        D1PSPC
     C                   MOVEL(P)  PDSPCD        KKKEY
     C*
     C     TBKY          CHAIN     PPS01                              98
     C*
     C  N99              MOVEL(P)  PSDESC        D1PSPD
     C   99              MOVEL(P)  *BLANKS       D1PSPD
     C*
     C                   MOVEL(P)  PDWCCE        D1PWCC
     C                   MOVEL(P)  PDBWC#        D1PWC#
     C*** BEGIN ADD *** 02/21/01 **************************************
     C     D1PPNM        IFEQ      *BLANKS
     C*** END   ADD *** 02/21/01 **************************************
     C*
     C     P1KY          CHAIN     PP101                              98
     C*
     C     *IN98         IFEQ      *OFF
     C*
     C                   MOVEL(P)  P1NAME        D1PPNM
     C                   MOVEL(P)  P1FSN         D1PPFN
     C                   MOVEL(P)  P1ADR1        D1PPA1
     C                   MOVEL(P)  P1ADR2        D1PPA2
     C*
     C                   MOVE      P1ST          XXST
     C                   Z-ADD     P1ZIP         XXZIP
     C                   MOVEL     P1CTY         XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         D1PPCZ
     C*
     C                   MOVE      P1AREA        D1PPAR
     C                   MOVE      P1PHN#        D1PPPH
     C*
     C                   ENDIF                                                  P1KY
     C*** BEGIN ADD *** 02/21/01 **************************************
     C                   ENDIF
     C*** END   ADD *** 02/21/01 **************************************
     C*
     C                   ENDIF                                                  PDKY
     C*
     C                   ENDSR
     C*****************************************************************
     C*    INFSUB - MOVE SUBSCRIBER INFORMATION                       *
     C*****************************************************************
     C     INFSUB        BEGSR
     C*
     C*  GET SUBSCRIBER INFORMATION
     C*
     C     SMKY          CHAIN     PSM01                              98
     C*
     C     *IN98         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SMNAME        D1SNAM
     C                   MOVEL(P)  SMADR1        D1SMA1
     C                   MOVEL(P)  SMADR2        D1SMA2
     C*
     C                   MOVE      SMST          XXST
     C                   Z-ADD     SMZIP         XXZIP
     C                   MOVEL     SMCTY         XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         D1SMCZ
     C*
     C                   Z-ADD     SMAREA        D1SARA
     C                   Z-ADD     SMPHN#        D1SPHN
     C*
     C                   ENDIF                                                  SMKY
     C*
     C                   ENDSR
     C*****************************************************************
     C*   MVINF1 - MOVE INFORMATION TO FDD1P                          *
     C*****************************************************************
     C     MVINF1        BEGSR
     C*
     C* MOVE COMPANY INFORMATION.
     C*
     C                   EXSR      INFCOM
     C*
     C* MOVE SUBSCRIBER INFORMATION.
     C*
     C                   EXSR      INFSUB
     C*
     C* MOVE GROUP INFORMATION.
     C*
     C                   EXSR      INFGRP
     C*
     C* MOVE EMPLOYER INFORMATION.
     C*
     C                   EXSR      INFEMP
     C*
     C* MOVE PROVIDER INFORMATION.
     C*
     C                   EXSR      INFPRO
     C*** BEGIN ADD *** 02/28/00 **************************************
     C*
     C* MOVE WCMS OFFICE INFORMATION.
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C*
     C                   EXSR      INFOFF
     C*
     C                   ENDIF
     C*** END   ADD *** 02/28/00 **************************************
     C*
     C                   ENDSR
     C*****************************************************************
     C*   MVINF2 - MOVE INFORMATION TO FDD1P1                         *
     C*****************************************************************
     C     MVINF2        BEGSR
     C*
     C* MOVE CLAIM INFORMATION
     C*
     C                   EXSR      INFCLM
     C*
     C* MOVE PRIMARY DIAGNOSIS INFORMATION.
     C*
     C                   EXSR      INFPDC
     C*
     C                   ENDSR
     C*****************************************************************
     C*   PRTDOC - SUBROUTINE TO EXECUTE PRTDOC COMMAND               *
     C*****************************************************************
     C     PRTDOC        BEGSR
     C*
     C*   IS AN OVERLAY FORM NECESSARY
     C*
     C     D1DOVR        IFNE      *BLANKS
     C*
     C                   Z-ADD     32            I
     C                   MOVEA     D1DOVR        OVR(I)
     C                   ADD       12            I
     C                   MOVEA     ')'           OVR(I)
     C                   MOVEA     OVR           CMDSTR
     C                   Z-ADD     75            LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDSTR
     C                   PARM                    LENGTH
     C*
     C                   ENDIF
     C*
     C*   BUILD PRTDOC COMMAND
     C*
     C*** Begin chg *** 03/02/01 **************************************
     C                   MOVEA     D1DCID        CMD(15)                        DOCUMENT
     C*******              MOVEAD1DCID    CMD,13           DOCUMENT
     C*** End   chg *** 03/02/01 **************************************
     C                   MOVE      D1#CPY        QQ#CPY            5
     C*** Begin chg *** 03/02/01 **************************************
     C                   MOVEA     QQ#CPY        CMD(37)                        COPIES
     C                   MOVEA     D1SDOC        CMD(67)                        DARTS DOC
     C*******              MOVEAQQ#CPY    CMD,35           COPIES
     C*******              MOVEAD1SDOC    CMD,65           DARTS DOC
     C*** End   chg *** 03/02/01 **************************************
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     D1FLDR        FLR
     C                   Z-ADD     1             I                 3 0
     C*** Begin chg *** 03/02/01 **************************************
     C                   Z-ADD     86            II                3 0
     C*******              Z-ADD84        II      30
     C*** End   chg *** 03/02/01 **************************************
     C*
     C     FLR(I)        DOWNE     *BLANK
     C*
     C                   MOVEA     FLR(I)        CMD(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C*
     C                   ENDDO
     C*
     C                   MOVEA     ''''          CMD(II)
     C                   ADD       1             II
     C*
     C                   MOVEA     ')'           CMD(II)
     C                   ADD       2             II
     C                   MOVEA     'SAVFLR('     CMD(II)
     C*
     C                   ADD       7             II
     C                   MOVEA     ''''          CMD(II)
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     D1SFLD        FLR
     C                   ADD       1             II
     C                   Z-ADD     1             I                 3 0
     C*
     C     FLR(I)        DOWNE     *BLANK
     C*
     C                   MOVEA     FLR(I)        CMD(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C                   END
     C*
     C                   MOVEA     ''')'         CMD(II)
     C*
     C*   SPECIFY THE SELECTED OUTPUT QUEUE ON THE PRTDOC COMMAND
     C*
     C                   ADD       3             II
     C                   MOVEA     'OUTQ('       CMD(II)
     C                   ADD       5             II
     C                   MOVEA     D1OUTQ        CMD(II)
     C                   ADD       10            II
     C                   MOVEA     ')'           CMD(II)
     C***  Begin Add  ***  06/08/05  **************************************
     C                   ADD       2             II
     C                   MOVEA     JOBQ          CMD(II)
     C***  End   Add  ***  06/08/05  **************************************
     C*
     C                   MOVEA     CMD           CMDSTR          225
     C                   Z-ADD     225           LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDSTR
     C                   PARM                    LENGTH
     C*
     C*   BUILD CHGDOCD COMMAND IF DOALCH NE 'Y'
     C*
     C     DOALCH        IFNE      'Y'
     C*
     C*** Begin chg *** 03/02/01 **************************************
     C                   MOVEA     D1SDOC        CM2(16)                        SAVE DOCUMENT
     C*******              MOVEAD1SDOC    CM2,14           SAVE DOCUMENT
     C*** End   chg *** 03/02/01 **************************************
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     D1SFLD        FLR
     C                   Z-ADD     1             I
     C*** Begin chg *** 03/02/01 **************************************
     C                   Z-ADD     48            II
     C*******              Z-ADD46        II
     C*** End   chg *** 03/02/01 **************************************
     C*
     C     FLR(I)        DOWNE     *BLANK                                       SAVE FOLDER
     C*
     C                   MOVEA     FLR(I)        CM2(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C*
     C                   ENDDO
     C*
     C                   MOVEA     ''''          CM2(II)
     C                   ADD       1             II
     C                   MOVEA     ')'           CM2(II)
     C*
     C                   MOVEA     CM2           CM2STR          225
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    CM2STR
     C                   PARM                    LENGTH
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C*   RECIP - SUBROUTINE TO LOAD THE CITY STATE & ZIP             *
     C*****************************************************************
     C     RECIP         BEGSR
     C*
     C*** GET AREA OF SERVICE FOR CREATION OF SAVE FOLDER/DOCUMENT
     C*
     C*** BEGIN ADD *** 07/01/04 **************************************
     C                   MOVEL(P)  D1FLDR        TEST4             4
     C     TEST4         IFEQ      'PDS/'
     C                   MOVE(P)   D1FLDR        TEST72           72
     C                   MOVEL(P)  TEST72        K1FLDR
     C                   ELSE
     C                   MOVEL(P)  D1FLDR        K1FLDR
     C                   ENDIF
     C*** END   ADD *** 07/01/04 **************************************
     C     DOKY          CHAIN     PDO01                              98
     C*
     C     *IN98         IFEQ      '0'
     C*
     C                   ENDIF
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C*
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       ##FLR            80
     C                   PARM      *BLANKS       ##DCID           12
     C                   PARM      DOPRFX        ##PRE             2
     C                   PARM                    #1CO#             3
     C                   PARM                    #1FND             3
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1SUB#            5
     C*
     C*
     C                   SELECT
     C*
     C     CON           WHENEQ    1
     C*
     C     D1KY2         CHAIN     PD101                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     D1KY2         CHAIN(N)  PD102                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  D1NAME        D1RRN1
     C                   MOVEL(P)  *BLANKS       D1RRN2
     C                   MOVEL(P)  D1ADR1        D1RRA1
     C                   MOVEL(P)  D1ADR2        D1RRA2
     C                   MOVEL(P)  D1CSZ         D1RRCT
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  ##FLR         D1SFLD
     C                   MOVEL(P)  ##DCID        D1SDOC
     C*
     C                   UPDATE    PD101
     C*
     C                   ENDIF
     C*
     C     CON           WHENEQ    2
     C*
     C     D1KY2         CHAIN     PD101                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  D1SNAM        D1RRN1
     C                   MOVEL(P)  D1RSAT        D1RRN2
     C                   MOVEL(P)  D1SMA1        D1RRA1
     C                   MOVEL(P)  D1SMA2        D1RRA2
     C                   MOVEL(P)  D1SMCZ        D1RRCT
     C*
     C                   MOVEL(P)  ##FLR         D1SFLD
     C                   MOVEL(P)  ##DCID        D1SDOC
     C*
     C                   UPDATE    PD101
     C*
     C                   ENDIF
     C*
     C     CON           WHENEQ    3
     C*
     C     D1KY2         CHAIN     PD101                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     F1FLG1        IFEQ      'Y'
     C*
     C                   MOVEL(P)  D1ANAM        D1RRN1
     C                   MOVEL(P)  D1RGAT        D1RRN2
     C                   MOVEL(P)  D1FAA1        D1RRA1
     C                   MOVEL(P)  D1FAA2        D1RRA2
     C                   MOVEL(P)  D1FACZ        D1RRCT
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  D1FNME        D1RRN1
     C                   MOVEL(P)  D1RGAT        D1RRN2
     C                   MOVEL(P)  D1FMA1        D1RRA1
     C                   MOVEL(P)  D1FMA2        D1RRA2
     C                   MOVEL(P)  D1FMCS        D1RRCT
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  ##FLR         D1SFLD
     C                   MOVEL(P)  ##DCID        D1SDOC
     C*
     C                   UPDATE    PD101
     C*
     C                   ENDIF
     C*
     C     CON           WHENEQ    4
     C*
     C     D1KY2         CHAIN     PD101                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     D1EMA1        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  D1ENME        D1RRN1
     C                   MOVEL(P)  D1REAT        D1RRN2
     C                   MOVEL(P)  D1EMA1        D1RRA1
     C                   MOVEL(P)  D1EMA2        D1RRA2
     C                   MOVEL(P)  D1EMCS        D1RRCT
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  D1ENME        D1RRN1
     C                   MOVEL(P)  D1REAT        D1RRN2
     C                   MOVEL(P)  D1EPA1        D1RRA1
     C                   MOVEL(P)  D1EPA2        D1RRA2
     C                   MOVEL(P)  D1EPCS        D1RRCT
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  ##FLR         D1SFLD
     C                   MOVEL(P)  ##DCID        D1SDOC
     C*
     C                   UPDATE    PD101
     C*
     C                   ENDIF
     C*
     C     CON           WHENEQ    5
     C*
     C     D1KY2         CHAIN     PD101                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  D1RONM        D1RRN1
     C                   MOVEL(P)  D1ROAT        D1RRN2
     C                   MOVEL(P)  D1ROA1        D1RRA1
     C                   MOVEL(P)  D1ROA2        D1RRA2
     C*
     C                   MOVE(P)   D1ROST        XXST
     C                   Z-ADD     D1ROZP        XXZIP
     C                   MOVEL(P)  D1ROCT        XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         D1RRCT
     C*
     C                   MOVEL(P)  ##FLR         D1SFLD
     C                   MOVEL(P)  ##DCID        D1SDOC
     C*
     C                   UPDATE    PD101
     C*
     C                   ENDIF
     C*** BEGIN CHG *** 02/21/01 **************************************
     C     CON           WHENEQ    6
     C*
     C     D1KY2         CHAIN     PD101                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     D1RPFE        KKFEIN
     C                   Z-ADD     D1RPMD        KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              98
     C*
     C     *IN98         IFEQ      *OFF
     C*
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        D1RRN1
     C                   MOVEL(P)  PDADR2        D1RRA1
     C                   MOVE      *BLANKS       D1RRA2
     C                   ELSE
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL(P)  PDNAME        D1RRN1
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      D1RRN1
     C                   ENDIF
     C                   MOVEL(P)  PDADR1        D1RRA1
     C                   MOVEL(P)  PDADR2        D1RRA2
     C                   ENDIF                                                  PDDBA1 = 'Y'
     C*
     C                   MOVE      PDST          XXST
     C                   Z-ADD     PDZIP         XXZIP
     C                   MOVEL     PDCTY         XXCTY
     C*
     C                   EXSR      SRADDR
     C*
     C                   MOVEL(P)  XXCSZ         D1RRCT
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  D1RPAT        D1RRN2
     C*
     C                   MOVEL(P)  ##FLR         D1SFLD
     C                   MOVEL(P)  ##DCID        D1SDOC
     C*
     C                   UPDATE    PD101
     C*
     C                   ENDIF
     C*** END   CHG *** 02/21/01 **************************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*****************************************************************
     C*   SRADDR - SUBROUTINE TO LOAD THE CITY STATE & ZIP            *
     C*****************************************************************
     C     SRADDR        BEGSR
     C*
     C                   MOVE      XXZPM1        XXZIP1            5
     C                   MOVE      XXZPM2        XXZIP2            4
     C*
     C     XXCTY         CAT(P)    ',':0         XXCSZ            40
     C                   CAT(P)    XXST:1        XXCSZ
     C                   CAT(P)    XXZIP1:1      XXCSZ
     C*
     C     XXZPM2        IFNE      *ZEROS
     C*
     C                   CAT(P)    '-':0         XXCSZ
     C                   CAT(P)    XXZIP2:0      XXCSZ
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C*   SRDART - WRITE DARTS RECORD                                 *
     C*****************************************************************
     C     SRDART        BEGSR
     C*
     C*  WRITE RECORD TO DARTS
     C*
     C*  ESTABLISH KEY
     C*** BEGIN ADD *** 02/24/00 **************************************
     C*
     C**  GET SYSTEM INFO
     C**********
     C**********           CALL 'SA001R'
     C**********           PARM *BLANK    ##WCIF  1        WC INST
     C**********           PARM *BLANK    ##MCIF  1        MC INST
     C**********           PARM *BLANK    ##WCIN  1        WC INTERFACE
     C**********           PARM *BLANK    ##AR    1        MC AR INST
     C**********           PARM *BLANK    ##NTWK  1        NETWORK INST
     C**********           PARM *BLANK    ##UR    1        PRECERT INST
     C**********           PARM *BLANK    ##MED   1        MEDMGMT INST
     C**********           PARM *BLANK    ##EDI   1        EDI INST
     C**********           PARM *BLANK    ##WCDK  1        WC DARTS KEY
     C*** END   ADD *** 02/24/00 **************************************
     C*
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMSUB#        ##SUB#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C*
     C*  DETERMINE IF THE GROUP IS LINKED OR STANDALONE.
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    ##CO#
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C                   PARM      *BLANKS       ##OUTS            1
     C*
     C                   CALL      'FA703R'
     C                   PARM      'K'           ##RTCD            1
     C                   PARM                    ##WCIN            1
     C*** BEGIN CHG *** 05/04/99 **************************************
     C**********           PARM           ##WDCK  1
     C                   PARM                    ##WCDK            1
     C*** END   CHG *** 05/04/99 **************************************
     C                   PARM                    ##CO#             3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      *BLANKS       #1KEY            40
     C*
     C     ##OUTS        IFEQ      'Y'
     C*
     C                   MOVEL(P)  'F0260R'      ##PNAM                         KEYED PROGRAM
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  'W0070R'      ##PNAM
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 05/26/99 **************************************
     C     D1DIDT        IFEQ      *ZEROS
     C*** END   ADD *** 05/26/99 **************************************
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         ##CDAT
     C*** BEGIN ADD *** 05/26/99 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      D1DIDT        ##CDAT
     C*
     C                   ENDIF
     C*** END   ADD *** 05/26/99 **************************************
     C*
     C     D1DIUS        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  D1DIUS        ##USER
     C                   MOVEL(P)  'Y'           ##DIFL
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  Q1USER        ##USER
     C                   MOVEL(P)  DODIFL        ##DIFL
     C*
     C                   ENDIF
     C*
     C*  ESTABLISH RECORD
     C*
     C                   MOVEA     *BLANKS       CM
     C*
     C                   SELECT
     C*
     C     CON           WHENEQ    1
     C                   MOVEA(P)  'Clm:'        CM(1)
     C*
     C     CON           WHENEQ    2
     C                   MOVEA(P)  'Sub:'        CM(1)
     C*
     C     CON           WHENEQ    3
     C                   MOVEA(P)  'Grp:'        CM(1)
     C*
     C     CON           WHENEQ    4
     C                   MOVEA(P)  'Emp:'        CM(1)
     C*
     C     CON           WHENEQ    5
     C                   MOVEA(P)  'Oth:'        CM(1)
     C*** BEGIN ADD *** 02/21/01 **************************************
     C*
     C     CON           WHENEQ    6
     C                   MOVEA(P)  'Prv:'        CM(1)
     C*** END   ADD *** 02/21/01 **************************************
     C*
     C                   ENDSL
     C*
     C                   CALL      'DA200R'
     C                   PARM      DORMCD        ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'Y'
     C*
     C                   MOVEL(P)  ##DESC        XXRMDS           40
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       XXRMDS
     C*
     C                   ENDIF
     C*
     C                   MOVEA     DODESC        CM(6)
     C*** BEGIN ADD *** 07/01/04 **************************************
     C                   MOVEL(P)  D1SFLD        TFOLD
     C     TEST          IFEQ      'PDS/'
     C                   MOVEL(P)  FOLDER        ##FOLD
     C                   ELSE
     C                   MOVEL(P)  D1SFLD        ##FOLD
     C                   ENDIF
     C*** END   ADD *** 07/01/04 **************************************
     C*
     C                   CALL      'DA700R'
     C                   PARM                    #1KEY            40
     C                   PARM                    ##COMM          256
     C                   PARM                    ##CDAT            8
     C                   PARM      D1SDOC        ##DCID           12
     C                   PARM                    ##PNAM           10
     C                   PARM      '000'         ##SECL            3
     C                   PARM                    ##USER           10
     C                   PARM      'F4112R'      ##PGM            10
     C*** BEGIN CHG *** 07/01/04 **************************************
     C**********           PARM D1SFLD    ##FOLD 80
     C                   PARM                    ##FOLD           80
     C*** END   CHG *** 07/01/04 **************************************
     C                   PARM                    ##DIFL            1
     C                   PARM      'F4112R'      ##IPGM           10
     C                   PARM      'N'           ##FL01            1
     C                   PARM      'N'           ##FL02            1
     C                   PARM      DORMCD        #QRMCD            5
     C                   PARM      '00000'       #QSEQ#            5
     C*
     C                   ENDSR
     C*****************************************************************
     C*   UPDATE - SUBROUTINE TO UPDATE RECORDS                       *
     C*****************************************************************
     C     UPDATE        BEGSR
     C*
     C                   MOVE(P)   D1DPCD        KKDPCD
     C                   MOVE(P)   D1FLDR        KKFLDR
     C                   MOVE(P)   D1DCID        KKDCID
     C*
     C                   EXSR      UPDAT1
     C                   EXSR      UPDAT2
     C*
     C                   ENDSR
     C*****************************************************************
     C*   UPDAT1 - SUBROUTINE TO UPDATE RECORDS IN FDD1P              *
     C*****************************************************************
     C     UPDAT1        BEGSR
     C*
     C*  GET WCMC CLAIM DEPARTMENT FILE 1
     C*
     C     D1KY2         CHAIN     PD101                              99
     C*
     C*  UPDATE CLAIM DEPARTMENT FILE 1
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVINF1
     C*
     C*  WRITE DEPARTMENT LETTERS WORKFILE RECORD FOR PRINTING
     C*
     C                   MOVEL     Q1USER        D1USER
     C                   MOVEL     Q1PGM         D1PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         D1DT
     C*
     C                   Z-ADD     DSTIME        D1TIME
     C*
     C                   UPDATE    PD101
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C*   UPDAT2 - SUBROUTINE TO UPDATE RECORDS OF FDD1P1             *
     C*****************************************************************
     C     UPDAT2        BEGSR
     C*
     C*  GET WCMC CLAIM DEPARTMENT FILE 2
     C*
     C     D1KY2         CHAIN     PD102                              99
     C*
     C*  UPDATE CLAIM DEPARTMENT FILE 2
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVINF2
     C*
     C*  WRITE DEPARTMENT LETTERS WORKFILE RECORD FOR PRINTING
     C*
     C                   MOVEL     Q1USER        D1USER
     C                   MOVEL     Q1PGM         D1PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         D1DT
     C*
     C                   Z-ADD     DSTIME        D1TIME
     C*
     C                   UPDATE    PD102
     C*
     C                   ENDIF
     C*
     C                   ENDSR
**  (CMD) QCMDEXC COMMAND
SYPRTDOC DOC('            ') COPIES(     ) SAVOUTPUT(*YES) SAVDOC(
   ) FLR('

**  (CM2) QCMDEXC COMMAND
SYCHGDA   DOC('            ') ALWRPL(*NO) FLR('


**  (OVR) QCMDEXC COMMAND
OVRPRTF FILE(QSYSPRT) FRONTOVL(
