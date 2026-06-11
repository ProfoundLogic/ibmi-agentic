     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F0572R - ERR MESSAGE DISPLAY FOR ADJUD PGM
     H*****************************************************************
     H*            P R O G R A M M E R    N O T E                     *
     H*                                                               *
     H* WHEN ADDING A MESSAGE/DESCRIPTION TO NUM/MSG REMEMEBER TO ADD *
     H* THE PROGRAM MESSAGE ID/THE MESSAGE FILE ID TO ERID/LVLID.     *
     H*                                                               *
     H* THE MESSAGE FILE TO LOOK FOR NEW MESSAGE IDS IN IS            *
     H* FSFIL3/MCMESG                                                 *
     H*                                                               *
     H* ANY TIME A PROGRAM MESSAGE IS ADDED ALWAYS ADD THE MESSAGE    *
     H* FILE MESSAGE ID TO THE COMPILE TIME ARRAY OR THE OPTION '5'   *
     H* TO VIEW MESSAGE DETAIL WILL NOT PROPERLY WORK                 *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/17/98 C:79   PROJ: NE005
     H*                  1. Added Message #GN
     H*                                                               *
     H*  09/18/92 C:05   PROJECT#: 99AAC                   7          *
     H*                  1.  Change to Subfile Processing             *
     H*                  2.  Only display the messages associated with*
     H*                        THE SELected detail charge record      *
     H*                                                               *
     H*  10/16/92 KRR    PROJ: 99AAD                                  *
     H*                  1. CHANGED TO WINDOW PROCESSING              *
     H*                  2. REMOVED F6 AND F16                        *
     H*                                                               *
     H*  11/06/92 C:20   PROJ: 99AAD                                  *
     H*                  1. ADDED MESSAGES FOR ELEVATED CARE AND      *
     H*                     MEDICAL DEVICE                            *
     H*                                                               *
     H*  11/24/92 C:02   PROJ: 99AAH                                  *
     H*                  1. CHANGE TO ALLOW MULTIPLE MODULES TO DISP. *
     H*                     MESSAGES                                  *
     H*                     MFS = Fee Schedule Messages               *
     H*                     MPC = Pre/Re-Certification Messages       *
     H*                     MCE = MDR Claims Edit Messages            *
     H*                     MCR = C/R Review Messages                 *
     H*                                                               *
     H*  12/04/92 C:20   PROJ: 99AAJ                                  *
     H*                  1. ADDED PRE-CERT ERROR MSGS TO ARRAY @ BTM  *
     H*                                                               *
     H*  02/01/93 C:05   PROJ: FS087                                  *
     H*                  1. Added Message #76                         *
     H*                                                               *
     H*  02/15/93 C:20   PROJ:                                        *
     H*                  1. CHANGED CODE TO ALLOW PRESSING ENTER W/O  *
     H*                     SELECTION TO RETURN TO CALLING PROGRAM    *
     H*                                                               *
     H*  03/02/93 C:05   PROJ: FS092                                  *
     H*                  1. Added Message #78                         *
     H*                                                               *
     H*  03/19/93 C:05   PROJ: FS104                                  *
     H*                  1. Added Message #79                         *
     H*                                                               *
     H*  04/09/93 C:05   PROJ: FS088                                  *
     H*                  1. Added Message #94                         *
     H*                                                               *
     H*  04/13/93 C:05   PROJ: FS089                                  *
     H*                  1. Added Message #80                         *
     H*                  2. Added Message #81                         *
     H*                                                               *
     H*  05/12/93 C:05   PROJ: FS088                                  *
     H*                  1. Added Message #82, 83, 84, 85, 86, 87     *
     H*                                                               *
     H*  05/25/93 C:20   PROJ: GA001                                  *
     H*                  1. Added Message #88                         *
     H*                                                               *
     H*  06/07/93 C:20   PROJ: AL002                                  *
     H*                  1. Added Message #89                         *
     H*                                                               *
     H*  06/08/93 C:20   PROJ: AL003                                  *
     H*                  1. Added Message #90                         *
     H*                                                               *
     H*  06/16/93 C:05   PROJ: LA001                                  *
     H*                  1. Added Message #91                         *
     H*                                                               *
     H*  06/23/93 C:20   PROJ: LA001                                  *
     H*                  1. Added Message #92                         *
     H*                  2. Added Message #93                         *
     H*                  3. Added Message #AA                         *
     H*                  4. Added Message #AB                         *
     H*                                                               *
     H*  07/20/93 C:20   PROJ: AL006                                  *
     H*                  1. Added Message #AC                         *
     H*                                                               *
     H*  08/11/93 C:20   REQ/PRJ: AL007                               *
     H*                  1. Added Message #AD                         *
     H*                                                               *
     H*  08/11/93 C:20   REQ/PRJ: LA006                               *
     H*                  1. Added Message #AE                         *
     H*                  1. Added Message #AF                         *
     H*                                                               *
     H*  09/14/93 C:20   REQ/PRJ: NYMNT                               *
     H*                  1. Added Message #AG                         *
     H*                                                               *
     H*  10/11/93 C:20   REQ/PROJ: MS001                              *
     H*                  1. Added Message #AH                         *
     H*                                                               *
     H*  10/18/93 C:20   REQ/PROJ: MS001                              *
     H*                  1. Added Message #AI, #AJ                    *
     H*                                                               *
     H*  10/19/93 C:20   REQ/PROJ: NE001                              *
     H*                  1. Added Message #AK                         *
     H*                                                               *
     H*  12/21/93 C:20   REQ/PROJ: ALMNT                              *
     H*                  1. Added Message #AL                         *
     H*                                                               *
     H*  12/30/93 C:20   REQ/PROJ: AL013                              *
     H*                  1. Added Message #AM                         *
     H*                                                               *
     H*  01/05/94 C:20   REQ/PROJ: FSMNT                              *
     H*                  1. REMOVE BUILDING OF MESSAGE ID FOR 2ND     *
     H*                     LEVEL PROCESSING.  ADDED A COMPILE TIME   *
     H*                     ARRAY TO PROGRAM TO GET 2ND LEVEL MESSAGE *
     H*                     ID.  THIS WAS DONE BECAUSE THE AA-AL      *
     H*                     MESSAGES COULD NOT BE USED IN THE         *
     H*                     MESSAGE FILE.                             *
     H*                                                               *
     H*  01/06/94 C:20   REQ/PROJ: OKMNT                              *
     H*                  1. Added Message #AN, #AO                    *
     H*                  2. Added 2nd level message ID MFS0113, 114   *
     H*                                                               *
     H*  02/28/94 C:20   REQ/PROJ: MA001                              *
     H*                  1. Added Message #AP, AQ, AR, AS, AT, AU, AV *
     H*                     AW, AX, AY, AZ                            *
     H*                  2. Added 2nd level message ID MFS0115,116,117*
     H*                     118, 119, 120, 121, 122, 123, 124, 125    *
     H*                                                               *
     H*  03/24/94 C:20   REQ/PROJ: OH001                              *
     H*                  1. Added Message #BA, BB                     *
     H*                  2. Added 2nd level message ID MFS0126, 127   *
     H*                                                               *
     H*  05/11/94 C:20   REQ/PROJ: 50051/ma003                        *
     H*                  1. Added Message #BC                         *
     H*                  2. Added 2nd level message ID MFS0128        *
     H*                                                               *
     H*  05/26/94 C:20   REQ/PROJ: FLMNT                              *
     H*                  1. Added Message #BD                         *
     H*                  2. Added 2nd level message ID MFS0129        *
     H*                                                               *
     H*  05/26/94 C:20   REQ/PROJ: KY001                              *
     H*                  1. Added Message #BEM BF                     *
     H*                  2. Added 2nd level message ID MFS0130, 131   *
     H*                                                               *
     H*  09/27/94 C:20   REQ/PROJ: NEMNT                              *
     H*                  1. Added Message #BG                         *
     H*                  2. Added 2nd level message ID MFS0132        *
     H*                                                               *
     H*  09/28/94 C:05   REQ/PROJ: LA008                              *
     H*                  1. Added Message #BH                         *
     H*                  2. Added 2nd level message ID MFS0133        *
     H*                  3. Added Message #BI                         *
     H*                  4. Added 2nd level message ID MFS0134        *
     H*                  5. Added Message #BJ                         *
     H*                  6. Added 2nd level message ID MFS0135        *
     H*                                                               *
     H*  11/22/94 C:05   REQ/PROJ: PA001                              *
     H*                  1. Added Message #BK                         *
     H*                  2. Added 2nd level message ID MFS0136        *
     H*                                                               *
     H*  12/02/94 C:05   REQ/PROJ: KY004                              *
     H*                  1. Added Message #BL                         *
     H*                  2. Added 2nd level message ID MFS0137        *
     H*                  3. Added Message #BM                         *
     H*                  4. Added 2nd level message ID MFS0138        *
     H*                                                               *
     H*  01/24/95 C:20   REQ/PROJ: 139/MA005                          *
     H*                  1. Added Message #BN                         *
     H*                  2. Added 2nd level message ID MFS0139        *
     H*                                                               *
     H*  02/27/95 C:20   REQ/PROJ: 142/MA006                          *
     H*                  1. Added Message #BO                         *
     H*                  2. Added 2nd level message ID MFS0140        *
     H*                                                               *
     H*  03/23/95 C:20   REQ/PROJ: 140/AL020                          *
     H*                  1. Added Message #BP                         *
     H*                  2. Added 2nd level message ID MFS0141        *
     H*                                                               *
     H*  06/22/95 C:20   REQ/PROJ: 1/NM001                            *
     H*                  1. Added Message #BQ                         *
     H*                  2. Added 2nd level message ID MFS0142        *
     H*                                                               *
     H*  07/26/95 C:32   REQ/PROJ: 1/TX001                            *
     H*                  1. Added Messages #BR, #BS                   *
     H*                  2. Added 2nd level message ID MFS0143-MFS144 *
     H*                                                               *
     H*  09/07/95 C:32   REQ/PROJ: 160/AL025                          *
     H*                  1. Added Message #BT                         *
     H*                  2. Added 2nd level message ID MFS0145        *
     H*                                                               *
     H*  11/02/95 C:20   REQ/PROJ: 166/TX003                          *
     H*                  1. Added Message #BU, #BV, #BW, #BX, #BY, #BZ*
     H*                     #CA, #CB, #CC
     H*                  2. Added 2nd level message ID MFS0146-MFS0154*
     H*                                                               *
     H*  12/04/95 C:32   REQ/PROJ: TX003                              *
     H*                  1. Added Message #CD                         *
     H*                                                               *
     H*  12/12/95 C:32   REQ/PROJ: OK003                              *
     H*                  1. Added Message #CE                         *
     H*                                                               *
     H*  12/17/95 C:32   REQ/PROJ: TX003                              *
     H*                  1. Added Message #CF                         *
     H*                                                               *
     H*  12/18/95 C:32   REQ/PROJ: TX001                              *
     H*                  1. Added Message #CG                         *
     H*                                                               *
     H*  12/18/95 C:32   REQ/PROJ: OK003                              *
     H*                  1. Added Messages #CH - #CO                  *
     H*                                                               *
     H*  01/19/96 C:32   REQ/PROJ: OK003                              *
     H*                  Chgd wording on #CM                          *
     H*                                                               *
     H*  02/12/96 C:41   PROJ: 59MEP                                  *
     H*                  1. Added Message #CP                         *
     H*                  2. Added Message #CO                         *
     H*                                                               *
     H*  03/15/96 C:32   PROJ: TX005                                  *
     H*                  1. Added Message #CQ - #DK                   *
     H*                                                               *
     H*  04/11/96 C:32   PROJ: OKMNT                                  *
     H*                  1. Added Message #DL, #DM                    *
     H*                                                               *
     H*  04/22/96 C:32   PROJ: NC001                                  *
     H*                  1. Added Message #DN - #DU                   *
     H*                                                               *
     H*  05/13/96 C:32   PROJ: TX005                                  *
     H*                  1. Added Message #DV - #DW                   *
     H*                                                               *
     H*  05/16/96 C:32   PROJ: RI003                                  *
     H*                  1. Added Message #DX - #DY                   *
     H*                                                               *
     H*  05/23/96 C:32   PROJ: NC002                                  *
     H*                  1. Added Message #DZ                         *
     H*                                                               *
     H*  05/29/96 C:32   PROJ: RI004                                  *
     H*                  1. Added Message #EA - #EC                   *
     H*                                                               *
     H*  06/04/96 C:32   PROJ: OKMNT                                  *
     H*                  1. Added Message #ED                         *
     H*                                                               *
     H*  06/19/96 C:32   PROJ: SC001                                  *
     H*                  1. Added Message #EE - #EO                   *
     H*                                                               *
     H*  07/16/96 C:32   PROJ: AL028                                  *
     H*                  1. Added Message #EP                         *
     H*                                                               *
     H*  07/17/96 C:32   PROJ: GA008                                  *
     H*                  1. Added Message #EQ - #ES                   *
     H*                                                               *
     H*  09/04/96 C:32   PROJ: MA009                                  *
     H*                  1. Added Message #ET - #EU                   *
     H*                                                               *
     H*  09/18/96 C:59   PROJ: KY007                                  *
     H*                  1. Added Message #EV                         *
     H*  10/08/96 C:59   PROJ: KS001                                  *
     H*                  1. Added Message #EW                         *
     H*                  1. Added Message #EX                         *
     H*                  1. Added Message #EY                         *
     H*                  1. Added Message #EZ                         *
     H*                  1. Added Message #FA                         *
     H*                  1. Added Message #FB                         *
     H*                  1. Added Message #FC                         *
     H*                  1. Added Message #FD                         *
     H*  01/10/97 C:59   PROJ: MS004                                  *
     H*                  1. Added Message #FE                         *
     H*
     H*  01/10/97 C:59   PROJ: AL031                                  *
     H*                  1. Added Message #FF
     H*                  1. Added Message #FG
     H*                  1. Added Message #FH                         *
     H*                  1. Added Message #FI                         *
     H*                  1. Added Message #FJ                         *
     H*                  1. Added Message #FK                         *
     H*
     H*  03/04/97 C:59   PROJ: KY008                                  *
     H*                  1. Added Message #FL
     H*                  1. Added Message #FM
     H*
     H*  03/04/97 C:59   PROJ: KY008                                  *
     H*                  1. Added Message #FN
     H*
     H*  03/31/97 C:59   PROJ: OH009                                  *
     H*                  1. Added Message #FO
     H*                  2. Added Message #FP
     H*                  2. Added Message #FQ
     H*                  2. Added Message #FR
     H*                  2. Added Message #FS
     H*                  2. Added Message #FT
     H*                  2. Added Message #FU
     H*                                                               *
     H*  06/17/97 C:41   PROJ: SC003                                  *
     H*                  1. Added Message #FV
     H*                                                               *
     H*  07/01/97 C:32   PROJ: 87MBY                                  *
     H*                  1. Added Messages #FW, #FX, #FY for diag err *
     H*                                                               *
     H*  07/10/97 C:32   PROJ: SC003                                  *
     H*                  1. Added Message #FZ
     H*                                                               *
     H*  03/31/97 C:59   PROJ: OH009                                  *
     H*                  1. Added Message #GA
     H*                                                               *
     H*  08/04/97 C:59   PROJ: GA009                                  *
     H*                  1. Added Message #GB
     H*                                                               *
     H*  08/15/97 C:59   PROJ: OH009 PHASE 4                          *
     H*                  1. Added Message #GC
     H*                  2. Added Message #GD
     H*                  3. Added Message #GE
     H*                  3. Added Message #GF
     H*                                                               *
     H*  08/20/97 C:32   PROJ: 97MBY CO#3                             *
     H*                  1. Added Message #GG
     H*                                                               *
     H*  08/25/97 C:59   PROJ: OH009 PHASE 4                          *
     H*                  1. Added Message #GH
     H*                                                               *
     H*  09/05/97 C:59   PROJ: TX007
     H*                  1. Added Message #GI
     H*                  2. Added Message #GJ
     H*                                                               *
     H*  09/12/97 C:32   PROJ: MC087/8657
     H*                  1. Added Message #GK
     H*                                                               *
XXX  H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  21    INVALID OPTION ENTERED                                 *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*                                                               *
     H*****************************************************************
     FF0572F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN1)
     F*** BEGIN DEL *** 10/21/92 **************************************
     F**********SMCOL   IF  E           K        DISK
     F*** END DEL 10/21/92 ********************************************
     FSMUPL     IF   E           K DISK

     D QSA             S              5    DIM(30)                              Passed array Seq#
     D QEA             S              5    DIM(30)                              Passed array Msg ID
     D*** START ADD * 11/24/92 ****************************************
      *-----------------* PRE-CERT
     D QSP             S              5    DIM(50)                              Passed array Seq#
     D QEP             S              5    DIM(50)                              Passed array Msg ID
      *-----------------* MDR
     D QSM             S              5    DIM(50)                              Passed array Seq#
     D QEM             S              5    DIM(50)                              Passed array Msg ID
      *-----------------* CRR
     D QSC             S              5    DIM(50)                              Passed array Seq#
     D QEC             S              5    DIM(50)                              Passed array Msg ID
     D***** END ADD * 11/24/92 ****************************************
     D QS              S              5  0 DIM(180) ASCEND                      Internal array Seq#
     D QE              S              5    DIM(180)                             Internal array Msg I
     D SA              S             10    DIM(180)                             Used to sort Msg ID
     D AL3             S              1    DIM(3)                               CHECK NUMERIC
      *** BEG CHG 11/17/98 ********************************************
     D******************* NUM     1 277  5   MSG    70   Msg ID/Msg Desc
     D NUM             S              5    DIM(281) CTDATA PERRCD(1)            Msg ID/Msg Desc
     D MSG             S             70    DIM(281) ALT(NUM)
      *** END CHG 11/17/98 ********************************************
     D EMSG            S             61    DIM(1) CTDATA PERRCD(1)              Error Msg Desc
     D*** BEGIN ADD *** 01/05/94 **************************************
      *** BEG CHG 11/17/98 ********************************************
     D******************  ERID    1 277  5   LVLID   7
     D ERID            S              5    DIM(281) CTDATA PERRCD(1)
     D LVLID           S              7    DIM(281) ALT(ERID)
      *** END CHG 11/17/98 ********************************************
     D*** END ADD *** 01/05/94 ****************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXMSG                  1     67
     D  XMSGID                63     67
     D                 DS
     D  QQMSGI                 1      7
     D  M                      1      1
     D  ALPH2                  2      3
     D  NUM40                  4      7
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QSA                            FEE SCHED
     C                   PARM                    QEA                               "
     C                   PARM                    QSP                            PRE CERT
     C                   PARM                    QEP                               "
     C                   PARM                    QSM                            MDR
     C                   PARM                    QEM                               "
     C                   PARM                    QSC                            CRR
     C                   PARM                    QEC                               "
     C                   PARM                    QQDSEQ            5
     C                   PARM                    QQRTCD            1
     C*
     C                   MOVEL     'M'           M
     C                   Z-ADD     *ZEROS        XXDSEQ            5 0
     C                   Z-ADD     *ZEROS        XA                3 0
     C                   Z-ADD     *ZEROS        XB                3 0
     C                   Z-ADD     *ZEROS        RRN1              3 0
     C                   MOVE      *BLANKS       XXMSG
     C                   MOVE      *BLANKS       XMSGID
     C*
     C*
     C                   MOVE      QSA           QS
     C                   MOVE      QEA           QE
     C*
     C                   DO        50            X
     C     X             ADD       30            Y
     C                   MOVE      QSP(X)        QS(Y)
     C                   MOVE      QEP(X)        QE(Y)
     C                   END
     C*
     C*
     C                   DO        50            X
     C     X             ADD       80            Y
     C                   MOVE      QSM(X)        QS(Y)
     C                   MOVE      QEM(X)        QE(Y)
     C                   END
     C*
     C*
     C                   DO        50            X
     C     X             ADD       130           Y                 5 0
     C                   MOVE      QSC(X)        QS(Y)
     C                   MOVE      QEC(X)        QE(Y)
     C                   END
     C*
     C                   MOVE      QQDSEQ        XXDSEQ
     C*
     C                   DO        180           X
     C     QS(X)         IFEQ      *ZERO
     C                   MOVEL     99999         SA(X)
     C                   ELSE
     C                   MOVEL     QS(X)         SA(X)
     C                   END
     C                   MOVE      QE(X)         SA(X)
     C                   END
     C*
     C                   SORTA     SA
     C*
     C                   DO        180           X
     C                   MOVEL     SA(X)         QS(X)
     C     QS(X)         IFLT      *ZERO
     C     QS(X)         MULT      -1            QS(X)
     C                   END
     C     QS(X)         IFEQ      99999
     C                   MOVE      *ZERO         QS(X)
     C                   END
     C                   MOVE      SA(X)         QE(X)
     C                   END
     C*
     C                   TIME                    DSDTTM
     C**********           MOVE Q1USER    SCUSER           DEL 10/21/92
     C**********           MOVE QQPRDS    SCPRDS           DEL 10/21/92
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*** BEGIN DEL *** 10/21/92 **************************************
     C********** COKEY     CHAINPCO01                60
     C**********           MOVELCONAME    SCCONM
     C*** END DEL *** 10/21/92 ****************************************
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW01C
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C**********           MOVELQ2FILE    SCFILE           DEL 10/21/92
     C**********           MOVELQ2RECF    SCRECF           DEL 10/21/92
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01C'
     C     Q2RECF        OREQ      'WDW01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     WDW01C
     C                   WRITE     WDW01
     C                   EXFMT     WDW01C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C*** BEGIN DEL *** 10/16/92 **************************************
     C***********************************
     C*   F6 = DETAIL INQUIRY
     C***********************************
     C********** *INKF     IFEQ '1'
     C**********
     C**********           MOVE SCDSEQ    QQDSEQ
     C**********
     C**********           CALL 'F0574R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCFYR
     C**********           PARM           SCCASE
     C**********           PARM           QQDSEQ
     C**********           PARM           QQRTCD
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'WDW01'   Q2RECF
     C**********           GOTO STRT
     C**********
     C**********           END                             ENDIF *INKF
     C*** END DEL *** 10/16/92 ****************************************
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01S'
     C     Q2RECF        OREQ      'WDW01C'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
     C*** BEGIN DEL *** 10/16/92 **************************************
     C***********************************
     C*  F16 = HISTORY SEARCH           *
     C***********************************
     C********** *INKQ     IFEQ '1'
     C**********           CALL 'F0575R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCFYR
     C**********           PARM           SCCASE
     C**********           PARM           SCSS#
     C**********           PARM           SCACDT
     C**********           PARM           QQRTCD
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'WDW01'   Q2RECF
     C**********           GOTO STRT
     C**********
     C**********           END                             END *INKQ
     C*** END DEL *** 10/16/92 ****************************************
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C************
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C**************************************************************************
     C*
     C*
     C                   MOVE      '0'           *IN50
     C*
     C*   Save RRN so that rolling can properly function               IS MARKED
     C*
     C                   MOVE      RRN1          SRRN1             5 0
     C                   MOVE      RRN1          XRRN1             5 0
     C*
     C                   READC     WDW01S                                 60
     C*
     C*** BEGIN ADD *** 02/15/93 **************************************
     C     *IN60         IFEQ      '1'
     C     SCSEL         ANDEQ     *BLANKS
     C                   GOTO      TERM
     C                   END                                                    END *IN60
     C*** END ADD *** 02/15/93 ****************************************
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*----------------------------------------------------------------
     C*   Check for invalid option
     C*----------------------------------------------------------------
     C     SCSEL         IFNE      ' '
     C     SCSEL         IFNE      '5'
     C                   MOVE      '1'           IN21              1
     C                   MOVE      '1'           *IN21
     C                   MOVE      '1'           *IN50
     C                   MOVE      RRN1          XRRN1             5 0
     C*
     C                   UPDATE    WDW01S
     C                   GOTO      NXT1
     C                   END
     C                   END
     C*----------------------------------------------------------------
     C*   Process for '5=Detail MESSAGE DISPLAY'
     C*----------------------------------------------------------------
     C     SCSEL         IFEQ      '5'
     C*** BEGIN CHG *** 01/05/94 **************************************
     C*
     C*======== BUILD MESSAGE ID TO DISPLAY LOWER LEVEL MSGS VIA 572C
     C*
     C**********           MOVE SCMSID    ALPH3
     C**********           MOVEAALPH3     AL3
     C**********           DO   3         Q       30
     C********** AL3,Q     IFLT '0'
     C********** AL3,Q     ORGT '9'
     C**********           MOVE '0'       AL3,Q
     C**********           END
     C**********           END
     C*
     C**********           MOVEAAL3       ALPH3   3
     C**********           MOVE *ALL'0'   NUM40   4
     C**********           MOVE ALPH3     NUM40   4
     C*
     C**********           MOVELSCMSID    ALPH2   2
     C********** ALPH2     IFEQ *BLANKS
     C**********           MOVE 'FS'      ALPH2
     C**********           END
     C*
     C                   Z-ADD     1             Q                 3 0
     C     SCMSID        LOOKUP    ERID(Q)                                75
     C     *IN75         IFEQ      '1'
     C                   MOVEL     LVLID(Q)      QQMSGI
     C                   ELSE
     C                   MOVE      *BLANKS       QQMSGI
     C                   END
     C*** END CHG *** 01/05/94 ****************************************
     C*
     C                   MOVE      *BLANK        QQRTCD
     C*
     C                   CALL      'F0572C'
     C                   PARM                    QQMSGI
     C                   PARM                    QQRTCD            1
     C*
     C     QQRTCD        IFEQ      'E'
     C                   MOVE      '1'           *IN50
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           IN22              1
     C                   MOVE      RRN1          XRRN1
     C                   UPDATE    WDW01S
     C                   GOTO      NXT1
     C                   END
     C*
     C                   MOVE      *BLANKS       SCSEL
     C                   MOVE      RRN1          XRRN1
     C                   UPDATE    WDW01S
     C                   GOTO      NXT1
     C*
     C                   END                                                    END SEL EQ '5'
     C*
     C*
     C*----------------------------------------------------------------
     C     NXT1          TAG
     C                   MOVEA     '00'          *IN(21)
     C*
     C                   READC     WDW01S                                 60
     C                   END
     C*
     C     IN21          IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   MOVE      ' '           IN21
     C                   END
     C*
     C     IN22          IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   MOVE      ' '           IN22
     C                   END
     C*
     C*   Restore RRN so that rolling can properly function
     C*
     C                   MOVE      SRRN1         RRN1
     C*
     C     *IN50         IFEQ      '1'
     C                   MOVE      XRRN1         #@$1
     C                   GOTO      STRT
     C                   END
     C*
     C*   Set at last record selected
     C*
     C                   Z-ADD     XRRN1         #@$1
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C*
     C                   GOTO      STRT
     C*
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN1
     C                   Z-ADD     1             XA                             ARRAY COUNTER
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C*
     C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 10 LINES
     C                   DO        5             X                 5 0
     C*
     C*** Look for a hit on the selected Sequence #
     C     XA            IFLT      181                                            181 TIMES
     C     XXDSEQ        LOOKUP    QS(XA)                                 62
     C     *IN62         IFEQ      '1'
     C                   ADD       1             RRN1
     C                   Z-ADD     1             XB
     C     QE(XA)        LOOKUP    NUM(XB)                                61
     C     *IN61         IFEQ      '1'
     C                   MOVEL     NUM(XB)       SCMSID
     C                   MOVEL     MSG(XB)       SCMSG
     C                   ELSE
     C                   MOVE      *BLANKS       XXMSG
     C                   MOVEL     EMSG(1)       XXMSG
     C                   MOVE      QE(XA)        XXMSG
     C                   MOVEL     QE(XA)        SCMSID
     C                   MOVEL     XXMSG         SCMSG
     C                   END                                                    END 1 *IN61
     C                   MOVE      '0'           *IN06
     C                   WRITE     WDW01S
     C                   ADD       1             XA
     C                   ELSE                                                   ELSE 1 *IN62
     C                   MOVE      '1'           *IN05
     C                   MOVE      '1'           *IN32
     C                   GOTO      NOMSG
     C                   END                                                    END 1 *IN62
     C                   END                                                    END LT 181
     C                   END                                                    END DO 7 X
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     NOMSG         TAG
     C     RRN1          IFEQ      0
     C                   Z-ADD     1             RRN1
     C                   Z-ADD     1             XB
     C     '99999'       LOOKUP    NUM(XB)                                61
     C     *IN61         IFEQ      '1'
     C                   MOVEL     MSG(XB)       SCMSG
     C                   END
     C                   MOVE      '0'           *IN06
     C                   WRITE     WDW01S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN1          #@$1
     C*
     C                   ENDSR
**  NUM (5) / MSG (70)
   01The Bill requires further inspection by an adjuster
   02The status indicated is an override type
   03Possible duplicate charges exists for this item
   04This item contains the Microsurgery Modifier
   05The item to be paid will change the already paid items. See Adjuster
   06The item is BY REPORT-requires further inspection by an adjuster
   07The item could be an initial cast, adjust the paid amount accordingly
   08Unusual services modifier.  Adjuster should verify amount to be paid
   09This visit occured during another procedure's follow-up period
   10Invalid modifier used with this item. Re-enter item w/valid modifier
   11Multiple procedures (97010-97139). Use 97530 under Fee Schedule
   12This item is not allowed when 97530 (combination code) is present
   13This item is paid at 20% of the major codes MRA. Please key amount
   14This item is paid at 25% of the major codes MRA. Please key amount
   15This item is paid at 50% of the major codes MRA. Please key amount
   16A major code must be present to pay this item
   17The total of codes 95831-95852 cannot exceed MRA for code 95834
   18See Table in Fee Schedule Manual for allowed amount
   19*WARNING*  Need Letter of Medical Necessity to pay this item
   20*WARNING* Combination of Codes 95831 and 95851 is not to exceed $35
   21If codes 93510,93543,and 93545 are together, use 93547
   22If codes 93501 and 93547 are together, use 93549 instead
   23Contains Modifier 90, must pay provider's charges in full
   24The item is ANE - requires further inspection by an adjuster
   25This item is non-reimburseable under the Fee Schedule
   26Only 2 nerve studies are allowed in a 90-day period
   27If the necessary documentation exists, override and pay at MRA
   28The detail item does not XREF any summary item on this bill
   29This procedure was previously paid under a complete code
   30More than two arthroscopic procedures need Documentation of Necessity
   31The total MRA OF procedures 92541 - 92545 should not exceed 150.00
   32Procedure is paid at "TOTAL BODY" value.  If needed reduce paid amt
   33Upper Body - 325.00, Face & Head - 175.00, Neck & TMJ - 300.00
   34TMJ - 90.00, Lumbar Sacral incl. legs - 425.00,  Total body - 650.00
   35Procedure is paid at "WHIRLPOOL,SMALL" value, If needed, adj paid amt
   36Whirlpool,Small - 20.00, Small,Sterile - 25.00, Large - 25.00
   37Large,Sterile - 30.00
   38This procedure is "BILATERAL", The MRA = 1300.00
   39Radiology paid as "PHYS FEE" not "TOT FEE"-See Manual for Tot Fee(s)
   40Invalid Office Visit Code for this Provider's Specialty
   41More than 2 Therapeutic Modalities should have Document of Necessity
   42Code is paid at Reasonable & Customary, Max. Charge is $75.00
   43Adjuster should review charges for an unscheduled, starred procedure
   44You may include the charge for medication in the payment of 90782
   45Reimbursement for Dispensed Medicine Code 96370 = AWP X 1.2 + $4.18
   46Reimbursement for IME Code 96320 = Refer to Prov/Carrier Agreement
   47A copy of the Consultation Report is required for reimbursement
   48An F-Wave is considered a part of the study of a nerve, please verify
   49Reimbursement was determined using the ANES Calcs. Please Verify
   50Reduced Services Modifier.  Adjuster should verify amount to be paid
   51Possible Duplicate Charge exists for this Office Visit item
   52This item is SV - Requires further inspection by an adjuster
   53Payment for 99070 not to exceed cost of item + 25% & $15/Single Item
   54Exceeds number of procedures allowed in time period
   55A copy of the Justification Report is required for reimbursement
   56Adjuster should verify use of this procedure
   57Anesthesia amount either BR or Not Established - Please Review
   58The surgeon's charge has not been paid for this proc - Please Review
   59Dislocations complicated by Fracture: inc unit val by 50% excpt ankle
   60If there are co-surgeons on this proc, refer to FS for Reimb
   61If amp w/in 14 dys of init maj vas reconst - refer to FS for reimb
   62Refer to Fee Schedule Manual for instructions on reimbursement
   63Values listed for arthroscopy for major proc, add 15% for ea addtl
   64Item is Postoper Mgmt Only mod-55,Refer to Fee Sched for reimbursement
   65Item is Two Surgeons mod-62, Refer to Fee Sched for reimbursement
   66Item is Surgical Team mod-66, Refer to Fee Sched for reimbursement
   67Item is Concurrent Care mod-75, Refer to Fee Sched for reimbursement
   68Item is Repeat of Proc by same phys mod-76, Rpt reqd for reimbursement
   69Item is Repeat of Proc by other phys mod-77, Rpt reqd for reimbursement
   70Item is Surgical Care Only mod-54,Refer to Fee Sched for reimbursement
   71Item uses assistant surgeon mod-80. Adjuster should verify usage
   72This charge is for a Medical Device, please review
   73This charge is for Elevated Care, please review
   74History exists for codes 97010-97139; Review payment of this charge
   75History exists for code 97145; Review payment of this charge
   76CPT modifier used with this charge.  Verify use and payment.
   77No anesthesia value in Fee Schedule.  Verify use and payment.
   78Addtl Minutes Code entered before Major Code. Verify payment.
   79Service Date entered is greater than today's date (system date).
   80Reimb was determined using other than C-AN or AN Calcs. Please Verify.
   81Reimb was calculated for obstetrical anesthesia.  Please Verify.
   82A modifier is NOT allowed with this code.  Please verify usage.
   83The item is I.C.-requires further inspection by an adjuster
   84This item is considered "PURCHASE" only. Please verify usage.
   85This item is considered "RENTAL" only. Please verify usage.
   86A Visit/Consultation charge exists for this day. Please review.
   87Prior written authorization required for reimbursement.
   88If proc. is located at a different site, override and pay 100% of MRA
   89Reimbursement was determined using the full CRNA Calcs. Please Verify
   90Adjustor should review charges for significant identifiable services.
   91Carrier to set payment on individual case basis based on documentation
   92If DME: MRA = $326.00, if Respiratory: MRA = $242.13
   93Please verify this service was provided by an OPHTHALMOLOGIST
   94Invalid NDC Drug Code, See Data Processing
   95Invalid User Parameter, See Data Processing
   96Invalid Case Number, See Data Processing
   97Invalid Claim Status Code, See Data Processing
   98Invalid Procedure Code, See Data Processing
   99Invalid data is present for this item
   AAER visit submitted with another visit charge.  Please verify.
   ABBased on POS: If Hosp or ASC submitting chrg, use Tech Comp.
   ACReview Fee Schedule for MRA value on this code/suffix.
   ADInvoice is required for payment.
   AEThis code should be submitted on a HCFA form
   AFPay 80% of MRA, if condition requires more than one dentist.
   AGImproper NY Zip Code submitted: Payment calculated on base region
   AHRefer to corresponding Anesthesia code for base units
   AIEnter charge on seperate EOMB to properly calculate
   AJService falls within pre-operative policy
   AKAnes not administered by an ANE or RNA is included in surgical proc
   ALNo service code information was found for this provider
   AMOnly two areas allowed unless documentation of multiple injuries
   ANAn anesthesia reimbursement value is not available for this code
   AOThis service is to be reimbursed per individual consideration
   APReimbursement varies per surgical procedure -see Opt '5' for details
   AQPhysical medicine codes may only be billed by physicians
   ARThis service has been reimbursed per the Clinical Pathologist rate
   ASThis service has been reimbursed per the Podiatric services rate
   ATPaid at maximum trip fee.  If round trip, override and pay 2X
   AUIf Metropolitan (Deaf forms): Rate=$112.50, if Stanford: Rate=$141.00
   AVIndividual therapy cannot exceed one hour per session
   AWMRI paid at Prof Comp amount, if necessary override and pay full MRA
   AXMRI paid at full MRA, if necessary override and pay at Prof Comp amt
   AYModifier 'RP' is not a valid modifier for a code specified as rental
   AZHistory exists for codes X2106-X2107; Review payment of this charge
   BAHistory exists for code A4557; Review payment of this charge
   BBHistory exists for code Z6300; Review payment of this charge
   BCImproper MA County Code submitted:Payment calculated on national rate
   BDThis item is not allowed when 97010-97139 is present
   BEProfessional Component amount Not Established - Please Review
   BFFor 4 or more Surgical procedures, review for global fee
   BGPaid per 97200-97201 combination:Review that time exceeded 30 minutes
   BHVerify Prov/Mod: Payment should be made to employer, not individual
   BIDepending on type of supply, payment for 99070 may be increased 20%
   BJVerify submitted documentation indicates necessary office visit.
   BKImproper PA Zip Code submitted: Payment calculated on base region
   BLFor 5 or more Phys Med procedures, review
   BMFor 4 or more Radiology procedures, review for global fee
   BNProvider was paid at the Out-of-State hospital reimbursement rate
   BOInvalid Private Duty Nursing Care Code for this Provider's Specialty
   BPSpecialty Code and level of service don't match, please verify
   BQDocumentation is required for reimbursement
   BRItem is Pre-oper Mgmt Only mod-56,Refer to Fee Sched for reimbursement
   BSDME D0540, Doc of medical necessity required for use over 30 days
   BTOverride and pay at $200 for 1st hour; $50 for each addtl 30 mins
   BUEOMB Code required, total paid does not equal total charged
   BVThe payment amount is greater than the charged amount
   BWThe payment amount of this line item exceeds $25,000.00
   BXThe charge amount of this line item exceeds $25,000.00
   BYEOMB Code required, the payment has been reduced to zero
   BZEOMB Code required, the payment amount is greater than the charges
   CAInvalid modifier used with an MRI/Cat Scan
   CBThe payment amount of this line item exceeds $50,000.00
   CCThe charge amount of this line item exceeds $50,000.00
   CDPT service over the 8 per day/per body area limit (1st wk trtment)
   CENarr Rept (99080): 1st pg $38.16, ea additl pg $15.26, Max $76.32
   CFPT service over the 4 per day/per body area limit aft 1st wk trmnt
   CGOnly 4 mods., procs., manipuls. allowed when office visit billed
   CHItem is Decision for Surgery Mod-57,Refer to Fee Sched for reimbursmnt
   CIReturn to Oper Rm(related proc) Mod-78,Refer to Fee Sched for reimburs
   CJUnrelated Proc by Phys (Post OP) Mod-79, See Fee Sched for reimbursmnt
   CKMinimum Assist by a Surgeon  Mod-81, Refer to Fee Sched for reimburs
   CLAssistant Surgeon (for resdnt Surg)Mod-82, See Fee Sched for reimburs
   CMNumber of epidural steroid injections is 3 / Yr, (cervical 2 / Yr)
   CNStaged proc. by same Phys Mod-58, Refer to Fee Sched for reimbursement
   COStop-Loss Method: Total claim > $50,000, MRA reduced to 80%
   CPPlease review line item due to date of service older than 07/01/93
   CQThis service is included under critical care services (99291-99292)
   CRIf only modalities are performed, limit 2 weeks, unless DOP provided
   CSFCE(s) allowed max 3 times for a total of $700, per visit
   CTIf accreditation is NOT CARF, REDUCE PAYMENT BY 20%
   CUExceeds number of procedures allowed in time period, DOP REQUIRED
   CVThis procedure is limited to once every 30 days
   CWThis procedure does not have the corresponding L, S, or E values
   CXPossible unbundling of a total proc - separate proc coded w/SURG
   CYOffice visit, reimbursed at procedure code 99025 amount
   CZFollowup visit included in the charge for a starred procedure
   DAManipulation under ANES of a joint w/SURG, same day (disallowed)
   DBIf billed with a patient encounter on same date, disallow payment
   DCIV injection of contrast material is included in the CAT proc.
   DDIf billing for videofluoroscopy, no reimbursement is allowed
   DEPanel testing, read report for verification of services
   DFEpidural steroid injections shall be billed using code 62289.
   DGReview notes, may need to DOWNCODE to code 99212.
   DHReview notes, may need to UPCODE to code 99213.
   DIReview notes, may need to DOWNCODE to code 99213.
   DJPT service over the 8 per day or 4 hr time limit (1st wk trmnt)
   DKPT service over the 4 per day or 2 hr time limit aft 1st wk trmnt
   DLThere are multiple asterisk procedures on this date verify payment
   DMIf surg proc is a follow-up to a previous major surg, pay at 50%
   DNOffice visit within follow-up of surgery, chg to MC004, pay zero
   DOSpecialty code is 'PA', apply code MC001 w/modifier -80
   DPCode 64830 is allowed as 20% of surgeon's bill for repair of nerves
   DQMedical testimony fee is set by the NCIC hearing officer.
   DRProvider used incorrect codes, must use CHnnn codes.
   DSCPT4 code 64450 is reimburseable only with asterisk procedures.
   DTVerify medical assessment and plan of care.
   DUArthroscopy is global - review operative report.
   DVOnly 1 of these procedure codes is allowed per date,adjust part code
   DWInvalid part code used with this item. Re-enter item w/valid part cd
   DXCode 99024 is allowed $20 (maximum) for removal of sutures
   DYIf billing for Hepatitis B Immune Globulin Inj., allow $165 (5ml)
   DZPayment Reduced - Review Operative Report/Multiple Surgical Rule
   EAOnly 1 procedure is allowed with modifier -80 (per date)
   EBThis procedure has already been paid with this modifier
   ECReview notes on payment allowed
   EDAsterisk proc billed w/ E/M code, (New/Estab?) Apply Rule Manually
   EEIf Check-List Report ($48 allowed), if Written Report ($60 allowed)
   EFSupplies are not allowed with this code range
   EGThis procedure is limited to once every 2 weeks
   EHVerify provider's license or certification in biofeedback
   EISupervision/Interpretation included in basic allowance,REVIEW payment
   EJVerify anesthesia MAP, override and pay at 50%
   EKThis procedure is limited to once every 7 days
   ELMust verify degree of burn
   EMOnly 1 'initial minutes' code is allowed per date
   ENVerify assistant as MD
   EOIf 1-2 body areas ($350 allowed), more than 2 areas ($500 allowed)
   EPPayment is for 1st hour; override and pay $51.35, ea addtl 30 mins
   EQMinimum allowance is $35.00, MAX is $75.00
   ERThe total MRA OF procedures 92541 - 92545 should not exceed 250.00
   ESThe maximum allowed for code 93760 is $650.00
   ETThis service has been reimbursed per the Vision Care Services rate
   EUThis service has been reimbursed per Freestanding Diag. Fac. rate
   EVPre-certification is required
   EWSupplies = INV + 25% OR $15.00, Meds = AWP +6.00 whichever is less
   EXPrevious surgery paid, Review history before paying at 50%.
   EYIf this is one of 1st 3 visits, override and pay at 100%.
   EZThe payment amount of this line item exceeds $3,500.00
   FAThe payment amount of this line item exceeds $1,260.00
   FBReview Operative Report/Multiple RADIOLOGY Rule.
   FCLimited to four hours per date of service.
   FD21 Visits Paid, Authorization Required.
   FEF-Wave limit 2 per extremity, H-Reflex Limit 1 per leg
   FFReview bill and rules before paying
   FGAdditional timed billed with out initial time
   FHReview medical necessity
   FITwo or more phycical tests, = 95834
   FJVerify Pre-authorization
   FKWork Hardening has passed eight weeks
   FLFollow up visit with PT, verify medical necessity
   FMTest and Measurements are included in the value of the Eval & Mgt
   FNMultiple procedures have been billed, adjustor to review
   FOInvalid Place of Service code
   FPPhysician of Record may only be reimbursed for a consultation
   FQVerify appropriate diagnosis
   FR12 Visits Paid, Authorization Required.
   FSTraumatic brain injury claim, submit to utilization management
   FTVerify specialty of provider for this treatment
   FUHot packs not reimbursable with massage
   FVPayment is for 1st hour; override and pay $52.89, ea addtl 30 mins
   FWInvalid ICD9 Code, send to UR
   FXDiag unrelated, send to UR
   FYBilled Diag is not allowed on claim, send to UR
   FZSend to Utilization Review
   GAConsultation must be ordered by the physician of record
   GBPay additional $30.00 per home health visit
   GCManual pricing required
   GDMedical review required
   GEMultiple units entered, verify payment
   GFDoes diagnosis support multiple views
   GGBilled diagnosis has blank BWC status in Case Mgmt
   GHCode used has not been updated to current fee schedule
   GICarrier to set payment at fair and reasonable rate
   GJStop Loss reached, billed is greater than $40,000.00
   GKPaid amt not = Charged amt - MRA denied - other - network reduct
   GLConsulting physician is not permitted to treat
   GMA Consultation has been paid to this provider
   GNMultiple PT services billed.  Reduced 50% per ground rule #5
   GOOnly 3 visits allowed in 7 days. Written report must be submitted
PC001PC001-THIS IS MESSAGE # 1 FOR PRE-CERT
PC002PC002-Invalid Place of Service code
PC003PC003-Service date does not fall within Pre/Re-Cert date range
PC004PC004-Diagnosis code has not been included in the Pre/Re-Cert
PC005PC005-Procedure code has not been included in the Pre/Re-Cert
PC006PC006-Total # of visits > than cert. # of visits within date rng
PC007PC007-Procedure code is not within certified date range
PC008PC008-Total number of visits greater than cert. number of visits
CE001THIS IS MESSAGE # 1 FOR MDR CLAIM EDIT
CR001THIS IS MESSAGE # 1 FOR CRR
99999No messages exist for this charge
**  EMSG
Invalid Message ID - No matching message found for Message ID
**  ERID (5) / LVLID (7)
   01MFS0001
   02MFS0002
   03MFS0003
   04MFS0004
   05MFS0005
   06MFS0006
   07MFS0007
   08MFS0008
   09MFS0009
   10MFS0010
   11MFS0011
   12MFS0012
   13MFS0013
   14MFS0014
   15MFS0015
   16MFS0016
   17MFS0017
   18MFS0018
   19MFS0019
   20MFS0020
   21MFS0021
   22MFS0022
   23MFS0023
   24MFS0024
   25MFS0025
   26MFS0026
   27MFS0027
   28MFS0028
   29MFS0029
   30MFS0030
   31MFS0031
   32MFS0032
   33MFS0033
   34MFS0034
   35MFS0035
   36MFS0036
   37MFS0037
   38MFS0038
   39MFS0039
   40MFS0040
   41MFS0041
   42MFS0042
   43MFS0043
   44MFS0044
   45MFS0045
   46MFS0046
   47MFS0047
   48MFS0048
   49MFS0049
   50MFS0050
   51MFS0051
   52MFS0052
   53MFS0053
   54MFS0054
   55MFS0055
   56MFS0056
   57MFS0057
   58MFS0058
   59MFS0059
   60MFS0060
   61MFS0061
   62MFS0062
   63MFS0063
   64MFS0064
   65MFS0065
   66MFS0066
   67MFS0067
   68MFS0068
   69MFS0069
   70MFS0070
   71MFS0071
   72MFS0072
   73MFS0073
   74MFS0074
   75MFS0075
   76MFS0076
   77MFS0077
   78MFS0078
   79MFS0079
   80MFS0080
   81MFS0081
   82MFS0082
   83MFS0083
   84MFS0084
   85MFS0085
   86MFS0086
   87MFS0087
   88MFS0088
   89MFS0089
   90MFS0090
   91MFS0091
   92MFS0092
   93MFS0093
   94MFS0094
   95MFS0095
   96MFS0096
   97MFS0097
   98MFS0098
   99MFS0099
   AAMFS0100
   ABMFS0101
   ACMFS0102
   ADMFS0103
   AEMFS0104
   AFMFS0105
   AGMFS0106
   AHMFS0107
   AIMFS0108
   AJMFS0109
   AKMFS0110
   ALMFS0111
   AMMFS0112
   ANMFS0113
   AOMFS0114
   APMFS0115
   AQMFS0116
   ARMFS0117
   ASMFS0118
   ATMFS0119
   AUMFS0120
   AVMFS0121
   AWMFS0122
   AXMFS0123
   AYMFS0124
   AZMFS0125
   BAMFS0126
   BBMFS0127
   BCMFS0128
   BDMFS0129
   BEMFS0130
   BFMFS0131
   BGMFS0132
   BHMFS0133
   BIMFS0134
   BJMFS0135
   BKMFS0136
   BLMFS0137
   BMMFS0138
   BNMFS0139
   BOMFS0140
   BPMFS0141
   BQMFS0142
   BRMFS0143
   BSMFS0144
   BTMFS0145
   BUMFS0146
   BVMFS0147
   BWMFS0148
   BXMFS0149
   BYMFS0150
   BZMFS0151
   CAMFS0152
   CBMFS0153
   CCMFS0154
   CDMFS0155
   CEMFS0156
   CFMFS0157
   CGMFS0158
   CHMFS0159
   CIMFS0160
   CJMFS0161
   CKMFS0162
   CLMFS0163
   CMMFS0164
   CNMFS0165
   COMFS0166
   CPMFS0167
   CQMFS0168
   CRMFS0169
   CSMFS0170
   CTMFS0171
   CUMFS0172
   CVMFS0173
   CWMFS0174
   CXMFS0175
   CYMFS0176
   CZMFS0177
   DAMFS0178
   DBMFS0179
   DCMFS0180
   DDMFS0181
   DEMFS0182
   DFMFS0183
   DGMFS0184
   DHMFS0185
   DIMFS0186
   DJMFS0187
   DKMFS0188
   DLMFS0189
   DMMFS0190
   DNMFS0191
   DOMFS0192
   DPMFS0193
   DQMFS0194
   DRMFS0195
   DSMFS0196
   DTMFS0197
   DUMFS0198
   DVMFS0199
   DWMFS0200
   DXMFS0201
   DYMFS0202
   DZMFS0203
   EAMFS0204
   EBMFS0205
   ECMFS0206
   EDMFS0207
   EEMFS0208
   EFMFS0209
   EGMFS0210
   EHMFS0211
   EIMFS0212
   EJMFS0213
   EKMFS0214
   ELMFS0215
   EMMFS0216
   ENMFS0217
   EOMFS0218
   EPMFS0219
   EQMFS0220
   ERMFS0221
   ESMFS0222
   ETMFS0223
   EUMFS0224
   EVMFS0225
   EWMFS0226
   EXMFS0227
   EYMFS0228
   EZMFS0229
   FAMFS0230
   FBMFS0231
   FCMFS0232
   FDMFS0233
   FEMFS0234
   FFMFS0235
   FGMFS0236
   FHMFS0237
   FIMFS0238
   FJMFS0239
   FKMFS0240
   FLMFS0241
   FMMFS0242
   FNNFS0243
   FONFS0244
   FPNFS0245
   FQNFS0246
   FRNFS0247
   FSNFS0248
   FTNFS0249
   FUNFS0250
   FVNFS0251
   FWNFS0252
   FXNFS0253
   FYNFS0254
   FZNFS0255
   GANFS0256
   GBNFS0257
   GCNFS0258
   GDNFS0259
   GENFS0260
   GFNFS0261
   GGNFS0262
   GHNFS0263
   GINFS0264
   GJNFS0265
   GKNFS0266
PC002MPC0002
PC003MPC0003
PC004MPC0004
PC005MPC0005
PC006MPC0006
PC007MPC0007
PC008MPC0008
