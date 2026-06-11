     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H* Program: F0758R  - 10-day Request Letter Print Program        *
     H*****************************************************************
     H* Created on 12/09/2002 by c:62 for Project 114MEO              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 10/28/15 C:307 Support: 35844                                 *
     H*                -To add the code for not printing routing page *
     H*                 for client '114'.                             *
     H*                                                               *
     H* 09/01/15 C:307 Support: 35844                                 *
     H*                -Extra Page on Downcode letter and adding other*
     H*                 information.                                  *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 10/12/05 C:132 PROJECT MC114 - 49225                          *
     H*               - Add code to include second office address     *
     H*                 line if it exists.                            *
     H*                                                               *
     H* 03/16/05 C:62 PROJECT 114MEO MC114 46102                      *
     H*               -REMOVE VERBIAGE "CERTIFIED MAIL"               *
     H*               -REMOVE THE DATE                                *
     H*                                                               *
     H* 02/07/03 C:62 PROJECT 114MEO                                  *
     H*               ADD COMPANY INFO TO LETTER                      *
     H*                                                               *
     H*****************************************************************
     FFALTL1    UF   E           K DISK
     FFAL2L3    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FWMOFL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D clientID        s              3    inz('000')
     D* Get client ID
     D GetClientID     pr                  extpgm('RTVCLTID')
     D   outCltID                     3

     C*****************************************************************
     C* Parameter Lists
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@LTR#            9
     C                   PARM                    @@FLAG            1
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
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
     C*****************************************************************
     C* Key Lists
     C*****************************************************************
     C*
     C* Key List for the Claim Master File
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C* Key List for the Group Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C* Key List for the Request Letter File
     C*
     C     LTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKLTR#
     C*
     C* Key List for the Letter Detail File
     C*
     C     L2KY          KLIST
     C                   KFLD                    LTCO#
     C                   KFLD                    LTSUB#
     C                   KFLD                    LTFND
     C                   KFLD                    LTFYR
     C                   KFLD                    LTCASE
     C                   KFLD                    LTLTR#
     C*
     C* Key List for the WCMS Office Master File
     C*
     C     OFKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKOFC#
     C*
     C* Key List for the Output Routing File
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* Key List for the Provider File
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*****************************************************************
     C* Like Defines
     C*****************************************************************
     C     *LIKE         DEFINE    LTCO#         KKCO#
     C     *LIKE         DEFINE    LTSUB#        KKSUB#
     C     *LIKE         DEFINE    LTFND         KKFND
     C     *LIKE         DEFINE    LTFYR         KKFYR
     C     *LIKE         DEFINE    LTCASE        KKCASE
     C     *LIKE         DEFINE    LTLTR#        KKLTR#
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    OFOFC#        KKOFC#
     C*****************************************************************
     C* Main Processing
     C*****************************************************************
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      @@LTR#        KKLTR#
     C*****************************************************************
     C* Chain out to Request Letter File
     C*****************************************************************
     C     LTKY          CHAIN     PLT01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C* Get User Name
     C*
     C     Q1USER        CHAIN     PUP01                              99
     C                   MOVEL(P)  UPUSNM        XXUSNM           50
     C*
     C* Print Routing Page
     C*
      ***** BEG ADD ***** 01/28/10 ***********************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
      ***** BEG ADD ***** 01/28/10 ***********************************
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********          MOVE      '0'           XXPRT
     C*********     ORKY          CHAIN     POR01                              99
     C*********     *IN99         IFEQ      *OFF
     C*********                   EXCEPT    ROUTE
     C*********          ENDIF                                                  ENDIF *IN99
      ***** End Del ***** 08/26/14 *********************************************
     C*****************************************************************
     C* Get information before printing page.
     C*****************************************************************
     C*
     C* Get Provider Name/Address
     C*
     C                   SETOFF                                       16
     C                   Z-ADD     LTFEIN        KKFEIN
     C                   Z-ADD     LTMOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        XXPNAM
     C                   MOVEL(P)  PDADR2        XXPAD1
     C                   ELSE
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL(P)  PDNAME        XXPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      XXPNAM
     C                   ENDIF
     C                   MOVEL(P)  PDADR1        XXPAD1
     C                   MOVEL(P)  PDADR2        XXPAD2
     C     PDADR2        IFNE      *BLANKS
     C                   SETON                                        16
     C                   ENDIF
     C                   ENDIF                                                  PDDBA1 = 'Y'
     C                   MOVEL     PDZIP         XXPZP1            5
     C                   MOVE      PDZIP         XXPZP2            4
     C     PDCTY         CAT(P)    ',':0         XXPCSZ
     C                   CAT       PDST:1        XXPCSZ
     C                   CAT       XXPZP1:1      XXPCSZ
     C                   CAT       '-':0         XXPCSZ
     C                   CAT       XXPZP2:0      XXPCSZ
     C                   ELSE
     C                   MOVE      *BLANKS       XXPNAM           40
     C                   MOVE      *BLANKS       XXPAD1           40
     C                   MOVE      *BLANKS       XXPAD2           40
     C                   MOVE      *BLANKS       XXPCSZ           40
     C                   ENDIF                                                  ENDIF *IN99
     C*
     C* Get Patient Name
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   CALL      'SRNAME'
     C                   PARM      CMLSN         ##LAST           56
     C                   PARM      CMFSN         ##FRST           15
     C                   PARM      '1'           ##TERM            1
     C                   MOVEL     ##LAST        XXCNAM
     C                   ELSE                                                   ELSIF *IN99
     C                   MOVE      *BLANKS       XXCNAM           40
     C                   ENDIF                                                  ENDIF *IN99
     C*
     C* Get Group Name
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        XXGNAM           30
     C                   ELSE
     C                   MOVEL     *BLANKS       XXGNAM
     C                   ENDIF
     C*
     C                   Z-ADD     LTFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFSDT            8 0
     C*
     C                   Z-ADD     LTTSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTSDT            8 0
     C*
     C                   TIME                    DSDTTM
     C*
     C* Get the Office Phone Number
     C*
     C                   MOVEL(P)  UPOFF#        KKOFC#
     C     OFKY          CHAIN     POF01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     OFAREA        XXAREA
     C                   MOVEL     OFPHN#        XXPHN1
     C                   MOVE      OFPHN#        XXPHN2
     C*** BEGIN ADD *** 02/07/03 **************************************
     C                   Z-ADD     OFFARA        XXFARA
     C                   MOVEL     OFFAX#        XXFAX1
     C                   MOVE      OFFAX#        XXFAX2
     C                   MOVEL(P)  OFADR1        XXADR1
     C*
      ***** BEG ADD ***** 10/12/05 *************************************
     C     OFADR2        IFNE      *BLANKS
     C     XXADR1        CAT       ',':0         XXADR1
     C                   CAT       OFADR2:1      XXADR1
     C                   ENDIF                                                  OFFADR2 <> BLNK
      ***** END ADD ***** 10/12/05 *************************************
     C*
     C     OFCTY         CAT(P)    ',':0         XXCSZ
     C                   CAT       OFST:1        XXCSZ
     C                   MOVEL     OFZIP         XXZIP1            5
     C                   MOVE      OFZIP         XXZIP2            4
     C                   CAT       XXZIP1:1      XXCSZ
     C     XXZIP2        IFNE      *ZEROS
     C                   CAT       '-':0         XXCSZ
     C                   CAT       XXZIP2:0      XXCSZ
     C                   ENDIF
     C*** END   ADD *** 02/07/03 **************************************
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXAREA            3 0
     C                   Z-ADD     *ZEROS        XXPHN1            3 0
     C                   Z-ADD     *ZEROS        XXPHN2            4 0
     C*** BEGIN ADD *** 02/07/03 **************************************
     C                   Z-ADD     *ZEROS        XXFARA            3 0
     C                   Z-ADD     *ZEROS        XXFAX1            3 0
     C                   Z-ADD     *ZEROS        XXFAX2            4 0
     C*
      ***** BEG CHG ***** 10/12/05 *************************************
     C*************        MOVE *BLANKS   XXADR1 40
     C                   MOVE      *BLANKS       XXADR1           80
      ***** END CHG ***** 10/12/05 *************************************
     C                   MOVE      *BLANKS       XXCSZ            40
     C*** END   ADD *** 02/07/03 **************************************
     C                   ENDIF
     C*****************************************************************
     C* Print the page
     C*****************************************************************
     C                   EXCEPT    PAGES
     C                   EXCEPT    HEADER
     C*
     C     L2KY          CHAIN     PL201                              99
     C     *IN99         DOWEQ     *OFF
     C                   EXSR      CHKEND
     C                   EXCEPT    DETAIL
     C     L2KY          READE     PL201                                  99
     C                   ENDDO
     C*
     C                   EXSR      CHKEND
     C                   EXCEPT    FOOT1
     C                   EXSR      CHKEND
     C                   EXCEPT    FOOT2
     C                   EXSR      CHKEND
     C                   EXCEPT    FOOT3
     C                   EXSR      CHKEND
     C                   EXCEPT    FOOT4
     C                   EXSR      CHKEND
     C                   EXCEPT    FOOT5
     C*
     C* Update Process date of FALTP
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTPRDT
     C*
     C                   MOVE      Q1PGM         LTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LTTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTDT
     C*
     C                   UPDATE    PLT01
     C                   ENDIF                                                  ENDIF 99
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       //***** Beg Add ***** 10/28/15 ***********************************
       GetClientID(clientID);
       //***** End Add ***** 10/28/15 ***********************************
       xxPrt = '0';
       Chain orKy POR01;
       //***** Beg Chg ***** 10/28/15 ***********************************
       //If %found;
       If %found and clientID <> '114';
       //***** End Chg ***** 10/28/15 ***********************************
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   SETON                                        LR
     C*****************************************************************
     C* CHKEND - CHECKS THE END OF THE PAGE                           *
     C*****************************************************************
     C     CHKEND        BEGSR
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    PAGES
     C                   SETOFF                                       OA
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             Q1USER              26
     O**********                                 28 '-'
     O**********             XXUSNM            +001
     O**********E            ROUTE          1
     O**********E            ROUTE          1
     O**********E            ROUTE          2
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Chg ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       Q1USER              26
      ***** End Chg ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             @@QUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Chg ***** 09/01/15 *********************************************
      ***** Beg Chg ***** 10/28/15 *********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             @@QUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Chg ***** 10/28/15 *********************************************
      ***** End Chg ***** 09/01/15 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                20 '(C) COPYRIGHT Fiserv'
     O***                                41 ' Insurance Solutions,'
     O***                                58 'Inc. 2002 - 2005'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** BEG CHG ***** 01/28/10 **********************************
     OQSYSPRT   E            PAGES          1 05
     O          E            HEADER         1
     O                                            3 'To:'
     O                       XXPNAM            +002
     O          E            HEADER         1
     O                       XXPAD1            +005
     O          E    16      HEADER         1
     O                       XXPAD2            +005
     O          E            HEADER         2
     O                       XXPCSZ            +005
     O*** BEGIN CHG *** 02/07/03 **************************************
     O********E* 1             HEADER
     O**********                          5 'From:'
     O**********                       +001 'CompServices, Inc.'
     O********E* 2             HEADER
     O**********                       +006 'Repricing Services.'
     O          E            HEADER         1
     O                                            5 'From:'
     O                                         +001 'CompServices, Inc.'
     O                                         +025 'Phone: ('
     O                       XXAREA            +000
     O                                         +000 ')'
     O                       XXPHN1            +001
     O                                         +000 '-'
     O                       XXPHN2            +000
     O          E            HEADER         1
     O                                         +006 'Repricing Services.'
     O                                         +024 'Fax  : ('
     O                       XXFARA            +000
     O                                         +000 ')'
     O                       XXFAX1            +001
     O                                         +000 '-'
     O                       XXFAX2            +000
     O          E            HEADER         1
     O                       XXADR1            +006
     O          E            HEADER         2
     O                       XXCSZ             +006
     O*** END   CHG *** 02/07/03 **************************************
     O          E            HEADER         1
     O                                            3 'Re:'
     O                                         +001 'Claimant:'
     O                       XXCNAM            +001
     O          E            HEADER         1
     O                                         +004 'Group:'
     O                       XXGNAM            +001
     O                                         +003 'Claim #:'
     O                       CMFND             +001
     O                                         +000 '-'
     O                       CMFYR             +000
     O                                         +000 '-'
     O                       CMCASE            +000
     O          E            HEADER         2
     O                                         +004 'Dates of Service:'
     O                       XXFSDT            +001 '  /  /    '
     O                                         +000 '-'
     O                       XXTSDT            +000 '  /  /    '
     O          E            HEADER         3
     O                                            5 'Date:'
     O                       DSDATE            +001 '  /  /    '
     O          E            HEADER         2
     O                                            4 'Dear'
     O                       XXPNAM            +001
     O          E            HEADER         1
     O                                           20 'We are in receipt of'
     O                                           40 ' your bill for servi'
     O                                           60 'ces in connection wi'
     O                                           80 'th the above caption'
     O                                          100 'ed claim.  Please   '
     O          E            HEADER         2
     O                                           20 'be advised of the fo'
     O                                           40 'llowing:            '
     O          E            HEADER         2
     O                                           20 'Services have been c'
     O                                           40 'orrect coded, consis'
     O                                           60 'tent with Medicare g'
     O                                           80 'uidelines as follows'
     O                                          100 ':                   '
     O          E            HEADER         1
     O                                           17 'From code'
     O                                           31 'To code'
     O                                           47 'Reason'
     O          E            DETAIL         1
     O                       L2FRCD              15
     O                       L2TOCD              30
     O                       L2REAS             101
     O          E            FOOT1       1  2
     O                                           20 'If you would like to'
     O                                           40 ' discuss these chang'
     O                                           60 'es, you should conta'
     O                                           68 'ct us at'
     O                       XXAREA            +001
     O                                         +000 '-'
     O                       XXPHN1            +000
     O                                         +000 '-'
     O                       XXPHN2            +000
     O                                         +001 'within 10 days.'
     O          E            FOOT2          3
     O                                           10 'Sincerely,'
     O          E            FOOT3          1
     O                       XXUSNM              50
     O          E            FOOT4          2
     O                                           20 'Manager Repricing Se'
     O                                           40 'rvices              '
     O          E            FOOT5          1
     O*** BEGIN DEL *** 03/16/05 **************************************
     O**********                         16 'Certified Mail ('
     O**********               DSDATE  +000 '  /  /    '
     O**********                       +000 ')'
     O*** END   DEL *** 03/16/05 **************************************
