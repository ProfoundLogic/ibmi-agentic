     H* (c) 2015 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W3070R - COMMISSION CHECK REGISTER
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 09/10/15 C:136 Support: 37184                                 *
     H*                Program included EFT transactions when         *
     H*                processing MICR checks because of new EFT      *
     H*                Seq #.                                         *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*  04/17/12 C:255 Project: WC2044                               *
     H*                 Modify program to use EFT flag. Change exist- *
     H*                 ing keylist ACky14 to use the flag.           *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  01/05/09 C:136 SUPPORT: WA045 65963                          *
     H*                 CHANGE WMACL6 TO LOOK AT 'P' TYPE             *
     H*                 TRANSACTIONS ONLY. ALSO CORRECTED THE HEADINGS*
     H*                 ON THE FIRST PAGE.                            *
     H*                                                               *
     H*  09/20/07 C:136 PROJECT: WC997                                *
     H*                 IMPLEMENT LOGIC FOR EFT - AGENT COMMISSION    *
     H*                 CHECKS                                        *
     H*                                                               *
     H*  09/07/05 C:53  Project: 129WLV
     H*                 ADD FLAG FOR USING THE CURRENT DATE
     H*
     H*  12/17/04 C:132 PROJECT: WA121   SEQ: 45032                   *
     H*                 If AGCCKM NE AGMOD, access WMAGP using AGCCKM *
     H*                 Mod value and assign to Agent Name the Name   *
     H*                 from the AGCCKM Mod record.                   *
     H*                                                               *
     H*  03/29/04 C:09 PROJECT: WA129   SEQ: XXXXX                    *
     H*                Ignore commission void records.                *
     H*                                                               *
     H*  10/22/03 C:132 PROJECT: WA107   SEQ: 39325                   *
     H*                 Ignore negative commission payment records    *
     H*                 that may have been posted.                    *
     H*                                                               *
     H*  01/06/97 C:46 PROJECT: WC623                                 *
     H*                ADD POSTING DATE TO AGENT COMMISSION KEY LIST  *
     H*                                                               *
     H*****************************************************************
     FWXWPL1    IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMACL6    IF   E           K DISK
     FWMACL14   IF   E           K DISK
     F                                     RENAME(PAC01:PAC14)
     FWMAGL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D TITLE           C                   CONST('Commission Check -
     D                                     Register')
      *
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *******Beg Add********04/17/12**********************************
     D xxeftc          s                   like(ACeftc)
      *******End Add********04/17/12**********************************
     C*
     C     *LIKE         DEFINE    WPDT          XXDT
      *******Beg Del********04/17/12**********************************
     C******LIKE         DEFINE    ACCKTP        XXCKTP
      *******End Del********04/17/12**********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQFND             3
     C                   PARM                    QQCURD            1
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
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   AGENT COMMISSIONS FILE
     C*
     C     ACKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C     ACKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCHK#
     C                   KFLD                    XXDT
     C*
      *******Beg Chg********04/17/12**********************************
     C*****ACKY14        KLIST
     C*****              KFLD                    XXCO#
     C*****              KFLD                    XXFND
     C*****              KFLD                    XXCKTP
     C*****              KFLD                    XXDT
     C*
     C     ACKY14        KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C                   KFLD                    xxeftc
     C                   KFLD                    xxdt
      *******End Chg********04/17/12**********************************
     C*
     C*   WASTE CHECK FILE
     C*
     C     WPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C     WPKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCHK#
     C*
     C*   AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ACAGT#
     C*
     C*   KEY LIST FOR THE AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
      *
      *
     C     AGKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    XXCCKM
      *
     C     *LIKE         DEFINE    AGCCKM        XXCCKM
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQDATE        X1DATE            8 0
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C                   MOVE      QQFND         XXFND             3 0
     C                   TIME                    DSDTTM
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
      *
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
     C*
     C*   CHAIN TO THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C*  Center Company name in display field
     C*
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>*BLANK
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        CONAME
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFEQ      *BLANKS                                      FIELD
     C                   MOVEL     TITLE         ORTITL
     C                   ENDIF                                                  ORTITL=*BLANKS
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
     C                   END
     C*
     C*  CENTER TITLE IN FIELD
     C*
     C                   MOVEL     ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        ORTITL
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C*
     C*   GET THE STARTING CHECK#
     C*
     C                   Z-ADD     0             XXCHK#            7 0
     C*
     C                   MOVE      '0'           *IN50
     C     ACKEY         CHAIN     PAC01                              60
     C     *IN60         DOWEQ     '0'
     C     *IN50         ANDEQ     '0'
     C     ACPODT        IFNE      XXDT
     C     ACCHCK        OREQ      0
      * If Check amount is Negative, bypass this record.
     C     ACAMT         ORLT      *ZERO
      *** beg add ***** 09/10/15 **************************
     C     ACEFTC        OREQ      'E'
      *** end add ***** 09/10/15 **************************
     C     ACCOTP        OREQ      'V'
     C     ACKEY         READE     PAC01                                  60
     C                   ELSE
     C                   MOVE      '1'           *IN50
     C                   END
     C                   END
     C*
     C     WPKEY         CHAIN     PWP01                              61
     C     *IN61         DOWEQ     '0'
     C     WPTRDT        ANDNE     XXDT
     C     WPKEY         READE     PWP01                                  61
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C     *IN61         IFEQ      '0'
     C     ACCHCK        IFLT      WPCHK#
     C                   Z-ADD     ACCHCK        XXCHK#
     C                   ELSE
     C                   Z-ADD     WPCHK#        XXCHK#
     C                   END
     C                   ELSE
     C                   Z-ADD     ACCHCK        XXCHK#
     C                   END
     C                   ELSE
     C  N61              Z-ADD     WPCHK#        XXCHK#
     C                   END
     C*
     C                   Z-ADD     0             XXCKTL            7 0
     C                   Z-ADD     0             XXWCTL            7 0
     C                   Z-ADD     0             XXATOT           11 2
     C*
     C*  IF THERE ARE CHECKS WITHIN ENTERED MONTH/YEAR PRINT IN
     C*  SEQUENCE BY CHECK#, THE CHECKS ARE FOUND IN EITHER WMACP
     C*  OR WASTE CHECK FILE.
     C*
     C     XXCHK#        IFNE      *ZERO
     C*
     C                   MOVE      '0'           *IN50
     C     *IN50         DOWEQ     '0'
     C*
     C     ACKY2         CHAIN     PAC01                              60
     C     *IN60         IFEQ      '0'
     C* Exit New Subroutine to Load and Print Info
     C                   EXSR      LOAD
     C*
     C                   ELSE
     C*
     C*  IF THE CHECK WAS NOT FOUND IN THE AGENT COMM FILE SEE IF
     C*  IT IS IN THE WASTE CHECK FILE.
     C*
     C     WPKY2         CHAIN     PWP01                              61
     C     *IN61         IFEQ      '0'
     C     WPTRDT        ANDEQ     XXDT
     C*
     C*  PRINT THE WASTE CHECK INFORMATION
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    DETWP
     C                   ADD       1             XXWCTL
     C*
     C*  IF THE CHECK WAS NOT FOUND IN EITHER FILE FOR THAT MONTH/YR
     C*  WE ARE FINISHED PRINTING CHECK REGISTER.  DROP OUT IF LOOP.
     C*
     C                   ELSE
     C                   MOVE      '1'           *IN50
     C                   END
     C                   END
     C*
     C*  INCREMENT THE CHECK NUMBER AND CONTINUE IF THERE ARE MORE
     C*  CHECKS WITHIN THE SPECIFIED MONTH/YEAR.
     C*
     C                   ADD       1             XXCHK#
     C                   END
     C                   END
     C*
      * Print Check Totals, Then Go List EFT Records
     C                   Z-ADD     XXATOT        XXGTOT           11 2
     C                   Z-ADD     XXCKTL        XXCKTG            7 0
     C                   EXCEPT    TOTAL
     C                   EXSR      EFT
     C*
     C*  PRINT TOTALS
     C*
     C*
     C*  If Need, Print Headings, then Print EFT Totals at LAST RECORD
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADE
     C                   EXCEPT    HEAD1
     C                   ENDIF
     C     XXCKTL        IFGT      *ZEROS
     C                   ADD       XXATOT        XXGTOT
     C                   ADD       XXCKTL        XXCKTG
     C                   EXCEPT    TOTLE
     C                   EXCEPT    TOTLG
     C                   ENDIF
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    TOTAL
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKEY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C*===============================================================
     C* EFT - Process EFT Payments and PreNotes
     C*===============================================================
     C*
     C     EFT           BEGSR
     C*
      *******Beg Chg********04/17/12**********************************
     C*****              MOVEL(P)  'EFT'         XXCKTP
      /Free
       xxeftc = 'E';
      /End-free
      *******End Chg********04/17/12**********************************
     C*
     C* New Logical which includes ACCKTP (Check Type)
     C     ACKY14        CHAIN     WMACL14                            60
     C*
     C* If have an EFT, set on Headings Indicator
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *INOA
     C                   ENDIF
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     ACIPGM        IFEQ      'W3060R'
     C                   EXSR      LOAD
     C                   ENDIF                                                  ACCOTP=P
     C*
     C     ACKY14        READE     WMACL14                                60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  EFT
     C*
     C*===============================================================
     C* LOAD - Load INFO for Printing Checks and EFTs
     C*===============================================================
     C*
     C     LOAD          BEGSR
     C*
     C*  GET AGENT NAME
     C*
     C                   CLEAR                   AGNAME
     C     AAKEY         CHAIN     PAA01                              60
     C  N60AGKEY         CHAIN     PAG01                              60
      *
      *  If "Check Mailed to Modifier" is not equal to the "Agent Modifier",
      *  access the Agent Master file using the "Mailed to Modifier" value.
      *  Then proceed with processin.
      *
     C     AGMOD         IFNE      AGCCKM
     C                   Z-ADD     AGCCKM        XXCCKM
     C     AGKY2         CHAIN     PAG01                              60
      *
     C     *IN60         IFEQ      *ON
      *  Re-read original Agent master record
     C     AGKEY         CHAIN     PAG01                              60
     C                   ENDIF                                                  *IN51 *ON
     C                   ENDIF                                                  AGMOD NE AGCCKM
      *
     C*
     C     QQCURD        IFNE      'Y'
     C                   Z-ADD     ACPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPODT            6 0
     C                   ELSE
     C                   Z-ADD     ACIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPODT
     C                   ENDIF
     C*
     C*  Print the Agent Check Information (and EFTs)
     C*
     C     *INOA         IFEQ      *ON
     C*
     C                   EXCEPT    HEAD
     C*
      *******Beg Chg********04/17/12**********************************
     C*****              SELECT
     C*****ACCKTP        WHENEQ    'EFT'
     C*****              EXCEPT    HEADE
     C*****              ENDSL
      /Free
       Select;
          When Aceftc = 'E' ;
             Except HEADE;
       EndSl;
      /End-Free
      *******End Chg********04/17/12**********************************
     C*
     C                   EXCEPT    HEAD1
     C*
     C                   ENDIF                                                  *INOA=*ON
     C*
     C                   MOVE      *OFF          *INOA
     C*
      *******Beg Chg********04/17/12**********************************
     C*****              SELECT
     C*****ACCKTP        WHENEQ    'EFT'
     C*****ACAMT         ANDEQ     *ZERO
     C*****              EXCEPT    PRENOT
     C*****              ENDSL
      /Free
       Select;
          When ACeftc = 'E'  And ACamt = *Zeros;
             Except PRENOT;
       EndSl;
      /End-Free
      *******End Chg********04/17/12**********************************
     C*
     C                   EXCEPT    DETAC
     C*
     C                   ADD       1             XXCKTL
     C                   ADD       ACAMT         XXATOT
     C*
     C                   ENDSR                                                  LOAD
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
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
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
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
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              86
     O                       XXDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       FNFND         Z   +  1
     O                       FNNAME            +  1
     O          E            HEADE          2
     O                                         + 39 'EFT Transactions'
     O*
     O          E            HEAD1          1
     O                                           60 'CHECK'
     O          E            HEAD1          2
     O                                           16 'AGENT#  NAME'
     O                                           70 'DATE     CHECK#'
     O                                           86 'AMOUNT'
     O*
     O          E            PRENOT         0
     O                                         + 89 'Prenote'
     O*
     O          E            DETAC          1
     O                       ACAGT#        Z     09
     O                       AGNAME              52
     O                       XXPODT        Y     61
     O                       ACCHCK        Z     70
     O                       ACAMT         J     87
     O          E            DETWP          1
     O                       WPDESC              34
     O                       WPCHK#        Z     70
     O          E            TOTAL       1  2
     O                       XXCKTL        JB    13
     O                                           20 'CHECKS'
     O                       XXWCTL        JB    46
     O                                           59 'WASTE CHECKS'
     O                                           71 'TOTAL:'
     O                       XXATOT        JB    87
     O*
     O* The Total line for EFT, Printed at Last Record. Other TOTAL
     O* line has the Amounts Zeroed when Printed (B Code) so starts
     O* again with ZERO amounts for EFTs. If no EFTs-prints zero totals
     O          E            TOTLE       1  2
     O                       XXCKTL        JB    13
     O                                           27 'TOTAL OF EFTS'
     O                                           71 'TOTAL AMOUNT:'
     O                       XXATOT        JB    87
     O* Also print a Grand Total line if have EDTs printed
     O          E            TOTLG          1
     O                       XXCKTG        JB    13
     O                                           31 'NBR OF CKS & EFTS'
     O                                           71 'GRAND TOTALS:'
     O                       XXGTOT        JB    87
