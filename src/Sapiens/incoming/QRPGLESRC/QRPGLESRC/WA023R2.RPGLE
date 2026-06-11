     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA023R2 - Check Printing Sort API
     H*****************************************************************
     H* Description:  This program calculates the check printing sort *
     H*   order and loads WARPP/RPCSRT.                               *
     H*                                                               *
     H* Called by: W7001R                                             *
     H*                                                               *
     H* Created on 08/01/2006 by C:53 for Project 114WMX
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 04/04/14 C:279 Project: 129WSC  Priss:30                      *
     H*                Correct Check Print sort order                 *
     H*                                                               *
     H* 11/25/13 C:282 PROJECT: 129WSC                                *
     H*                Add Printing order enhancement                 *
     H*                                                               *
     H* 04/19/10 C:235 PROJECT: 25WRG                                 *
     H*                CLAIMS GENERAL RETROFIT                        *
     H*                                                               *
     H* 09/19/08 C:197 PROJECT: 71WJE                                 *
     H*                CHANGE PRINTER ORDER OF CLAIM CHECKS           *
     H*                                                               *
     H*****************************************************************
     FWARPLR    UF   E           K DISK
     FWMCML1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
      *******BEG ADD********11/25/13**********************************
     F*****WAR3L4    UF   E           K DISK
     FWAR3L11   UF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FWMBIL5    IF   E           K DISK
      *******END ADD********11/25/13**********************************
      *

     D                 DS
     D  XXSORT                 1     26
     D  XXSRT1                 1      1
     D  XXSRT2                 2     26  0
      *
     D                 DS
     D  XFEINM                 1     12
     D  RPFEIN                 1      9  0
     D  RPMOD                 10     12  0
      *******BEG ADD********11/25/13**********************************
     D  XXMOD          s              3
      *******END ADD********11/25/13**********************************
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C*------------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            8
     C******* beg add ****** 04/04/14 **************************************
     C                   Parm                    QQtyp             1
     C******* end add ****** 04/04/14 **************************************
     C*
     C     CMKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C     CMKY1         KLIST
     C                   KFLD                    RPCO#

     C*******BEG ADD********11/25/13**********************************
     C     BIKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    RPCKTP
     C                   KFLD                    RPFND
      *
     C
     C     BIKY1         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    RPCKTP
     C                   KFLD                    RPFND
     C                   KFLD                    XXDT
     C*******END ADD********11/25/13**********************************
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*  Key list for Bank Info File
     C*
     C     *LIKE         DEFINE    RPCO#         ##CO#
     C     *LIKE         DEFINE    RPSEQ#        ##SUB#
     C     *LIKE         DEFINE    RPFND         ##FND
     C     *LIKE         DEFINE    RPFYR         ##FYR
     C     *LIKE         DEFINE    RPCASE        ##CASE
     C     *LIKE         DEFINE    RPFEIN        ##FEIN
     C     *LIKE         DEFINE    RPMOD         ##MOD
     C     *LIKE         DEFINE    RPEOB#        ##EOB#
     C     *LIKE         DEFINE    RPSEQ#        ###DTL
     C     *LIKE         DEFINE    RPTRDT        XXDT
     C     *LIKE         DEFINE    RPFND         SVFND
     C     *LIKE         DEFINE    RPCKTP        SVCKTP
     C*******BEG ADD********11/25/13**********************************
     C     *LIKE         DEFINE    COCKFE        XXCKFE
     C     *LIKE         DEFINE    RPCO#         XXCO#
     C                   MOVE      'WC'          XXAPPL            2
     C                   MOVE      *HIVAL        XXWK90            9 0
     C*******END ADD********11/25/13**********************************
     C                   MOVE      QQDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*------------------------------------------------------------------
     C*******BEG DEL********11/25/13**********************************
     C*******BEG ADD********04/19/10**********************************
     C*****CMKY1         CHAIN     PCO01                              99
     C******IN99         IFEQ      *OFF
     C*****COCKFE        IFEQ      'Y'
     C*****              MOVEL(P)  RPFEIN        RPCSRT
     C*****              ENDIF
     C*****              ENDIF
     C*******END ADD********04/19/10**********************************
     C*******END DEL********11/25/13**********************************
     C*******BEG ADD********11/25/13**********************************
      /Free
       Setll *Loval pco01;
       Read pco01;
       Dow Not %EOF(smcol);
          xxCo#  = COco#;
          xxCKFE = COckfe;

          Select;
          When xxCKFE = *Blank;

          When xxCKFE = 'P';
             //****** beg chg ******** 04/04/14 ************************
             // Setll *Loval prp02;
             // Read  prp02;
             // Dow Not %EOF(WAR3L4) and RPco# = xxCo#;
             // Dow Not %EOF(WAR3L11) and RPco# = xxCo#;

             If QQtyp = 'G';
                Setll *Loval prp02;
                Read  prp02;
             Else;
                Setll *Loval prp01;
                Read  prp01;
             Endif;

             Dow not %eof and RPco# = xxCo#;
             //****** end chg ******** 04/04/14 ************************
                If RPtrdt <= xxDT;
                   If RPfnd <> svFND Or
                      RPcktp <> svCKTP;
                      Exsr Sr$Lmt;
                   EndIf;
                   Exsr SrSortP;
                   RPcsrt = xxSort;
                   //****** beg chg ******** 04/04/14 ************************
                   // Update prp02;

                   If QQtyp = 'G';
                      Update prp02;
                   Else;
                      Update prp01;
                   Endif;
                   //****** end chg ******** 04/04/14 ************************
                 EndIf;
             //****** beg add ******** 04/04/14 ************************
             If QQtyp = 'G';
             //****** end add ******** 04/04/14 ************************
             Read  prp02;
             //****** beg add ******** 04/04/14 ************************
             Else;
                Read  prp01;
             Endif;
             //****** end add ******** 04/04/14 ************************
             EndDo;
       Other;
      /End-Free
     C*******END ADD********11/25/13**********************************
     C*  START READING ALL VALID PAYMENT REQUEST
     C*
     C                   READ      PRP01                                  LR
     C     *INLR         DOWEQ     *OFF
     C*******BEG ADD********11/25/13**********************************
     C     RPco#         ANDEQ     xxCo#

     C     RPTRDT        IFLE      XXDT

     C     XXCKFE        IFEQ      'Y'
     C                   MOVEL(P)  RPMOD         XXMOD
     C                   EVAL      RPCSRT = %EditC(RPFEIN:'X') + xxMOD
     C                   ENDIF
      /free
       If cltId <> '114' and cltId <> '071';
          Update prp01;
       Endif;
      /end-free
     C*******END ADD********11/25/13**********************************
     C*******BEG DEL********11/25/13**********************************
     C*****RPTRDT        IFLE      XXDT
     C*******END DEL********11/25/13**********************************
     C*
     C     CLTID         IFEQ      '114'
     C*
     C     RPFND         IFEQ      600
     C                   EXSR      SRSORT
     C                   MOVEL(P)  XXSORT        RPCSRT
     C                   ENDIF
     C*
     C     RPCSRT        CAT(P)    XFEINM:0      RPCSRT
     C                   UPDATE    PRP01
     C*
     C                   ENDIF
     C     CLTID         IFEQ      '071'
     C*
     C                   EXSR      SRSORT
     C                   MOVEL(P)  XXSORT        RPCSRT
     C*
     C     RPCSRT        CAT(P)    XFEINM:0      RPCSRT
     C                   UPDATE    PRP01
     C                   ENDIF                                                  CLTID='071'
     C                   ENDIF
     C*
     C                   READ      PRP01                                  LR
     C                   ENDDO
      *******BEG ADD********11/25/13**********************************
      /Free
       EndSl;

       Read  pco01;
       EndDo;

       *InLr = *On;
      /End-Free
      *******END ADD********11/25/13**********************************
     C*
     C**************************************************************************
     C*  SRSORT - This subroutine calculates the check printing sort order.
     C**************************************************************************
     C     SRSORT        BEGSR
     C*
     C*  First, load XXSRT1 based on RPRETP.
     C*
     C                   SELECT
     C     RPRETP        WHENEQ    'M'
     C                   MOVE      '1'           XXSRT1
     C     RPRETP        WHENEQ    'C'
     C                   MOVE      '2'           XXSRT1
     C     RPRETP        WHENEQ    'E'
     C                   MOVE      '3'           XXSRT1
     C     RPRETP        WHENEQ    'L'
     C                   MOVE      '4'           XXSRT1
     C                   ENDSL
     C*
     C*
     C*         FOR CLT ID = 71
     C*
     C     CLTID         IFEQ      '071'
     C*
     C                   SELECT
     C     RPRETP        WHENEQ    'C'
     C                   MOVE      '1'           XXSRT1
     C     RPRETP        WHENEQ    'E'
     C                   MOVE      '2'           XXSRT1
     C     RPRETP        WHENEQ    'L'
     C                   MOVE      '3'           XXSRT1
     C     RPRETP        WHENEQ    'M'
     C                   MOVE      '4'           XXSRT1
     C                   ENDSL
     C*
     C                   ENDIF                                                  IFEQ '071'
     C*
     C*  Next, load XXSRT2 with CMACL#
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CMACL#        XXSRT2
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       XXSRT2
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C**************************************************************************
     C*  SR$LMT - This subroutine retrieves the second signature $ limit.
     C**************************************************************************
     C     SR$LMT        BEGSR
     C*
     C*  First, load XXSRT1 with '1' if the payment amount is over the
     C*  second signature threshold in WMBIP, else load '2'.
     C*
     C     BIKY1         SETLL     PBI01
     C     BIKY          READE     PBI01                                  99
     C*
     C   99              MOVE      *HIVAL        BI$LMT
     C*
     C                   MOVE      RPFND         SVFND
     C                   MOVE      RPCKTP        SVCKTP
     C*
     C                   ENDSR
     C**************************************************************************
     C*  SRSORTP - This subroutine calculates the check printing sort order.
     C**************************************************************************
     C     SRSORTP       BEGSR
     C*
     C*  First, load XXSRT1 with '1' if the payment amount is over the
     C*  second signature threshold in WMBIP, else load '2'.
     C*
     C     RPAMT         IFGE      BI$LMT
     C                   MOVE      '1'           XXSRT1
     C                   ELSE
     C                   MOVE      '2'           XXSRT1
     C                   ENDIF
     C*
     C*  Next, load XXSRT2 so the checks with EOBs print first, with
     C*  multiple page EOBs printing before single page EOBs.
     C*
     C*
     C                   Z-ADD     XXWK90        XXSRT2
     C*
     C     RPEOB#        IFNE      0
     C*
     C                   CALL      'FA035R'
     C                   PARM      RPCO#         ##CO#
     C                   PARM      *ZEROS        ##SUB#
     C                   PARM      RPFND         ##FND
     C                   PARM      RPFYR         ##FYR
     C                   PARM      RPCASE        ##CASE
     C                   PARM      RPFEIN        ##FEIN
     C                   PARM      RPMOD         ##MOD
     C                   PARM      RPEOB#        ##EOB#
     C                   PARM                    ###DTL
     C*
     C     XXWK90        SUB       ###DTL        XXSRT2
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
