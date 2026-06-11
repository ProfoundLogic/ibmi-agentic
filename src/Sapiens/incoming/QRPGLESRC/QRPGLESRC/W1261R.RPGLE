     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H/TITLE W1261R - CREATE DIRECT DEBIT TRANSMISSION FILE
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
     H bnddir('WTBNDDIR')
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*        THIS PROGRAM CREATES A TRANSMISSION FILE FROM THE      *
     H*        BATCH#. THIS BATCH NUMBER IS READ AND DATA IS WRITTEN  *
     H*        TO WAEFP IN PREPARATION FOR TRANSMISSION.              *
     H*                                                               *
     H*        A/R BATCH WILL BE UPDATED WITH THE TRANSMISSION DATE,  *
     H*        USER, AND TIME.                                        *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W126OC                                                      *
     H* CALLS TO:                                                     *
     H*   WA073R                                                      *
     H*                                                               *
     H* PROGRAMMER: JACKIE JANOWIAK                                   *
     H* CREATED: 02/08/92               PROJECT: AKU                  *
     H* REWRITTEN 04/11/03 BY C:136 PROJECT: WC1116
     H* RE-REWRITTEN 06/29/09 BY C:191 PROJECT: WC1806
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   60  GENERAL CHAIN INDICATOR                                           *
     H***************************************************************************
     H* REVISIONS:                                                              *
      *                                                                         *
      * 12/16/24 C:333 Project: WC2990                                          *
      *                A/R Batch: Update Locking Process.                       *
      *                                                                         *
      * 05/04/21 C:136 Project: WC1986                                          *
      *                Adjust EFT logic by using GetEftStatus()                 *
      *                                                                         *
      * 06/03/15 C:136 Project: MC396                                           *
      *                Add logic to populate next available EFT seq# to the     *
      *                check # field. Also, flag the transaction as EFT in      *
      *                the new EFT flag field.                                  *
      *                                                                         *
      * 06/29/09 C:191 Project: WC1806
      *                 RE-WRITE taking parts from original program             *
      *                 Change to combine with ACH process.                     *
      *                 Now calls WA073R to write EFT to WAEFP                  *
      *                                                                         *
      * 10/07/08 C:136 Support: WA139 65401                                     *
      *                Expand Immed Origin to handle 10 digit code              *
      *                                                                         *
      * 11/10/06 C:136 Support: WA129 55800                                     *
      *                Correct bank account information.                        *
      *                                                                         *
      * 10/09/06 C:136 Project: WC1435                                          *
      *                Group ACH transactions by Emp/Div                        *
      *                                                                         *
      * 04/13/04 C:132 PROJECT:  WC1190  WA109 42171                            *
      *              - If UPD mode or EAIDT = zeros, update with date           *
     H*                                                                         *
     H* 01/09/04 C:136 Project: WC1116                                          *
     H*                Use active ACH history record as of the posting date     *
     H*                of the transaction.                                      *
     H*                                                                         *
     H* 06/30/03 C:136 Project: WC1116                                          *
     H*                Do not update EADT to keep "creation date" unchanged in  *
     H*                W/W A/R Batch. Create a zero record transmission file    *
     H*                if run on zero A/R batch record.                         *
     H*                                                                         *
     H* 06/19/03 C:136 Project: WC1116                                          *
     H*                Right adjust bank name in header file record.            *
     H*                Use EMEMP# instead of EMFEIN in detail record.           *
     H*                                                                         *
     H* 06/11/03 C:136 Project: WC1116                                          *
     H*                Use CBIORG instead of COFEIN in File Header.             *
     H*                Correct type '5' record.                                 *
     H*                                                                         *
     H***************************************************************************
     FWDEALC    uf   e           k disk
     F                                     infds(dbfds)
      *** beg del**** 05/04/21 *****************
     F***wmefl6    if   e           k disk
     F***                                     infds(dbfds1)
      *** end del**** 05/04/21 *****************
    D*---------------------------------------------------------------------
    D* Data Structures
    D*---------------------------------------------------------------------
      *
     D dbfds           ds
     D  dbrrn                397    400b 0
      *
      *** beg del**** 05/04/21 *****************
     D*** dbfds1          ds
     D***  dbrrn1               397    400b 0
      *** end del**** 05/04/21 *****************
      *
     D                 DS
     Ddsdttm                         14  0
     Ddstime                          6  0 overlay(dsdttm)
     Ddsdate                          8  0 overlay(dsdttm:7)
      *
     D psds          esds                  extname(szq1p)
      *
     d xxco#           s                   like(EACO#)
     d xxbat#          s                   like(EABAT#)
     D xxbdt           s              7  0 inz                                  Batch Date
     d xxcleanup       s               n
     d xxapprvd        s               n
     d wrkDt           s               d   datfmt(*ymd)
      *
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
      *** beg add ***** 06/03/15 *******************************
     d ##NxtEft#       s              7  0
     *=========================================================
     * Prototypes
     *=========================================================
     d/copy wprbank
      //*** Beg Add ***** 12/16/24 ****************************************
     D/copy sPrLock
      //*** End Add ***** 12/16/24 ****************************************
      *** end add ***** 06/03/15 *******************************
      *** beg add ***** 05/04/21 *****************
       Dcl-S chrChkType Char(10);
       Dcl-S chrEftType Char(1) Inz('E');
       Dcl-S chrEftKey  Char(30);
       Dcl-S chrEfDt Char(7);
       Dcl-S chrEobType Char(3);
       Dcl-S chrFnd Char(3);
       Dcl-S chrFyr Char(3);
       Dcl-S chrPre Char(1);
       Dcl-S chrRRN Char(9);
       Dcl-S dbRRN1 Packed(9:0);
      *** end add ***** 05/04/21 *****************
      //*** Beg Add ***** 12/16/24 ****************************************
       Dcl-S inCo# Packed(3:0);
       Dcl-S inFndVar Packed(3:0) inz(0);
       Dcl-S inBat# Packed(9:0);
      //*** End Add ***** 12/16/24 ****************************************
     c*=========================================================================
     c*   plist
     c*=========================================================================
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     c*=========================================================================
     c*   klist
     c*=========================================================================
     c*
     c*  employer accounts receivable batch file
     c*
     c     eaky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxbat#
     c*
     c     efky          klist
     c                   kfld                    xxco#
     c                   kfld                    eafnd
     c                   kfld                    eaemp#
     c                   kfld                    eadiv
     c*=========================================================================
     c*  Entry parm list
     c*=========================================================================
     c     *entry        plist
     c                   parm                    qqco#             3
     c                   parm                    qqbat#            9
     c*=========================================================================
     c*  Main
     c*=========================================================================
     c                   time                    dsdttm
     C* convert batch date to correct format
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxbdt
      *
     c                   move      qqco#         xxco#
     c                   move      qqbat#        xxbat#
      *
      * Check for zero A/R Batch record and create zero transmission file.
     c     eaky          chain     pea01                              60

     c                   if        %found and EABATA = 0
      *
      * update the zero A/R Batch record with transmission date
     c                   move      *date         wrkDt
     c     *cymd         move      wrkDt         EAACHD
     c                   eval      EAACHT = dstime
     c                   eval      EAACHU = Q1USER
     c                   eval      EAPGM  = Q1PGM
      *
      * IF INITAL DATE IS ZEROS, UPDATE IT AND OTHER INITAL FIELDS
     C                   IF        EAIDT = *ZEROS
     C                   EVAL      EAIDT = EAACHD
     C                   MOVE      Q1PGM         EAIPGM
     C                   MOVE      Q1USER        EAIUSR
     C                   ENDIF
      *
     c                   update    pea01
     c                   else
      *
      *   loop through A/R batch file
     c                   dow       not *in60
      *
      * Check EFT information file
      *** beg chg**** 05/04/21 **************************
     C***                   exsr      srchkapr
     C***                   if        xxapprvd
       chrEfDt = %Char(%Date():*Cymd0);
       chrEftKey = %Editc(EAemp#:'X') + %Editc(EAdiv:'X');
       chrFnd = %Editc(EAfnd:'X');
       chrFyr = %Editc(EAfyr:'X');
       If GetEftStatus(qqCo#:chrEftType:chrFnd:chrEftKey:
        chrEfDt:chrPre:chrRRN:chrEobType:chrChkType:chrFyr) = *On;
          dbRRN1 = %Int(chrRRN);
      *** end chg ***** 05/04/21 **************************
      * Approved record found, process AR record
      *
     c                   exsr      srProcess
     C                   endif                                                  xxapprvd

     c     eaky          reade     pea01                                  60
     c                   enddo                                                  *IN60 = *OFF
     c*
     c                   endif                                                  EABATA = 0
      *
     C                   if        xxcleanup
      * Call API to finally cleanup
     C                   call      'WA073R'
     C                   parm      'X'           ##type
     C                   parm      *zeros        ##rrn
     C                   parm      *zeros        ##rrn1
     C                   parm      ' '           ##rtn
     C                   endif                                                  xxcleanup
      * Update data area to end job status
     C     *dtaara       define                  achbat           30
     C     *lock         in        *dtaara
     C                   movel     ' '           achbat
     C                   out       *dtaara
      //*** Beg Add ***** 12/16/24 ************************************
       If QQBAT# <> *Blanks;
          inCo# = %Dec(QQCo#:3:0);
          inBat# = %Dec(QQBat#:9:0);
          ClearPolicyLock(inCo#:inFndVar:inBat#);
       Endif;
      //*** End Add ***** 12/16/24 ************************************
     c                   seton                                        lr
      *
      *** beg del ***** 05/04/21 **************************
      **********************************************************************************
      * srchkapr - Check approval date for EFT processing                              *
      **********************************************************************************
     C***     srchkapr      begsr
     C****
     C***                   eval      xxapprvd = *off
     C***     efky          chain     pef01                              60
      ****
     C***                   dow       not *in60
     C**** find effective record, i.e. batch date in between effective and expiry dates
     C***                   if        xxbdt >= efefdt and
     C***                             (xxbdt < efexdt or efexdt = 0)
     C***                   eval      xxapprvd = *on
      **** effective record found leave loop
     C***                   leave
     C***                   endif                                                  xxbdt>= efefdt
     C***     efky          reade     pef01                                  60
     C***                   enddo                                                  *in60
      ****
     C***                   endsr
      *** end del ***** 05/04/21 **************************
     C************************************************************************
     C* srProcess - Process AR record by calling API
     C************************************************************************
     C     srProcess     begsr
      *
      * Call API to write to EFT central holding file API
     C                   call      'WA073R'
     C                   parm      'E'           ##type            1
     C                   parm      dbrrn         ##rrn             9 0
     C                   parm      dbrrn1        ##rrn1            9 0
     C                   parm      ' '           ##rtn             1
      *** beg add ***** 06/03/15 ********************************
     C                   parm                    ##NxtEft#
      *** end add ***** 06/03/15 ********************************
      *
     C                   if        ##rtn = ' '
     C                   eval      xxcleanup = *on
      *** beg add ***** 06/03/15 *******************************
      /free
       EAfl01 = 'E';
       EAchk# = ##NxtEft#;
      /end-free
      *** end add ***** 06/03/15 *******************************
      *   update the A/R batch record with transmission info
     c                   move      *date         wrkDt
     c     *cymd         move      wrkDt         EAACHD
     c                   eval      EAACHT = dstime
     c                   eval      EAACHU = Q1USER
     c                   eval      EAPGM  = Q1PGM
      *
      * IF INITAL DATE IS ZEROS, UPDATE IT AND OTHER INITAL FIELDS
     C                   IF        EAIDT = *ZEROS
     C                   EVAL      EAIDT = EAACHD
     C                   MOVE      Q1PGM         EAIPGM
     C                   MOVE      Q1USER        EAIUSR
     C                   ENDIF
      *
     c                   update    pea01
     C                   endif
      *
     C                   endsr
