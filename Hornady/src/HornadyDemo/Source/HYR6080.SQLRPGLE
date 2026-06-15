     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2009')
     H DFTACTGRP(*NO) ACTGRP('HDSOPT') BNDDIR('HYR6080')
      **************************************************************************
      * APPLICATION: Order Processing                                          *
      *       TITLE: Order Status Email                                        *
      *          BY: William Earl (MCA)                                        *
      *        DATE: April 2009                                                *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  10/27/2009  MCA  Add secondary email type processing.                 *
      *  11/16/2011  MCA  Changes for warranty confirmation emails.            *
      *                   1) Allow warranty message after standard intro line. *
      *                   2) Skip item detail section if only item on order is *
      *                      xWRNTY.  Otherwise, just skip xWRNTY items in     *
      *                      detail section.                                   *
      *  10/30/2012  MCA  Add shipment tracking message.                       *
      *  06/12/2013  MCA  Maintain turnaround number separation.               *
      *  08/29/2013  MCA  Add Get Loaded 2011-2019.                            *
      *  10/21/2014  MCA  Use code "P" for rep customer type, so that          *
      *                   different verbiage from HYPOETX can be retrieved     *
      *                   for retail orders than from rep orders.              *
      *  10/14/2016  MCA  SnapSafe changes.                                    *
      *  11/03/2016  MCA  Add Old Dominion tracking info to shipment emails.   *
      *  11/03/2016  MCA  Add ABF tracking info to shipment emails.            *
      *  03/10/2017  MCA  Send status emails for distributor/wholesale drop    *
      *                   ship orders.                                         *
      *  05/15/2017  MCA  Don't send dollar amounts for wholsale/dealer drop   *
      *                   ship orders.                                         *
      *  06/16/2017  MCA  Increase maximum tracking URL length from 75 to 100. *
      *  12/05/2017  MCA  Add FedEx tracking info to shipment emails.          *
      *  08/16/2018  MCA  Add donation order specific emails.                  *
      *  01/25/2019  MCA  Fix donation order specific "shipped" emails.  They  *
      *                   had not been working because the subject line was    *
      *                   mistakenly more than 44 characters (by using the     *
      *                   wrong constructs).                                   *
      *  02/17/2020  MCA  Send *BCC email to address specified in data area    *
      *                   DONEMAIL1 for monitoring.                            *
      *  01/12/2021  MCA  Add Get Loaded 2020-2029.                            *
      *  09/13/2021  GH   Send the message via a Profound.js script (HYN6080)  *
      *  10/01/2021  MCA  Add email-friendly carrier descriptions and tracking *
      *                   for ship-via FN, F2, F3, FT, and FF.                 *
      *  10/15/2021  MCA  Allow "donation" email *BCC for shipped orders       *
      *                   seperate from other status emails.  This is so Abbie *
      *                   can get the "shipped" emails, but not get the        *
      *                   "confirmation" emails.                               *
      *  01/11/2022  GH   Remove the Totals from drop ship emails              *
      *  08/16/2023  MCA  Retrieve Varsity tracking numbers from Varsity's     *
      *                   files, if applicable.                                *
      *  08/16/2023  MCA  Add tracking of FedEx One Rate shipments.            *
      *  11/25/2024  MCA  Remove CPF2103 errors from the job log for ADDLIBLE. *
      *  04/06/2026  MCA  Add email-friendly carrier description and tracking  *
      *                   for ship-via FW.                                     *
      *  04/14/2026  MCA  Add tracking of USPS shipments.  Prior to this       *
      *                   change, only PM had tracking.                        *
      *                                                                        *
      **************************************************************************

     FOEORHD    IF   E           K DISK
     FOEORDT    IF   E           K DISK
     FOEOCMT    IF   E           K DISK
     FOEORHP01  IF   E           K DISK
     FOEORDP01  IF   E           K DISK
     FOEIVHH01  IF   E           K DISK
     FHYPTDTA   IF   E           K DISK
     FHYPCLMS   IF   E           K DISK
     FHDCUST    IF   E           K DISK
     FHDDSHP2   IF   E           K DISK
     FHDSHPV    IF   E           K DISK    USROPN
     FHYPOETX   IF   E           K DISK
     FHYPOECT   IF   E             DISK    USROPN BLOCK(*NO)
     FHYPOELG   O    E             DISK    BLOCK(*NO)
     FHYPOELD   O    E             DISK    BLOCK(*NO)

     D/EJECT
      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR6080         PR                  EXTPGM('HYR6080')
     D  Order#                        8P 0 CONST
     D  Turn#                         9P 0 CONST
     D  EmailType                    10A   CONST
     D  SendTo                        1A   CONST
     D  Source                       10A   CONST
     D  ShutDown                      1N   CONST
     D  RtnCode                       2A

     D QCMDEXC         PR                  EXTPGM('QCMDEXC')
     D  Command                    4096A   CONST
     D  CommandLen                   15P 5 CONST

     D QMHRMVPM        PR                  EXTPGM('QMHRMVPM')                   Remove program msgs
     D  out_MsgStack                 10A   CONST
     D  out_MsgStkCtr                10I 0 CONST
     D  out_MsgKey                    4A   CONST
     D  out_MsgToRmv                 10A   CONST
     D  io_MsgErrCode                10I 0

     D HYN6080         PR                  EXTPGM('HYN6080')
     D  xToAddr                     300A   CONST
     D  xCcAddr                     300A   CONST
     D  xBcAddr                     300A   CONST
     D  xSubject                     52A   CONST
     D  xTOPMSG                    5000A   CONST
     D  xSHPITEMS                  5000A   CONST
     D  xTotalAmt                    30A   CONST
     D  xBCKITEMS                  5000A   CONST
     D  xBCKAmt                      30A   CONST
     D  xNOBCKITEMS                5000A   CONST
     D  xNOBCKAmt                    30A   CONST
     D  xBTMMSG                    5000A   CONST
     D  xMsg                       5342A   CONST
     D  xShowPrice                    1A   CONST

     D RtvEmailAddr    PR            64A

     D Trackable       PR             1N

     D ShipVia         PR            30A

     D TrackingNbr     PR            30A

     D TrackWeb        PR           100A

     D EditPrice       PR            17A

     D EditQty         PR            17A

      /COPY *LIBL/QPRPSRC,HYR9934

      **************************************************************************
      * Define *Entry paramter list.                                           *
      **************************************************************************
     D HYR6080         PI
     D  in_Order#                     8P 0 CONST
     D  in_Turn#                      9P 0 CONST
     D  in_EmailType                 10A   CONST
     D  in_SendTo                     1A   CONST
     D  in_Source                    10A   CONST
     D  in_ShutDown                   1N   CONST
     D  out_RtnCode                   2A

      **************************************************************************
      * Define program status data structure.                                  *
      **************************************************************************
     D/COPY QCPYILE,GUIPGMDS

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D DatabaseID      S              2A   DTAARA(DATABASEID)
     D wk_DonEmail1    S             60A   DTAARA(DONEMAIL1)
     D wk_DonEmail2    S             60A   DTAARA(DONEMAIL2)

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Order#       S                   LIKE(OEORD#)
     D wk_Turn#        S              9P 0
     D wk_EmailType    S             10A
     D wk_EmailType2   S             10A
     D wk_SendTo       S              1A
     D wk_Source       S             10A
     D wk_ShutDown     S              1N
     D wk_RtnCode      S              2A
     D wk_EmailAddr    S                   LIKE(CLEMAL)
     D wk_CustType     S                   LIKE(ETCTYP)
     D wk_Qty          S                   LIKE(ODQORD)
     D wk_TotalAmt     S             11P 2
     D wk_ExtPrc       S             11P 2
     D wk_MaxQtyLen    S              1P 0
     D wk_BOFnd        S              1N   INZ(*OFF)
     D wk_Subject1     S             27A
     D wk_Subject2     S             22A
     D wk_PO#          S             46A
     D wk_Donation     S              1N
     D wk_RcdCnt       S              5P 0
     D wk_CmdErr       S              1N   INZ(*OFF)
     D wk_TOINTNET     S            300A

     D wk_TOADDR       S            300A
     D wk_CCADDR       S            300A
     D wk_BCADDR       S            300A
     D wk_SHPITEMS     S           5000A
     D wk_BCKITEMS     S           5000A
     D wk_BCKAmt       S             11P 2
     D wk_NOBCKITEMS   S           5000A
     D wk_NOBCKAmt     S             11P 2
     D wk_TOPMSG       S           5000A
     D wk_BTMMSG       S           5000A
     D wk_ShowPrice    S              1A
     D wk_Command      S           5000A
     D tempCount       S              8P 0
     D sendMsg         S               N

     D wk_DSTD         S             52A
     D wk_LONGMSG      S           5009A
     D wk_EmailCmd     S           5342A
     D wk_CmdLen       S             15P 5
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D out_MsgID       S              7A
     D out_MsgFile     S             20A   INZ('GUPMSGF   *LIBL     ')
     D out_MsgData     S            512A
     D out_MsgDtaLen   S             10I 0 INZ(%len(wk_MsgData))
     D out_MsgType     S             10A   INZ('*INFO')
     D out_MsgStack    S             10A
     D out_MsgStkCtr   S             10I 0
     D out_MsgKey      S              4A
     D out_MsgToRmv    S             10A   INZ('*ALL')
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0
     D kf_CODE         S                   LIKE(ETCODE)
     D kf_CTYP         S                   LIKE(ETCTYP)
     D kf_POS          S                   LIKE(ETPOS)

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D Subject1a       C                   'Your Hornady order'
     D Subject1b       C                   'Your Hornady warranty order'
     D Subject1c       C                   'Your Hornady donation order'
     D Subject2a       C                   'Hornady order'
     D Subject2b       C                   'Hornady warranty order'
     D Subject2c       C                   'Hornady donation order'
     D rec_Subject     C                   ' has been received.'
     D rec_Text1       C                   'Your Hornady order has been receive-
     D                                     d.  The order number is '
     D rec_Text2       C                   'Your Hornady order has been receive-
     D                                     d, pending review.  The order number-
     D                                      is '
     D rec_Text3       C                   'The following items have been order-
     D                                     ed:'
     D rls_Subject     C                   ' has been released.'
     D rls_Text1       C                   'Your Hornady order has been release-
     D                                     d.  The order number is '
     D rls_Text2       C                   'The following items are ordered:'
     D phn_Subject     C                   ' confirmation.'
     D phn_Text1a      C                   'This email is to confirm your Horna-
     D                                     dy order.  The order number is '
     D phn_Text1b      C                   'This email is to confirm your Horna-
     D                                     dy warranty order.  The order number-
     D                                      is '
     D phn_Text1c      C                   'This email is to confirm your Horna-
     D                                     dy donation order.  The order number-
     D                                      is '
     D phn_Text2       C                   'The following items have been order-
     D                                     ed:'
     D shp_Subject1    C                   ' has been shipped.'
     D shp_Subject2    C                   ' shipped.'
     D shp_Text1a      C                   'Your Hornady order has been shipped-
     D                                     .  The order number is '
     D shp_Text1b      C                   'Your Hornady warranty order has bee-
     D                                     n shipped.  The order number is '
     D shp_Text1c      C                   'Your Hornady donation order has bee-
     D                                     n shipped.  The order number is '
     D shp_Text2       C                   'The following items have been shipp-
     D                                     ed:'
     D shp_Text3       C                   'The following items are on backorde-
     D                                     r:'
     D shp_Text4       C                   'The following items on this order w-
     D                                     ill not be shipped:'
     D shp_Text5a      C                   'The '
     D shp_Text5b      C                   ' tracking number is '
     D shp_Text5c      C                   '.  You can track your shipment at '
     D shp_Text5d      C                   'The carrier for this shipment is '
     D shp_Text5e      C                   '.'
     D shp_Text6       C                   'The shipped total is $'
     D chg_Subject1    C                   ' has been changed.'
     D chg_Subject2    C                   ' changed.'
     D chg_Text1a      C                   'Your Hornady order has been changed-
     D                                     .  The order number is '
     D chg_Text1b      C                   'Your Hornady warranty order has bee-
     D                                     n changed.  The order number is '
     D chg_Text2       C                   'The order now consists of the follo-
     D                                     wing items:'
     D can_Subject1    C                   ' has been cancelled.'
     D can_Subject2    C                   ' cancelled.'
     D can_Text1a      C                   'Your Hornady order has been cancell-
     D                                     ed.  The order number is '
     D can_Text1b      C                   'Your Hornady warranty order has bee-
     D                                     n cancelled.  The order number is '
     D can_Text1c      C                   'Your Hornady donation order has bee-
     D                                     n cancelled.  The order number is '
     D can_Text2       C                   'The following items on this order w-
     D                                     ill not be shipped:'
     D can_Text6       C                   'The cancelled total is $'
     D po_Text         C                   ' and the P.O. number is '
     D NewLine         C                   ':/N'
     D NewParagraph    C                   ':/P'
     D digits          C                   '0123456789'

     C/EJECT
      **************************************************************************
      * Main procedure.                                                        *
      **************************************************************************
     C                   EVAL      wk_CmdErr = *OFF

     C                   EVAL      wk_Command = 'ADDLIBLE ' +
     C                               'LIB(PROFOUNDJS) ' +
     C                               'POSITION(*LAST)'
     C                   EVAL      wk_CmdLen = %len(%trimr(wk_Command))
     C                   CALLP(E)  QCMDEXC (wk_Command:wk_CmdLen)
     C                   IF        %error
     C                   EVAL      wk_CmdErr = *ON
     C                   ENDIF                                                  %error

     C                   EVAL      wk_Command = 'ADDLIBLE ' +
     C                               'LIB(PROFOUNDUI) ' +
     C                               'POSITION(*LAST)'
     C                   EVAL      wk_CmdLen = %len(%trimr(wk_Command))
     C                   CALLP(E)  QCMDEXC (wk_Command:wk_CmdLen)
     C                   IF        %error
     C                   EVAL      wk_CmdErr = *ON
     C                   ENDIF                                                  %error

     C                   IF        wk_CmdErr
     C                   EXSR      $RMVPM
     C                   EVAL      wk_CmdErr = *OFF
     C                   ENDIF                                                  wkCmdErr

     C                   EVAL      wk_Order# = in_Order#
     C                   EVAL      wk_Turn# = in_Turn#
     C                   EVAL      wk_EmailType = in_EmailType
     C                   EVAL      wk_SendTo = in_SendTo
     C                   EVAL      wk_Source = in_Source
     C                   EVAL      wk_ShutDown = in_ShutDown
     C                   EVAL      wk_RtnCode = *Zeros

     C                   SELECT

     C                   WHEN      wk_ShutDown

     C                   EVAL      *INLR = *ON

     C                   OTHER

     C                   DO

      *
      * Only send emails if in the production environment.
      *
     C                   IN        DatabaseID
     C                   IF        DatabaseID <> 'HY'
     C                               and  DatabaseID <> 'SS'
     C                   LEAVE
     C                   ENDIF                                                  DatabaseID<>'HY'

      *
      * Retrieve order email control record.
      *
     C                   IF        not %open(HYPOECT)
     C                   OPEN      HYPOECT
     C     1             CHAIN     HYPOECT
     C                   ENDIF                                                  not %open(HYPOECT)
     C                   IF        not %found(HYPOECT)
     C                   EVAL      wk_RtnCode = '01'
     C                   EXSR      $WrtLog
     C                   LEAVE
     C                   ENDIF                                                  not %found(HYPOECT)

      *
      * Override the send-to control fields, if requested.
      *
     C                   IF        wk_SendTo = 'Y'  or                          Customer and BCC
     C                               wk_SendTo = 'C'  or                        Customer only
     C                               wk_SendTo = 'B'                            BCC only
     C                   EVAL      ECSND1 = wk_SendTo
     C                   EVAL      ECSND2 = wk_SendTo
     C                   EVAL      ECSND3 = wk_SendTo
     C                   EVAL      ECSND4 = wk_SendTo
     C                   EVAL      ECSND5 = wk_SendTo
     C                   EVAL      ECSND6 = wk_SendTo
     C                   EVAL      ECSND7 = wk_SendTo
     C                   ENDIF                                                  wk_SendTo='Y' or

      *
      * If email is not setup to be sent, leave program.
      *
     C                   IF        (wk_EmailType = 'RECEIVED'  and
     C                               (ECSND1 = 'N'  or  ECSND1 = *Blank))  or
     C                             (wk_EmailType = 'REVIEW'  and
     C                               (ECSND2 = 'N'  or  ECSND2 = *Blank))  or
     C                             (wk_EmailType = 'RELEASED'  and
     C                               (ECSND3 = 'N'  or  ECSND3 = *Blank))  or
     C                             (wk_EmailType = 'PHONE'  and
     C                               (ECSND4 = 'N'  or  ECSND4 = *Blank))  or
     C                             (wk_EmailType = 'SHIPPED'  and
     C                               (ECSND5 = 'N'  or  ECSND5 = *Blank))  or
     C                             (wk_EmailType = 'CHANGED'  and
     C                               (ECSND6 = 'N'  or  ECSND6 = *Blank))  or
     C                             (wk_EmailType = 'CANCELLED'  and
     C                               (ECSND7 = 'N'  or  ECSND7 = *Blank))
     C                   LEAVE
     C                   ENDIF                                                  wk_EmailType='RECEIV

      *
      * Retrieve order header.
      *
     C     wk_Order#     CHAIN     OEORHD
     C                   IF        not %found(OEORHD)
     C                   EVAL      wk_RtnCode = '02'
     C                   EXSR      $WrtLog
     C                   LEAVE
     C                   ENDIF                                                  not %found(OEORHD)

      *
      * Retrieve customer email address.
      *
     C                   EVAL      wk_EmailAddr = RtvEmailAddr()
     C                   EVAL      wk_TOADDR = wk_EmailAddr
     C                   IF        wk_EmailAddr = *Blanks
     C                   IF        OELOC# <> 91
     C                   EVAL      wk_RtnCode = '03'
     C                   EXSR      $WrtLog
     C                   ENDIF                                                  OELOC#<>91
     C                   LEAVE
     C                   ENDIF                                                  wk_EmailAddr=*Blanks

      *
      * Retrieve customer master record.
      *
     C     OEBLTO        CHAIN     HDCUST
     C                   IF        not %found(HDCUST)
     C                   EVAL      CMCCLS = *Blanks
     C                   ENDIF                                                  not %found(HDCUST)

      *
      * Determine customer type.
      *
     C                   SELECT
     C                   WHEN      CMCCLS = 'RP'  or                            Domestic sales rep
     C                               CMCCLS = 'SF'                              Foreign sales rep
     C                   EVAL      wk_CustType = 'P'
     C                   WHEN      OELOC# = 91                                  Wholesale/Distributo
     C                   EVAL      wk_CustType = 'W'
     C                   WHEN      OELOC# = 93                                  Dealer
     C                   EVAL      wk_CustType = 'D'
     C                   WHEN      OELOC# = 95                                  Retail
     C                   EVAL      wk_CustType = 'R'
     C                   OTHER
     C                   EVAL      wk_CustType = *Blanks
     C                   ENDSL

      *
      * Determine whether order is a donation.
      *
     C                   IF        %subst(OEORRF:1:2) = 'D-'
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from OEORDT
                where ODORD#=:wk_Order# and ODSLPR<>0;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_Donation = *ON
     C                   ELSE
     C                   EVAL      wk_Donation = *OFF
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ELSE
     C                   EVAL      wk_Donation = *OFF
     C                   ENDIF                                                  %subst(OEORRF:1:2)=

      *
      * Determine email type.
      *
     C                   SELECT
     C                   WHEN      OEORTY = 'W'
     C                   SELECT
     C                   WHEN      wk_EmailType = 'RECEIVED'
     C                   EVAL      wk_EmailType2 = 'WTYRECEIVE'
     C                   WHEN      wk_EmailType = 'REVIEW'
     C                   EVAL      wk_EmailType2 = 'WTYREVIEW'
     C                   WHEN      wk_EmailType = 'RELEASED'
     C                   EVAL      wk_EmailType2 = 'WTYRELEASE'
     C                   WHEN      wk_EmailType = 'PHONE'
     C                   EVAL      wk_EmailType2 = 'WTYPHONE'
     C                   WHEN      wk_EmailType = 'SHIPPED'
     C                   EVAL      wk_EmailType2 = 'WTYSHIP'
     C                   WHEN      wk_EmailType = 'CHANGED'
     C                   EVAL      wk_EmailType2 = 'WTYCHANGE'
     C                   WHEN      wk_EmailType = 'CANCELLED'
     C                   EVAL      wk_EmailType2 = 'WTYCANCEL'
     C                   ENDSL
     C                   WHEN      wk_Donation                                  Donation order
     C                   SELECT
     C                   WHEN      wk_EmailType = 'RECEIVED'
     C                   LEAVE
     C                   WHEN      wk_EmailType = 'REVIEW'
     C                   LEAVE
     C                   WHEN      wk_EmailType = 'RELEASED'
     C                   EVAL      wk_EmailType2 = 'DONRELEASE'
     C                   WHEN      wk_EmailType = 'PHONE'
     C                   EVAL      wk_EmailType2 = 'DONPHONE'
     C                   WHEN      wk_EmailType = 'SHIPPED'
     C                   EVAL      wk_EmailType2 = 'DONSHIP'
     C                   WHEN      wk_EmailType = 'CHANGED'
     C                   LEAVE
     C                   WHEN      wk_EmailType = 'CANCELLED'
     C                   EVAL      wk_EmailType2 = 'DONCANCEL'
     C                   ENDSL
     C                   WHEN      %subst(OEORRF:1:4) = 'GL8 ' or               Get Loaded Promo
     C                               %subst(OEORRF:1:4) = 'GL9 ' or
     C                               %subst(OEORRF:1:5) = 'GL10 ' or
     C                               %subst(OEORRF:1:5) = 'GL11 ' or
     C                               %subst(OEORRF:1:5) = 'GL12 ' or
     C                               %subst(OEORRF:1:5) = 'GL13 ' or
     C                               %subst(OEORRF:1:5) = 'GL14 ' or
     C                               %subst(OEORRF:1:5) = 'GL15 ' or
     C                               %subst(OEORRF:1:5) = 'GL16 ' or
     C                               %subst(OEORRF:1:5) = 'GL17 ' or
     C                               %subst(OEORRF:1:5) = 'GL18 ' or
     C                               %subst(OEORRF:1:5) = 'GL19 ' or
     C                               %subst(OEORRF:1:5) = 'GL20 ' or
     C                               %subst(OEORRF:1:5) = 'GL21 ' or
     C                               %subst(OEORRF:1:5) = 'GL22 ' or
     C                               %subst(OEORRF:1:5) = 'GL23 ' or
     C                               %subst(OEORRF:1:5) = 'GL24 ' or
     C                               %subst(OEORRF:1:5) = 'GL25 ' or
     C                               %subst(OEORRF:1:5) = 'GL26 ' or
     C                               %subst(OEORRF:1:5) = 'GL27 ' or
     C                               %subst(OEORRF:1:5) = 'GL28 ' or
     C                               %subst(OEORRF:1:5) = 'GL29 '
     C                   SELECT
     C                   WHEN      wk_EmailType = 'RECEIVED'
     C                   EVAL      wk_EmailType2 = 'GLRECEIVE'
     C                   WHEN      wk_EmailType = 'REVIEW'
     C                   EVAL      wk_EmailType2 = 'GLREVIEW'
     C                   WHEN      wk_EmailType = 'RELEASED'
     C                   EVAL      wk_EmailType2 = 'GLRELEASE'
     C                   WHEN      wk_EmailType = 'PHONE'
     C                   EVAL      wk_EmailType2 = 'GLPHONE'
     C                   WHEN      wk_EmailType = 'SHIPPED'
     C                   EVAL      wk_EmailType2 = 'GLSHIP'
     C                   WHEN      wk_EmailType = 'CHANGED'
     C                   EVAL      wk_EmailType2 = 'GLCHANGE'
     C                   WHEN      wk_EmailType = 'CANCELLED'
     C                   EVAL      wk_EmailType2 = 'GLCANCEL'
     C                   ENDSL
     C                   OTHER
     C                   EVAL      wk_EmailType2 = wk_EmailType
     C                   ENDSL

      *
      * If a "SHIPPED" email is being sent, retrieve in-process record data.
      *
     C                   IF        wk_EmailType = 'SHIPPED'
     C                   IF        not %open(OEORHP01)
     C                   OPEN      OEORHP01
     C                   ENDIF                                                  not %open(OEORHP01)
     C     wk_Order#     SETLL     OEORHP01
     C                   DOU       %eof(OEORHP01)  or  (IHSTAT = 'S'  or
     C                               IHSTAT = 'X'  or  IHSTAT = 'B') and
     C                              (wk_Turn# = *Zero  or  IHTURN = wk_Turn#)
     C     wk_Order#     READE     OEORHP01
     C                   ENDDO                                                  %eof(OEORHP01) or
     C                   IF        %eof(OEORHP01)
     C                   EVAL      IHSVSV = OESVSV
     C                   EVAL      IHAIV# = *Zero
     C                   ENDIF                                                  %eof(OEORHP01)
     C     wk_Turn#      CHAIN     HYPTDTA
     C                   IF        not %found(HYPTDTA)
     C                   EVAL      TDALOC = *Blanks
     C                   EVAL      TDTRK# = *Blanks
     C                   EVAL      TDVORD = *Blanks
     C                   ENDIF                                                  not %found(HYPTDTA)
     C                   ENDIF                                                  wk_EmailType='SHIPPE

      *
      * Retrieve order summary data.
      *
     C                   EXSR      $RtvOrdSumm

     C                   EVAL      wk_TOINTNET = 'TOINTNET('

      *
      * If email to be sent to the customer, load address.
      *
     C                   IF        (wk_EmailType = 'RECEIVED'  and
     C                               (ECSND1 = 'Y'  or  ECSND1 = 'C'))  or
     C                             (wk_EmailType = 'REVIEW'  and
     C                               (ECSND2 = 'Y'  or  ECSND2 = 'C'))  or
     C                             (wk_EmailType = 'RELEASED'  and
     C                               (ECSND3 = 'Y'  or  ECSND3 = 'C'))  or
     C                             (wk_EmailType = 'PHONE'  and
     C                               (ECSND4 = 'Y'  or  ECSND4 = 'C'))  or
     C                             (wk_EmailType = 'SHIPPED'  and
     C                               (ECSND5 = 'Y'  or  ECSND5 = 'C'))  or
     C                             (wk_EmailType = 'CHANGED'  and
     C                               (ECSND6 = 'Y'  or  ECSND6 = 'C'))  or
     C                             (wk_EmailType = 'CANCELLED'  and
     C                               (ECSND7 = 'Y'  or  ECSND7 = 'C'))
     C                   EVAL      wk_TOINTNET = %trimr(wk_TOINTNET) + '(' +
     C                               %trimr(wk_EmailAddr) + ' *PRI)'
     C                   ENDIF                                                  wk_EmailType='RECEIV

      *
      * If email to be sent to the BCC addresses, load them.
      *
     C                   EVAL      wk_BCADDR = *blanks
     C                   IF        (wk_EmailType = 'RECEIVED'  and
     C                               (ECSND1 = 'Y'  or  ECSND1 = 'B'))  or
     C                             (wk_EmailType = 'REVIEW'  and
     C                               (ECSND2 = 'Y'  or  ECSND2 = 'B'))  or
     C                             (wk_EmailType = 'RELEASED'  and
     C                               (ECSND3 = 'Y'  or  ECSND3 = 'B'))  or
     C                             (wk_EmailType = 'PHONE'  and
     C                               (ECSND4 = 'Y'  or  ECSND4 = 'B'))  or
     C                             (wk_EmailType = 'SHIPPED'  and
     C                               (ECSND5 = 'Y'  or  ECSND5 = 'B'))  or
     C                             (wk_EmailType = 'CHANGED'  and
     C                               (ECSND6 = 'Y'  or  ECSND6 = 'B'))  or
     C                             (wk_EmailType = 'CANCELLED'  and
     C                               (ECSND7 = 'Y'  or  ECSND7 = 'B'))
     C                   IF        ECBCC1 <> *Blanks
     C                   EVAL      wk_TOINTNET = %trimr(wk_TOINTNET) + ' (' +
     C                               %trimr(ECBCC1) + ' *BCC)'
     C                   EVAL      wk_BCADDR = ECBCC1
     C                   ENDIF                                                  ECBCC1<>*Blanks
     C                   IF        ECBCC2 <> *Blanks
     C                   EVAL      wk_TOINTNET = %trimr(wk_TOINTNET) + ' (' +
     C                               %trimr(ECBCC2) + ' *BCC)'
     C                   EVAL      wk_BCADDR = %trimr(wk_BCADDR) + ',' + ECBCC2
     C                   ENDIF                                                  ECBCC2<>*Blanks
     C                   IF        ECBCC3 <> *Blanks
     C                   EVAL      wk_TOINTNET = %trimr(wk_TOINTNET) + ' (' +
     C                               %trimr(ECBCC3) + ' *BCC)'
     C                   EVAL      wk_BCADDR = %trimr(wk_BCADDR) + ',' + ECBCC3
     C                   ENDIF                                                  ECBCC3<>*Blanks
     C                   ENDIF                                                  wk_EmailType='RECEIV
     C                   IF        wk_Donation
     C                   SELECT
     C                   WHEN      wk_EmailType = 'SHIPPED'
     C                   IN        wk_DonEmail2
     C                   IF        wk_DonEmail2 <> '*NONE'  and
     C                               wk_DonEmail2 <> *Blanks
     C                   EVAL      wk_TOINTNET = %trimr(wk_TOINTNET) +
     C                               ' (' + %trimr(wk_DonEmail2) + ' *BCC)'
     C                   eval      wk_BCADDR = %trimr(wk_BCADDR) +
     C                             ',' +  %trimr(wk_DonEmail2)
     C                   ENDIF                                                  wk_DonEmail2<>*Blank
     C                   OTHER
     C                   IN        wk_DonEmail1
     C                   IF        wk_DonEmail1 <> '*NONE'  and
     C                               wk_DonEmail1 <> *Blanks
     C                   EVAL      wk_TOINTNET = %trimr(wk_TOINTNET) +
     C                               ' (' + %trimr(wk_DonEmail1) + ' *BCC)'
     C                   eval      wk_BCADDR = %trimr(wk_BCADDR) +
     C                             ',' +  %trimr(wk_DonEmail1)
     C                   ENDIF                                                  wk_DonEmail1<>*Blank
     C                   ENDSL
     C                   ENDIF                                                  wk_Donation
     C                   IF        OEORTY = 'W'
     C                               or  wk_Donation
     C                   EVAL      wk_TOINTNET = %trimr(wk_TOINTNET) +
     C                               ' (bearl@hornady.com *BCC)'
     C                   eval      wk_BCADDR = %trimr(wk_BCADDR) +
     C                             ', bearl@hornady.com'
     C                   ENDIF                                                  OEORTY='W'

     C                   EVAL      wk_TOINTNET = %trimr(wk_TOINTNET) + ')'

      *
      * Load the email subject line.
      *
     C                   IF        OEORTY = 'W'
     C                   EVAL      wk_Subject1 = Subject1b
     C                   EVAL      wk_Subject2 = Subject2b
     C                   ELSE
     C                   IF        wk_Donation
     C                   EVAL      wk_Subject1 = Subject1c
     C                   EVAL      wk_Subject2 = Subject2c
     C                   ELSE
     C                   EVAL      wk_Subject1 = Subject1a
     C                   EVAL      wk_Subject2 = Subject2a
     C                   ENDIF                                                  wk_Donation
     C                   ENDIF                                                  OEORTY='W'

     C                   SELECT
     C                   WHEN      wk_EmailType = 'RECEIVED'  or
     C                               wk_EmailType = 'REVIEW'
     C                   EVAL      wk_DSTD = 'DSTD(''' + %trimr(wk_Subject1) +
     C                               ' ' + %triml(%editc(wk_Order#:'Z')) +
     C                               rec_Subject + ''')'
     C                   WHEN      wk_EmailType = 'RELEASED'
     C                   EVAL      wk_DSTD = 'DSTD(''' + %trimr(wk_Subject1) +
     C                               ' ' + %triml(%editc(wk_Order#:'Z')) +
     C                               rls_Subject + ''')'
     C                   WHEN      wk_EmailType = 'PHONE'
     C                   EVAL      wk_DSTD = 'DSTD(''' + %trimr(wk_Subject2) +
     C                               ' ' + %triml(%editc(wk_Order#:'Z')) +
     C                               phn_Subject + ''')'
     C                   WHEN      wk_EmailType = 'SHIPPED'
     C                   IF        OEORTY = 'W'
     C                               or  wk_Donation
     C                   EVAL      wk_DSTD = 'DSTD(''' + %trimr(wk_Subject2) +
     C                               ' ' + %triml(%editc(wk_Order#:'Z')) +
     C                               shp_Subject2 + ''')'
     C                   ELSE
     C                   EVAL      wk_DSTD = 'DSTD(''' + %trimr(wk_Subject1) +
     C                               ' ' + %triml(%editc(wk_Order#:'Z')) +
     C                               shp_Subject1 + ''')'
     C                   ENDIF                                                  OEORTY='W'
     C                   WHEN      wk_EmailType = 'CHANGED'
     C                   IF        OEORTY = 'W'
     C                   EVAL      wk_DSTD = 'DSTD(''' + %trimr(wk_Subject2) +
     C                               ' ' + %triml(%editc(wk_Order#:'Z')) +
     C                               chg_Subject2 + ''')'
     C                   ELSE
     C                   EVAL      wk_DSTD = 'DSTD(''' + %trimr(wk_Subject1) +
     C                               ' ' + %triml(%editc(wk_Order#:'Z')) +
     C                               chg_Subject1 + ''')'
     C                   ENDIF                                                  OEORTY='W'
     C                   WHEN      wk_EmailType = 'CANCELLED'
     C                   IF        OEORTY = 'W'
     C                   EVAL      wk_DSTD = 'DSTD(''' + %trimr(wk_Subject2) +
     C                               ' ' + %triml(%editc(wk_Order#:'Z')) +
     C                               can_Subject2 + ''')'
     C                   ELSE
     C                   EVAL      wk_DSTD = 'DSTD(''' + %trimr(wk_Subject2) +
     C                               ' ' + %triml(%editc(wk_Order#:'Z')) +
     C                               can_Subject1 + ''')'
     C                   ENDIF                                                  OEORTY='W'
     C                   ENDSL

     C                   IF        DatabaseID = 'SS'
     C                   EVAL      wk_DSTD =
     C                               %scanrpl('Hornady':'SnapSafe':wk_DSTD)
     C                   ENDIF                                                  DatabaseID='SS'

     C                   EVAL      wk_LONGMSG = 'LONGMSG('''
     C                   EVAL      wk_SHPITEMS = *blanks
     C                   EVAL      wk_BCKITEMS = *blanks
     C                   EVAL      wk_BCKAmt = *zeros
     C                   EVAL      wk_NOBCKAmt = *zeros
     C                   EVAL      wk_NOBCKITEMS = *blanks
     C                   EVAL      wk_BTMMSG = *blanks
     C                   EVAL      wk_ShowPrice = '1'

      *
      * Load any user-defined text to be included at the top of the email body.
      *
     C                   EVAL      kf_CODE = wk_EmailType2
     C                   EVAL      kf_CTYP = wk_CustType
     C                   EVAL      kf_POS = 'T'                                 Top of email body
     C     key_ET        CHAIN     HYPOETX
     C                   IF        not %found(HYPOETX)  and
     C                               wk_CustType <> *Blanks
     C                   EVAL      kf_CTYP = *Blanks
     C     key_ET        CHAIN     HYPOETX
     C                   ENDIF                                                  not %found(HYPOETX)
     C                   IF        %found(HYPOETX)
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               %trimr(ETTEXT) + NewParagraph
     C                   ENDIF                                                  %found(HYPOETX)

      *
      * Load standard text to be included at the top of the email body.
      *
     C                   IF        %subst(OEORRF:1:4) = 'GL8 ' or               Get Loaded Promo
     C                               %subst(OEORRF:1:4) = 'GL9 ' or
     C                               %subst(OEORRF:1:5) = 'GL10 '  or
     C                               %subst(OEORRF:1:5) = 'GL11 ' or
     C                               %subst(OEORRF:1:5) = 'GL12 ' or
     C                               %subst(OEORRF:1:5) = 'GL13 ' or
     C                               %subst(OEORRF:1:5) = 'GL14 ' or
     C                               %subst(OEORRF:1:5) = 'GL15 ' or
     C                               %subst(OEORRF:1:5) = 'GL16 ' or
     C                               %subst(OEORRF:1:5) = 'GL17 ' or
     C                               %subst(OEORRF:1:5) = 'GL18 ' or
     C                               %subst(OEORRF:1:5) = 'GL19 '  or
     C                               %subst(OEORRF:1:5) = 'GL20 '  or
     C                               %subst(OEORRF:1:5) = 'GL21 '  or
     C                               %subst(OEORRF:1:5) = 'GL22 '  or
     C                               %subst(OEORRF:1:5) = 'GL23 '  or
     C                               %subst(OEORRF:1:5) = 'GL24 '  or
     C                               %subst(OEORRF:1:5) = 'GL25 '  or
     C                               %subst(OEORRF:1:5) = 'GL26 '  or
     C                               %subst(OEORRF:1:5) = 'GL27 '  or
     C                               %subst(OEORRF:1:5) = 'GL28 '  or
     C                               %subst(OEORRF:1:5) = 'GL29 '  or
     C                               (%subst(OEORRF:1:1) = 'I'  and             Web
     C                                %subst(OEORRF:2:1) <> *Blank  and
     C                                %check(digits:%trimr(%subst(OEORRF:2))) =
     C                                  *Zero)
     C                   EVAL      wk_PO# = po_Text + %triml(OEORRF)
     C                   ELSE
     C                   EVAL      wk_PO# = *Blanks
     C                   ENDIF

     C                   SELECT
     C                   WHEN      wk_EmailType = 'RECEIVED'
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) + rec_Text1 +
     C                               %triml(%editc(wk_Order#:'Z')) +
     C                               %trimr(wk_PO#) + '.' +
     C                               NewParagraph + rec_Text3 + NewLine
     C                   WHEN      wk_EmailType = 'REVIEW'
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) + rec_Text2 +
     C                               %triml(%editc(wk_Order#:'Z')) +
     C                               %trimr(wk_PO#) + '.' +
     C                               NewParagraph + rec_Text3 + NewLine
     C                   WHEN      wk_EmailType = 'RELEASED'
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) + rls_Text1 +
     C                               %triml(%editc(wk_Order#:'Z')) +
     C                               %trimr(wk_PO#) + '.' +
     C                               NewParagraph + rls_Text2 + NewLine
     C                   WHEN      wk_EmailType = 'PHONE'
     C                   IF        OEORTY = 'W'
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               phn_Text1b +
     C                               %triml(%editc(wk_Order#:'Z')) + '.' +
     C                               NewParagraph
     C                   EVAL      kf_CODE = wk_EmailType2
     C                   EVAL      kf_CTYP = wk_CustType
     C                   EVAL      kf_POS = 'W'                                 Warranty message
     C     key_ET        CHAIN     HYPOETX
     C                   IF        not %found(HYPOETX)  and
     C                               wk_CustType <> *Blanks
     C                   EVAL      kf_CTYP = *Blanks
     C     key_ET        CHAIN     HYPOETX
     C                   ENDIF                                                  not %found(HYPOETX)
     C                   IF        %found(HYPOETX)
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               %trimr(ETTEXT) + NewParagraph
     C                   ENDIF                                                  %found(HYPOETX)
     C     wk_Order#     SETLL     OEORDT
     C                   DOU       %eof(OEORDT)  or  (ODORST <> 'C'  and
     C                               %subst(ODITEM:2:6) <> 'WRNTY ')
     C     wk_Order#     READE     OEORDT
     C                   ENDDO                                                  %eof(OEORDT) or
     C                   IF        not %eof(OEORDT)
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) + phn_Text2 +
     C                               NewLine
     C                   ENDIF                                                  not %eof(OEORDT)
     C                   ELSE
     C                   IF        wk_Donation
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               phn_Text1c +
     C                               %triml(%editc(wk_Order#:'Z')) +
     C                               '.' +
     C                               NewParagraph
     C                   ELSE
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               phn_Text1a +
     C                               %triml(%editc(wk_Order#:'Z')) +
     C                               %trimr(wk_PO#) + '.' +
     C                               NewParagraph + phn_Text2 + NewLine
     C                   ENDIF                                                  wk_Donation
     C                   ENDIF                                                  OEORTY='W'
     C                   WHEN      wk_EmailType = 'SHIPPED'
     C                   IF        OEORTY = 'W'
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               shp_Text1b +
     C                               %triml(%editc(wk_Order#:'Z')) + '.' +
     C                               NewParagraph + shp_Text2 + NewLine
     C                   ELSE
     C                   IF        wk_Donation
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               shp_Text1c +
     C                               %triml(%editc(wk_Order#:'Z')) + '.' +
     C                               NewParagraph + shp_Text2 + NewLine
     C                   ELSE
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               shp_Text1a +
     C                               %triml(%editc(wk_Order#:'Z')) +
     C                               %trimr(wk_PO#) + '.' +
     C                               NewParagraph + shp_Text2 + NewLine
     C                   ENDIF                                                  wk_Donation
     C                   ENDIF                                                  OEORTY='W'
     C                   WHEN      wk_EmailType = 'CHANGED'
     C                   IF        OEORTY = 'W'
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               chg_Text1b +
     C                               %triml(%editc(wk_Order#:'Z')) + '.' +
     C                               NewParagraph + chg_Text2 + NewLine
     C                   ELSE
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               chg_Text1a +
     C                               %triml(%editc(wk_Order#:'Z')) +
     C                               %trimr(wk_PO#) + '.' +
     C                               NewParagraph + chg_Text2 + NewLine
     C                   ENDIF                                                  OEORTY='W'
     C                   WHEN      wk_EmailType = 'CANCELLED'
     C                   IF        OEORTY = 'W'
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               can_Text1b +
     C                               %triml(%editc(wk_Order#:'Z')) + '.' +
     C                               NewParagraph + can_Text2 + NewLine
     C                   ELSE
     C                   IF        wk_Donation
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               can_Text1c +
     C                               %triml(%editc(wk_Order#:'Z')) + '.' +
     C                               NewParagraph
     C                   ELSE
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               can_Text1a +
     C                               %triml(%editc(wk_Order#:'Z')) +
     C                               %trimr(wk_PO#) + '.' +
     C                               NewParagraph + can_Text2 + NewLine
     C                   ENDIF                                                  wk_Donation
     C                   ENDIF                                                  OEORTY='W'
     C                   ENDSL
      ** Create the 'top' portion of the message
     C                   eval        wk_TOPMSG = %subst(wk_LONGMSG:10)
      *
      * Load order items to the email body.
      *
     C                   IF        not wk_Donation  or
     C                             (wk_Donation  and  wk_EmailType = 'SHIPPED')

     C     wk_Order#     SETLL     OEORDT
     C                   DOU       %eof(OEORDT)
     C     wk_Order#     READE     OEORDT

     C                   IF        %eof(OEORDT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDT)

     C                   IF        ODORST = 'C'  and
     C                               wk_EmailType <> 'CANCELLED'
     C                   ITER
     C                   ENDIF                                                  ODORST='C'

     C                   IF        wk_EmailType = 'PHONE'  and
     C                               %subst(ODITEM:2:6) = 'WRNTY '
     C                   ITER
     C                   ENDIF                                                  wk_EmailType='PHONE'

     C                   SELECT
     C                   WHEN      wk_EmailType = 'SHIPPED'
     C                   IF        ODQSTC = *Zero
     C                   ITER
     C                   ENDIF                                                  ODQSTC=*Zero
     C                   IF        wk_Turn# = *Zero
     C                   EVAL      wk_Qty = ODQSTC
     C                   ELSE
     C                   EVAL      wk_Qty = *Zero
     C     key_ID        SETLL     OEORDP01
     C                   DOU       %eof(OEORDP01)
     C     key_ID        READE     OEORDP01
     C                   IF        %eof(OEORDP01)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDP01)
     C                   EVAL      wk_Qty = wk_Qty + IDQSTC
     C                   ENDDO                                                  %eof(OEORDP01)
     C                   IF        wk_Qty = *Zero
     C                   ITER
     C                   ENDIF                                                  wk_Qty=*Zero
     C                   ENDIF                                                  wk_Turn#=*Zero
     C                   WHEN      wk_EmailType = 'CHANGED'
     C                   EVAL      wk_Qty = ODQORD - ODQSTD
     C                   WHEN      wk_EmailType = 'CANCELLED'
     C                   EVAL      wk_Qty = ODQORD - ODQSTD
     C                   IF        wk_Qty <= *Zero
     C                   ITER
     C                   ENDIF                                                  wk_Qty<=*Zero
     C                   OTHER
     C                   EVAL      wk_Qty = ODQORD
     C                   ENDSL

     C                   IF        OELOC# = 95  or  OEDSHP = *Zero
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               NewLine + ' ' + ODITEM + ' ' +
     C                               %subst(RmvIPSym('T':ODIMDS):1:30) +
     C                               '   QTY ' + %trimr(EditQty()) +
     C                               '   $' + %trimr(EditPrice()) + ' ' + ODPRUM
     C                   EVAL      wk_SHPITEMS = %trimr(wk_SHPITEMS) +
     C                               '|' + ODITEM + '^' +
     C                               %subst(RmvIPSym('T':ODIMDS):1:30) +
     C                               '^QTY ' + %trimr(EditQty()) +
     C                               '^$' + %trimr(EditPrice()) + ' ' +
     C                               ODPRUM + '^' + CalcExt()
     C                   ELSE
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               NewLine + ' ' + ODITEM + ' ' +
     C                               %subst(RmvIPSym('T':ODIMDS):1:30) +
     C                               '   QTY ' + %trimr(EditQty())
     C                   EVAL      wk_SHPITEMS = %trimr(wk_SHPITEMS) +
     C                               '|' + ODITEM + '^' +
     C                               %subst(RmvIPSym('T':ODIMDS):1:30) +
     C                               '^QTY ' + %trimr(EditQty()) + '^  ^ '
     C                   EVAL      wk_ShowPrice = '0'
     C                   ENDIF                                                  OEDSHP=*Zero

     C                   ENDDO                                                  %eof(OEORDT)

     C                   ENDIF                                                  not wk_Donation or

      *
      * For "SHIPPED" or "CANCELLED" email, load totals to the email body.
      *
     C                   IF        OELOC# = 95  or  OEDSHP = *Zero

     C                   IF        wk_EmailType = 'SHIPPED'
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               NewParagraph + shp_Text6 +
     C                               %trim(%editc(wk_TotalAmt:'J')) + '.'
     C                   ENDIF                                                  wk_EmailType='SHIPPE

     C                   IF        wk_EmailType = 'CANCELLED'
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               NewParagraph + can_Text6 +
     C                               %trim(%editc(wk_TotalAmt:'J')) + '.'
     C                   ENDIF                                                  wk_EmailType='CANCEL

     C                   ELSE                                                   OELOC#=95 or

      * this is a drop ship order, clear the total and turn off the show price boolean
     C                   EVAL      wk_TotalAmt = 0
     C                   EVAL      wk_ShowPrice = '0'

     C                   ENDIF                                                  OELOC#=95 or

      *
      * For "SHIPPED" email, load backorder items to the email body.
      *
     C                   IF        wk_EmailType = 'SHIPPED'
     C                               and  not wk_Donation
     C                   EVAL      wk_BOFnd = *OFF
     C     wk_Order#     SETLL     OEORDT
     C                   DOU       %eof(OEORDT)
     C     wk_Order#     READE     OEORDT

     C                   IF        %eof(OEORDT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDT)

     C                   IF        ODORST = 'C'  or  ODQSTD >= ODQORD  or
     C                               ODQSTC <> *Zero  or  ODBOAL <> 'Y'
     C                   ITER
     C                   ENDIF                                                  ODORST='C' or

     C                   IF        not wk_BOFnd
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               NewParagraph + shp_Text3 + NewLine
     C                   EVAL      wk_BOFnd = *ON
     C                   ENDIF                                                  not wk_BOFnd

     C                   EVAL      wk_Qty = ODQORD - ODQSTD

     C                   IF        OELOC# = 95  or  OEDSHP = *Zero
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               NewLine + ' ' + ODITEM + ' ' +
     C                               %subst(RmvIPSym('T':ODIMDS):1:30) +
     C                               '   QTY ' + %trimr(EditQty()) +
     C                               '   $' + %trimr(EditPrice()) + ' ' + ODPRUM
     C                   EVAL      wk_BCKITEMS = %trimr(wk_BCKITEMS) +
     C                               '|' + ODITEM + '^' +
     C                               %subst(RmvIPSym('T':ODIMDS):1:30) +
     C                               '^QTY ' + %trimr(EditQty()) +
     C                               '^$' + %trimr(EditPrice()) + ' ' +
     C                               ODPRUM + '^' + CalcExt()
     C                   EVAL      wk_BCKAmt = wk_BCKAmt + (wk_Qty * ODSLPR)
     C                   ELSE
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               NewLine + ' ' + ODITEM + ' ' +
     C                               %subst(RmvIPSym('T':ODIMDS):1:30) +
     C                               '   QTY ' + %trimr(EditQty())
     C                   EVAL      wk_BCKITEMS = %trimr(wk_BCKITEMS) +
     C                               '|' + ODITEM + '^' +
     C                               %subst(RmvIPSym('T':ODIMDS):1:30) +
     C                               '^QTY ' + %trimr(EditQty()) + '^ ' +
     C                               '^ '
     C                   EVAL      wk_ShowPrice = '0'
     C                   ENDIF                                                  OELOC#=95 or

     C                   ENDDO                                                  %eof(OEORDT)
     C                   ENDIF                                                  wk_EmailType='SHIPPE

      *
      * For "SHIPPED" email, load do-not-backorder items to the email body.
      *
     C                   IF        wk_EmailType = 'SHIPPED'
     C                               and  not wk_Donation
     C                   EVAL      wk_BOFnd = *OFF
     C     wk_Order#     SETLL     OEORDT
     C                   DOU       %eof(OEORDT)
     C     wk_Order#     READE     OEORDT

     C                   IF        %eof(OEORDT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDT)

     C                   IF        ODORST = 'C'  or  ODQSTD >= ODQORD  or
     C                               ODQSTC <> *Zero  or  ODBOAL = 'Y'
     C                   ITER
     C                   ENDIF                                                  ODORST='C' or

     C                   IF        not wk_BOFnd
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               NewParagraph + shp_Text4 + NewLine
     C                   EVAL      wk_BOFnd = *ON
     C                   ENDIF                                                  not wk_BOFnd

     C                   EVAL      wk_Qty = ODQORD - ODQSTD

     C                   IF        OELOC# = 95  or  OEDSHP = *Zero
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               NewLine + ' ' + ODITEM + ' ' +
     C                               %subst(RmvIPSym('T':ODIMDS):1:30) +
     C                               '   QTY ' + %trimr(EditQty()) +
     C                               '   $' + %trimr(EditPrice()) + ' ' + ODPRUM
     C                   EVAL      wk_NOBCKITEMS = %trimr(wk_NOBCKITEMS) +
     C                               '|' + ODITEM + '^' +
     C                               %subst(RmvIPSym('T':ODIMDS):1:30) +
     C                               '^QTY ' + %trimr(EditQty()) +
     C                               '^$' + %trimr(EditPrice()) + ' ' +
     C                               ODPRUM + '^' + CalcExt()
     C                   EVAL      wk_NOBCKAmt=wk_NOBCKAmt + (wk_Qty * ODSLPR)
     C                   ELSE
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               NewLine + ' ' + ODITEM + ' ' +
     C                               %subst(RmvIPSym('T':ODIMDS):1:30) +
     C                               '   QTY ' + %trimr(EditQty())
     C                   EVAL      wk_NOBCKITEMS = %trimr(wk_NOBCKITEMS) +
     C                               '|' + ODITEM + '^' +
     C                               %subst(RmvIPSym('T':ODIMDS):1:30) +
     C                               '^QTY ' + %trimr(EditQty()) + '^ ' +
     C                               '^ '
     C                   EVAL      wk_ShowPrice = '0'
     C                   ENDIF                                                  OELOC#=95 or

     C                   ENDDO                                                  %eof(OEORDT)
     C                   ENDIF                                                  wk_EmailType='SHIPPE

      *
      * Load standard text to be included at the bottom of the email body.
      *
     C                   SELECT
     C                   WHEN      wk_EmailType = 'RECEIVED'
     C                   WHEN      wk_EmailType = 'REVIEW'
     C                   WHEN      wk_EmailType = 'RELEASED'
     C                   WHEN      wk_EmailType = 'PHONE'
     C                   WHEN      wk_EmailType = 'SHIPPED'
     C                   IF        IHSVSV <> 'BW'
     C****               IF        Trackable()  and  OEUDF3 <> *Blanks
     C                   IF        Trackable()  and
     C                               (OEUDF3 <> *Blanks  or  TDVORD <> *Blanks)
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               NewParagraph + shp_Text5a +
     C                               %trimr(ShipVia()) + shp_Text5b +
     C                               %trimr(TrackingNbr()) + shp_Text5c +
     C                               %trimr(TrackWeb()) + '.'
     C                   EVAL      wk_BTMMSG = NewParagraph + shp_Text5a +
     C                               %trimr(ShipVia()) + shp_Text5b +
     C                               %trimr(TrackingNbr()) + shp_Text5c +
     C                               %trimr(TrackWeb()) + '.'
     C                   EVAL      kf_CODE = 'TRACKMSG'
     C                   EVAL      kf_CTYP = wk_CustType
     C                   EVAL      kf_POS = 'B'                                 Bottom of email body
     C     key_ET        CHAIN     HYPOETX
     C                   IF        not %found(HYPOETX)  and
     C                               wk_CustType <> *Blanks
     C                   EVAL      kf_CTYP = *Blanks
     C     key_ET        CHAIN     HYPOETX
     C                   ENDIF                                                  not %found(HYPOETX)
     C                   IF        %found(HYPOETX)
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) + '  ' +
     C                               ETTEXT
     C                   EVAL      wk_BTMMSG = %trimr(wk_BTMMSG) + '  ' +
     C                               ETTEXT
     C                   ENDIF                                                  %found(HYPOETX)

     C                   ELSE
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               NewParagraph + shp_Text5d +
     C                               %trimr(ShipVia()) + shp_Text5e
     C                   EVAL      wk_BTMMSG = %trimr(wk_BTMMSG) +
     C                               NewParagraph + shp_Text5d +
     C                               %trimr(ShipVia()) + shp_Text5e
     C                   ENDIF                                                  Trackable() and
     C                   ENDIF                                                  IHSVSV<>'BW'
     C                   WHEN      wk_EmailType = 'CHANGED'
     C                   WHEN      wk_EmailType = 'CANCELLED'
     C                   ENDSL

      *
      * Load any user-defined text to be included at the bottom of the email body.
      *
     C                   EVAL      kf_CODE = wk_EmailType2
     C                   EVAL      kf_CTYP = wk_CustType
     C                   EVAL      kf_POS = 'B'                                 Bottom of email body
     C     key_ET        CHAIN     HYPOETX
     C                   IF        not %found(HYPOETX)  and
     C                               wk_CustType <> *Blanks
     C                   EVAL      kf_CTYP = *Blanks
     C     key_ET        CHAIN     HYPOETX
     C                   ENDIF                                                  not %found(HYPOETX)
     C                   IF        %found(HYPOETX)
     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) +
     C                               NewParagraph + ETTEXT
     C                   EVAL      wk_BTMMSG = %trimr(wk_BTMMSG) +
     C                               NewParagraph + ETTEXT
     C                   ENDIF                                                  %found(HYPOETX)

     C                   IF        DatabaseID = 'SS'
     C                   EVAL      wk_LONGMSG =
     C                               %scanrpl('Hornady':'SnapSafe':wk_LONGMSG)
     C                   EVAL      wk_BTMMSG =
     C                               %scanrpl('Hornady':'SnapSafe':wk_BTMMSG)
     C                   ENDIF                                                  DatabaseID='SS'

     C                   EVAL      wk_LONGMSG = %trimr(wk_LONGMSG) + ''')'

      *
      * Create the email command (using SNDDST).
      *
      ****   SNDDST     TYPE(*LMSG)
      ****                TOINTNET((joecustomer@isp.net *PRI)
      ****                  (employee1@hornady.com *BCC)
      ****                  (employee2@hornady.com *BCC)
      ****                  (employee3@hornady.com *BCC))
      ****                DSTD('Email subject - up to 44 bytes')
      ****                LONGMSG('Long message - up to 5000 bytes')
      ****                USRID(SALES S10B604B)

     C                   EVAL      wk_EmailCmd = 'SNDDST TYPE(*LMSG) ' +
     C                               %trimr(wk_TOINTNET) + ' ' +
     C                               %trimr(wk_DSTD) + ' ' +
     C                               %trimr(wk_LONGMSG) + ' USRID(' +
     C                               %trimr(ECDUSR) + ' ' +
     C                               %trimr(ECDADR) + ')'
     C                   EVAL      wk_CmdLen = %len(%trimr(wk_EmailCmd))

      *
      * Send the email.
      *
     C*                   CALLP(E)  QCMDEXC (wk_EmailCmd:wk_CmdLen)
     C*                   IF        %error
     C*                   EVAL      wk_RtnCode = '04'
     C*                   EXSR      $WrtLog
     C*                   LEAVE
     C*                   ENDIF                                                  %error
      /free

                         callp(e) HYN6080 (wk_TOADDR    :
                                           wk_CCADDR    :
                                           wk_BCADDR    :
                                           wk_DSTD      :
                                           wk_TOPMSG    :
                                           wk_SHPITEMS  :
                                           %trim(%editc(wk_TotalAmt:'J')) :
                                           wk_BCKITEMS  :
                                           %trim(%editc(wk_BCKAmt:'J')) :
                                           wk_NOBCKITEMS:
                                           %trim(%editc(wk_NOBCKAmt:'J')) :
                                           wk_BTMMSG    :
                                           wk_EmailCmd  :
                                           wk_ShowPrice );
                         if %error();
                           wk_RtnCode = '04';
                           exsr $WrtLog;
                           leave;
                         endif;

      /end-free
      *
      * Write a log record.
      *
     C                   EXSR      $WrtLog

     C                   ENDDO

     C                   ENDSL

     C                   EVAL      out_RtnCode = wk_RtnCode

     C                   RETURN

     C/EJECT
      **************************************************************************
      *****               S  U  B  R  O  U  T  I  N  E  S                  *****
      **************************************************************************
      **************************************************************************
      * Retrieve order summary data.                                           *
      **************************************************************************
     CSR   $RtvOrdSumm   BEGSR

     C                   EVAL      wk_TotalAmt = *Zero
     C                   EVAL      wk_MaxQtyLen = *Zero

     C                   IF        wk_EmailType = 'SHIPPED'  and
     C                               IHAIV# <> *Zero
     C     IHAIV#        CHAIN     OEIVHH01
     C                   IF        %found(OEIVHH01)
     C****               EVAL      wk_TotalAmt = HIINVA
     C                   ENDIF                                                  %found(OEIVHH01)
     C                   ENDIF                                                  wk_EmailType='SHIPPE

     C     wk_Order#     SETLL     OEORDT
     C                   DOU       %eof(OEORDT)
     C     wk_Order#     READE     OEORDT

     C                   IF        %eof(OEORDT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDT)

     C                   IF        ODORST = 'C'  and
     C                               wk_EmailType <> 'CANCELLED'
     C                   ITER
     C                   ENDIF                                                  ODORST='C'

     C                   SELECT
     C                   WHEN      wk_EmailType = 'SHIPPED'
     C                   IF        ODQSTC = *Zero
     C                   ITER
     C                   ENDIF                                                  ODQSTC=*Zero
     C                   IF        wk_Turn# = *Zero
     C                   EVAL      wk_Qty = ODQSTC
     C                   ELSE
     C                   EVAL      wk_Qty = *Zero
     C     key_ID        SETLL     OEORDP01
     C                   DOU       %eof(OEORDP01)
     C     key_ID        READE     OEORDP01
     C                   IF        %eof(OEORDP01)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDP01)
     C                   EVAL      wk_Qty = wk_Qty + IDQSTC
     C                   ENDDO                                                  %eof(OEORDP01)
     C                   ENDIF                                                  wk_Turn#=*Zero
     C                   WHEN      wk_EmailType = 'CHANGED'
     C                   EVAL      wk_Qty = ODQORD - ODQSTD
     C                   WHEN      wk_EmailType = 'CANCELLED'
     C                   EVAL      wk_Qty = ODQORD - ODQSTD
     C                   IF        wk_Qty <= *Zero
     C                   ITER
     C                   ENDIF                                                  wk_Qty<=*Zero
     C                   OTHER
     C                   EVAL      wk_Qty = ODQORD
     C                   ENDSL

     C                   SELECT
     C                   WHEN      (wk_Qty > 9999  or  wk_Qty < -9999)  and
     C                               wk_MaxQtyLen < 5
     C                   EVAL      wk_MaxQtyLen = 5
     C                   WHEN      (wk_Qty > 999  or  wk_Qty < -999)  and
     C                               wk_MaxQtyLen < 4
     C                   EVAL      wk_MaxQtyLen = 4
     C                   WHEN      (wk_Qty > 99  or  wk_Qty < -99)  and
     C                               wk_MaxQtyLen < 3
     C                   EVAL      wk_MaxQtyLen = 3
     C                   WHEN      (wk_Qty > 9  or  wk_Qty < -9)  and
     C                               wk_MaxQtyLen < 2
     C                   EVAL      wk_MaxQtyLen = 2
     C                   WHEN      wk_Qty <> *Zero  and  wk_MaxQtyLen < 1
     C                   EVAL      wk_MaxQtyLen = 1
     C                   ENDSL

     C****               IF        wk_EmailType <> 'SHIPPED'  or
     C****                         (wk_EmailType = 'SHIPPED'  and
     C****                           IHAIV# = *Zero)  or
     C****                         (wk_EmailType = 'SHIPPED'  and
     C****                           IHAIV# <> *Zero  and  not %found(OEIVHH01))
     C                   EVAL(H)   wk_ExtPrc = ODSLPR * wk_Qty
     C                   EVAL      wk_TotalAmt = wk_TotalAmt + wk_ExtPrc
     C****               ENDIF                                                  wk_EmailType<>'SHIPP

     C                   ENDDO                                                  %eof(OEORDT)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Write a log record.                                                    *
      **************************************************************************
     CSR   $WrtLog       BEGSR

     C                   EVAL      ELORD# = wk_Order#
     C                   EVAL      ELTYPE = wk_EmailType
     C                   SELECT
     C                   WHEN      wk_EmailType = 'RECEIVED'
     C                   EVAL      ELSEND = ECSND1
     C                   WHEN      wk_EmailType = 'REVIEW'
     C                   EVAL      ELSEND = ECSND2
     C                   WHEN      wk_EmailType = 'RELEASED'
     C                   EVAL      ELSEND = ECSND3
     C                   WHEN      wk_EmailType = 'PHONE'
     C                   EVAL      ELSEND = ECSND4
     C                   WHEN      wk_EmailType = 'SHIPPED'
     C                   EVAL      ELSEND = ECSND5
     C                   WHEN      wk_EmailType = 'CHANGED'
     C                   EVAL      ELSEND = ECSND6
     C                   WHEN      wk_EmailType = 'CANCELLED'
     C                   EVAL      ELSEND = ECSND7
     C                   OTHER
     C                   EVAL      ELSEND = '?'
     C                   ENDSL
     C                   EVAL      ELERCD = wk_RtnCode
     C                   EVAL      ELTSTP = %timestamp
     C                   IF        %subst(wk_EmailAddr:1:1) = ''''  and
     C                               %subst(wk_EmailAddr:
     C                                 %len(%trimr(wk_EmailAddr)):1) = ''''
     C                   EVAL      ELEMAL = %subst(wk_EmailAddr:2:
     C                               %len(%trimr(wk_EmailAddr))-2)
     C                   ELSE
     C                   EVAL      ELEMAL = wk_EmailAddr
     C                   ENDIF                                                  %subst(wk_EmailAddr:

     C                   WRITE     HYROELG

     C                   IF        wk_RtnCode = '04'
     C                   EVAL      ELCMD = wk_EmailCmd
     C                   WRITE     HYROELD
     C                   ENDIF                                                  wk_RtnCode='04'

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Remove messages from program message queue.                            *
      **************************************************************************
     CSR   $RmvPM        BEGSR

     C                   CALLP(E)  QMHRMVPM ('*' : *Zero : *Blanks : '*ALL' :
     C                               io_MsgErrCode)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization.                                                *
      **************************************************************************
     CSR   *InzSR        BEGSR

     C     key_ET        KLIST                                                  HYPOETX
     C                   KFLD                    kf_CODE
     C                   KFLD                    kf_CTYP
     C                   KFLD                    kf_POS

     C     key_ID        KLIST                                                  OEORDP01
     C                   KFLD                    wk_Turn#
     C                   KFLD                    wk_Order#
     C                   KFLD                    ODORL#
     C                   KFLD                    ODBLN#

     CSR                 ENDSR

     P/EJECT
      **************************************************************************
      * Retrieve the customer's email address.                                 *
      *   Return values: Email address.                                        *
      **************************************************************************
     P RtvEmailAddr    B
     D                 PI            64A

     D wk_EmailAddr    S             64A

     C                   DO

     C                   EVAL      wk_EmailAddr = *Blanks

      *
      * If order is being drop-shipped, retrieve drop ship email address.
      *
     C                   IF        OEDSHP <> *Zero
     C     OEDSHP        CHAIN     HDDSHP2
     C                   IF        %found(HDDSHP2)  and  DSEMAL <> *Blanks
     C                   EVAL      wk_EmailAddr = DSEMAL
     C                   LEAVE
     C                   ENDIF                                                  %found(HDDSHP2) and
     C                   ENDIF                                                  OEDSHP<>*Zero

     C                   IF        OELOC# = 91
     C                   LEAVE
     C                   ENDIF                                                  OELOC#=91

     C     wk_Order#     SETLL     OEOCMT
     C                   DOU       %eof(OEOCMT)
     C     wk_Order#     READE     OEOCMT
     C                   IF        %eof(OEOCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEOCMT)
     C                   IF        %subst(OCCMNT:1:7) = 'EMAIL: '
     C                   EVAL      wk_EmailAddr = %subst(OCCMNT:8)
     C                   IF        %subst(OCCMNT:58:3) = '...'
     C     wk_Order#     READE     OEOCMT
     C                   IF        %eof(OEOCMT)
     C                   EVAL      %subst(wk_EmailAddr:58) = *Blanks
     C                   ELSE
     C                   EVAL      %subst(wk_EmailAddr:58) = %subst(OCCMNT:8)
     C                   ENDIF                                                  %eof(OEOCMT)
     C                   ENDIF                                                  %subst(OCCMNT:58:3)
     C                   LEAVE
     C                   ENDIF                                                  %subst(OCCMNT:1:7)=
     C                   ENDDO                                                  %eof(OEOCMT)

     C                   IF        %eof(OEOCMT)
     C     OESHTO        CHAIN     HYPCLMS
     C                   IF        %found(HYPCLMS)  and  CLEMAL <> *Blanks
     C                   EVAL      wk_EmailAddr = CLEMAL
     C                   LEAVE
     C                   ENDIF                                                  %found(HYPCLMS)
     C                   IF        OEBLTO <> OESHTO
     C     OEBLTO        CHAIN     HYPCLMS
     C                   IF        %found(HYPCLMS)  and  CLEMAL <> *Blanks
     C                   EVAL      wk_EmailAddr = CLEMAL
     C                   LEAVE
     C                   ENDIF                                                  %found(HYPCLMS)
     C                   ENDIF                                                  OEBLTO<>OESHTO
     C                   ENDIF                                                  %eof(OEOCMT)

     C                   ENDDO

     C                   IF        wk_EmailAddr <> *Blanks  and
     C                              (%scan('-':wk_EmailAddr) > *Zero  or        Contains hyphen
     C                               %scan(',':wk_EmailAddr) > *Zero  or        Contains comma
     C                               %subst(wk_EmailAddr:1:1) > 'Z')            Starts with number
     C                   EVAL      wk_EmailAddr = '''' + %trimr(wk_EmailAddr) +
     C                               ''''
     C                   ENDIF                                                  wk_EmailAddr<>*Blank

     C                   RETURN    wk_EmailAddr

     P RtvEmailAddr    E

     P/EJECT
      **************************************************************************
      * Determine whether a shipping method is trackable.                      *
      *   Return values: *ON-trackable, *OFF-not trackable.                    *
      **************************************************************************
     P Trackable       B
     D                 PI             1N

     D wk_Trackable    S              1N

     C                   EVAL      wk_Trackable = *OFF

     C                   IF        IHSVSV = 'UP'  or                            UPS ground comm
     C                               IHSVSV = 'UN'  or                          UPS next day comm
     C                               IHSVSV = 'U2'  or                          UPS 2nd day comm
     C                               IHSVSV = 'U3'  or                          UPS 3rd day comm
     C                               IHSVSV = 'RG'  or                          UPS ground res
     C                               IHSVSV = 'RN'  or                          UPS next day res
     C                               IHSVSV = 'R2'  or                          UPS 2nd day res
     C                               IHSVSV = 'R3'  or                          UPS 3rd day res
     C                               IHSVSV = 'UC'  or                          UPS consignee
     C                               IHSVSV = 'CU'  or                          UPS C.O.D.
     C                               IHSVSV = 'FE'  or                          FedEx
     C                               IHSVSV = 'FN'  or                          FedEx Next Day
     C                               IHSVSV = 'F2'  or                          FedEx 2-Day
     C                               IHSVSV = 'F3'  or                          FedEx Express Saver
     C                               IHSVSV = '1R'  or                          FedEx 1Rate Pty Ovnt
     C                               IHSVSV = '2R'  or                          FedEx 1Rate Epres Sv
     C                               IHSVSV = '3R'  or                          FedEx 1Rate 2 Day
     C                               IHSVSV = '4R'  or                          FedEx 1Rate 2 Day AM
     C                               IHSVSV = '5R'  or                          FedEx 1Rate Std Ovnt
     C                               IHSVSV = '6R'  or                          FedEx 1Rate 1st Ovnt
     C                               IHSVSV = 'FC'  or                          USPS 1st class
     C                               IHSVSV = 'FI'  or                          USPS 1st class ins
     C                               IHSVSV = 'AR'  or                          USPS airmail
     C                               IHSVSV = 'AI'  or                          USPS airmail ins
     C                               IHSVSV = 'PM'  or                          Priority Mail
     C                               IHSVSV = 'PI'  or                          USPS priority ins
     C                               IHSVSV = 'AB'  or                          ABF
     C                               IHSVSV = 'FT'  or                          FedEx Freight Pty
     C                               IHSVSV = 'FW'  or                          FedEx White Glove
     C                               IHSVSV = 'FF'  or                          FedEx Freight Econo
     C                               IHSVSV = 'OD'                              Old Dominion
     C                   EVAL      wk_Trackable = *ON
     C                   ENDIF                                                  IHSVSV='UP or

     C                   RETURN    wk_Trackable

     P Trackable       E

     P/EJECT
      **************************************************************************
      * Retrieve the ship-via carrier description.                             *
      *   Return values: Carrier description.                                  *
      **************************************************************************
     P ShipVia         B
     D                 PI            30A

     D wk_ShipVia      S             30A

     C                   EVAL      wk_ShipVia = *Blanks

     C                   SELECT
     C                   WHEN      IHSVSV = 'UP'  or                            UPS ground comm
     C                               IHSVSV = 'UN'  or                          UPS next day comm
     C                               IHSVSV = 'U2'  or                          UPS 2nd day comm
     C                               IHSVSV = 'U3'  or                          UPS 3rd day comm
     C                               IHSVSV = 'RG'  or                          UPS ground res
     C                               IHSVSV = 'RN'  or                          UPS next day res
     C                               IHSVSV = 'R2'  or                          UPS 2nd day res
     C                               IHSVSV = 'R3'  or                          UPS 3rd day res
     C                               IHSVSV = 'UC'  or                          UPS consignee
     C                               IHSVSV = 'CU'                              UPS C.O.D.
     C                   EVAL      wk_ShipVia = 'UPS'
     C                   WHEN      IHSVSV = 'FE'  or                            FedEx
     C                               IHSVSV = 'FN'  or                          FedEx Next Day
     C                               IHSVSV = 'F2'  or                          FedEx 2-Day
     C                               IHSVSV = 'F3'                              FedEx Express Saver
     C                   EVAL      wk_ShipVia = 'Federal Express'
     C                   WHEN      IHSVSV = 'FT'  or                            FedEx Freight Pty
     C                               IHSVSV = 'FW'  or                          FedEx White Glove
     C                               IHSVSV = 'FF'                              FedEx Freight Econo
     C                   EVAL      wk_ShipVia = 'Fedex Freight'
     C                   WHEN      IHSVSV = 'FC'  or                            USPS 1st class
     C                               IHSVSV = 'FI'  or                          USPS 1st class ins
     C                               IHSVSV = 'AR'  or                          USPS airmail
     C                               IHSVSV = 'AI'  or                          USPS airmail ins
     C                               IHSVSV = 'PM'  or                          USPS priority
     C                               IHSVSV = 'PI'                              USPS priority ins
     C                   EVAL      wk_ShipVia = 'United States Postal Service'
     C                   OTHER
     C                   IF        not %open(HDSHPV)
     C                   OPEN      HDSHPV
     C                   ENDIF                                                  not %open(HDSHPV)
     C     IHSVSV        CHAIN     HDSHPV
     C                   IF        %found(HDSHPV)
     C                   EVAL      wk_ShipVia = SVSVDS
     C                   ELSE
     C                   EVAL      wk_ShipVia = IHSVSV
     C                   ENDIF                                                  %found(HDSHPV)
     C                   ENDSL

     C                   RETURN    wk_ShipVia

     P ShipVia         E

     P/EJECT
      **************************************************************************
      * Format the shipment tracking number.                                   *
      *   Return values: Tracking number.                                      *
      **************************************************************************
     P TrackingNbr     B
     D                 PI            30A

     D wk_TrackNbr     S             30A
     D wk_MDFTRK       S             25A
     D wk_MDFBRC       S             50A
     D wk_VarsityTrk#  S             50A

     C                   EVAL      wk_TrackNbr = *Blanks

     C                   IF        TDVORD <> *Blanks
      /Free
       wk_MDFTRK = *Blanks;
       wk_MDFBRC = *Blanks;
       EXEC SQL select MDFTRK, MDFBRC
                 into :wk_MDFTRK, :wk_MDFBRC
                from MFD1MD
                where MDCMNO='01' and MDOTYP=' ' and
                 MDORDR=:TDVORD
                order by MDBXNO
                fetch first row only;
       IF wk_MDFTRK <> *Blanks;
         wk_VarsityTrk# = wk_MDFTRK;
       ELSE;
         wk_VarsityTrk# = wk_MDFBRC;
       ENDIF;                                                                // wk_MDFTRK<>*Blanks
      /End-Free
     C                   ELSE
     C                   EVAL      wk_VarsityTrk# = *Blanks
     C                   ENDIF                                                  TDVORD<>*Blanks

     C                   SELECT
     C                   WHEN      wk_VarsityTrk# <> *Blanks
     C                   EVAL      wk_TrackNbr = wk_VarsityTrk#
     C                   WHEN      DatabaseID = 'SS'
     C                   EVAL      wk_TrackNbr = TDTRK#
     C                   WHEN      IHSVSV = 'UP'  or                            UPS ground comm
     C                               IHSVSV = 'UN'  or                          UPS next day comm
     C                               IHSVSV = 'U2'  or                          UPS 2nd day comm
     C                               IHSVSV = 'U3'  or                          UPS 3rd day comm
     C                               IHSVSV = 'RG'  or                          UPS ground res
     C                               IHSVSV = 'RN'  or                          UPS next day res
     C                               IHSVSV = 'R2'  or                          UPS 2nd day res
     C                               IHSVSV = 'R3'  or                          UPS 3rd day res
     C                               IHSVSV = 'UC'  or                          UPS consignee
     C                               IHSVSV = 'CU'                              UPS C.O.D.
     C                   IF        TDALOC = 'WEST'
     C                   EVAL      wk_TrackNbr = '1ZW' + OEUDF3
     C                   ELSE
     C                   EVAL      wk_TrackNbr = '1Z6' + OEUDF3
     C                   ENDIF                                                  TDALOC='WEST'
     C                   OTHER
     C                   EVAL      wk_TrackNbr = OEUDF3
     C                   ENDSL

     C                   RETURN    wk_TrackNbr

     P TrackingNbr     E

     P/EJECT
      **************************************************************************
      * Retrieve the shipment carrier tracking web site.                       *
      *   Return values: Tracking web site.                                    *
      **************************************************************************
     P TrackWeb        B
     D                 PI           100A

     D wk_WebSite      S            100A

     C                   EVAL      wk_WebSite = *Blanks

     C                   SELECT
     C                   WHEN      IHSVSV = 'UP'  or                            UPS ground comm
     C                               IHSVSV = 'UN'  or                          UPS next day comm
     C                               IHSVSV = 'U2'  or                          UPS 2nd day comm
     C                               IHSVSV = 'U3'  or                          UPS 3rd day comm
     C                               IHSVSV = 'RG'  or                          UPS ground res
     C                               IHSVSV = 'RN'  or                          UPS next day res
     C                               IHSVSV = 'R2'  or                          UPS 2nd day res
     C                               IHSVSV = 'R3'  or                          UPS 3rd day res
     C                               IHSVSV = 'UC'  or                          UPS consignee
     C                               IHSVSV = 'CU'                              UPS C.O.D.
     C                   EVAL      wk_WebSite = 'www.ups.com/WebTracking/+
     C                               processInputRequest?tracknum=' +
     C                               TrackingNbr()
     C****               WHEN      IHSVSV = 'FE'  or                            FedEx
     C****                           IHSVSV = 'FA'                              FedEx air
     C****               EVAL      wk_WebSite = 'www.fedex.com'
     C                   WHEN      IHSVSV = 'FE'  or                            FedEx
     C                               IHSVSV = 'FN'  or                          FedEx overnight
     C                               IHSVSV = 'F2'  or                          FedEx 2 day
     C                               IHSVSV = 'F3'                              FedEx express saver
     C                                              or
     C                               IHSVSV = '1R'  or                          FedEx 1Rate Pty Ovnt
     C                               IHSVSV = '2R'  or                          FedEx 1Rate Epres Sv
     C                               IHSVSV = '3R'  or                          FedEx 1Rate 2 Day
     C                               IHSVSV = '4R'  or                          FedEx 1Rate 2 Day AM
     C                               IHSVSV = '5R'  or                          FedEx 1Rate Std Ovnt
     C                               IHSVSV = '6R'                              FedEx 1Rate 1st Ovnt
     C                   EVAL      wk_WebSite = 'www.fedex.com/apps/fedextrack/+
     C                               ?tracknumbers=' +
     C                               TrackingNbr()
     C                   WHEN      IHSVSV = 'FT'  or                            FedEx Freight Pty
     C                               IHSVSV = 'FW' or                           FedEx White Glove
     C                               IHSVSV = 'FF'                              FedEx Freight Econo
     C                   EVAL      wk_WebSite = 'www.fedex.com/apps/fedextrack/+
     C                               ?tracknumbers=' +
     C                               TrackingNbr()
     C                   WHEN      IHSVSV = 'PM'                                Priority Mail
     C                               or  IHSVSV = 'FC'                          USPS 1st class
     C                               or  IHSVSV = 'FI'                          USPS 1st class ins
     C                               or  IHSVSV = 'AR'                          USPS airmail
     C                               or  IHSVSV = 'AI'                          USPS airmail ins
     C                               or  IHSVSV = 'PI'                          USPS priority ins
     C                   EVAL      wk_WebSite = 'https://tools.usps.com/go/+
     C                               TrackConfirmAction?tLabels=' +
     C                               %scanrpl(' ':'':%trimr(TrackingNbr()))
     C                   WHEN      IHSVSV = 'AB'                                ABF
     C                   EVAL      wk_WebSite = 'https://www.abfs.com/tools/+
     C                               trace/default.asp?hidSubmitted=Y&refno0=' +
     C                               %trimr(TrackingNbr()) + '&reftype0=A'
     C                   WHEN      IHSVSV = 'OD'                                Old Dominion
     C                   EVAL      wk_WebSite = 'http://www.odfl.com/trace/+
     C                               Trace.jsp?pronum=' +
     C                               TrackingNbr()
     C                   ENDSL

      **** NOTE: If we ever want to switch USPS tracking to Endicia's tracking web page,
      ****         https://www.endicia.com/Tracking/?PIC=9405511206209464345857

     C                   RETURN    wk_WebSite

     P TrackWeb        E

     P/EJECT
      **************************************************************************
      * Edit the unit price.                                                   *
      *   Return values: Edited unit price.                                    *
      **************************************************************************
     P EditPrice       B
     D                 PI            17A

     D wk_PriceX       S             17A

     C                   EVAL      wk_PriceX = %editc(ODSLPR:'J')
     C                   DOW       %subst(wk_PriceX:16:1) = *Zero
     C                               and  %subst(wk_PriceX:14:1) <> '.'
     C                   EVAL      wk_PriceX = ' ' + %subst(wk_PriceX:1:15) +
     C                               %subst(wk_PriceX:17:1)
     C                   ENDDO
     C****               IF        %subst(wk_PriceX:16:1) = '.'
     C****               EVAL      wk_PriceX = ' ' + %subst(wk_PriceX:1:15) +
     C****                           %subst(wk_PriceX:17:1)
     C****               ENDIF                                                  %subst(wk_PriceX:16:
     C                   EVAL      wk_PriceX = %triml(wk_PriceX)

     C                   RETURN    wk_PriceX

     P EditPrice       E

     P/EJECT
      **************************************************************************
      * Edit the quantity.                                                     *
      *   Return values: Edited quantity.                                      *
      **************************************************************************
     P EditQty         B
     D                 PI            17A

     D wk_QtyX         S             17A

     C                   MONITOR
     C                   EVAL      wk_QtyX = %char(%int(wk_Qty))
     C                   ON-ERROR
     C                   EVAL      wk_QtyX = %editc(wk_Qty:'J')
     C                   DOW       %subst(wk_QtyX:16:1) = *Zero
     C                   EVAL      wk_QtyX = ' ' + %subst(wk_QtyX:1:15) +
     C                               %subst(wk_QtyX:17:1)
     C                   ENDDO                                                   %subst(wk_QtyX:16:1
     C                   IF        %subst(wk_QtyX:16:1) = '.'
     C                   EVAL      wk_QtyX = ' ' + %subst(wk_QtyX:1:15) +
     C                               %subst(wk_QtyX:17:1)
     C                   ENDIF                                                   %subst(wk_QtyX:16:1
     C                   EVAL      wk_QtyX = %triml(wk_QtyX)
     C                   IF        wk_Qty > *Zero
     C                   DOW       %len(%trimr(wk_QtyX)) < wk_MaxQtyLen
     C                   EVAL      wk_QtyX = ' ' + wk_QtyX
     C                   ENDDO                                                   %len(%trimr(wk_QtyX
     C                   ELSE
     C                   DOW       %len(%trimr(wk_QtyX)) < wk_MaxQtyLen + 1
     C                   EVAL      wk_QtyX = ' ' + wk_QtyX
     C                   ENDDO                                                   %len(%trimr(wk_QtyX
     C                   ENDIF                                                   wk_Qty>*Zero
     C                   ENDMON                                                  wk_Qty>*Zero

     C                   RETURN    wk_QtyX

     P EditQty         E
      **************************************************************************
      * Calculate extended price
      *   Return values: Edited extended price
      **************************************************************************
     P CalcExt         B
     D                 PI            15a

     D wk_Ext          S             11p 2
     D wk_ExtC         S             15a

     C                   EVAL      wk_Ext = ODSLPR * wk_Qty
     C                   EVAL      wk_ExtC = '$' + %trim(%editc(wk_Ext:'J'))

     C                   RETURN    wk_ExtC

     P CalcExt         E

     P/EJECT
