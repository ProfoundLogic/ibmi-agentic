     H COPYRIGHT('(C) Copyright Hornady Manufacturing Company 2002')
      **************************************************************************
      * TITLE: Order Ship (Scanner) Build Work Records
      *    BY: William Earl (MCA)
      *  DATE: October 2002
      *
      *  REV DATE  BY   CHANGE
      *  --------  ---  -----------------------------------------------------
      *  02/13/2003  MCA  Fix bug - when determining whether order has been
      *                   previously shipped in the current invoice cycle,
      *                   only use OEORHP records in "S" status.
      *  12/03/2007  MCA  If order has been previously shipped in the current invoice
      *                   cycle, prevent invoice process from selecting the order while
      *                   shipping again.  If the order is currently selected in an
      *                   invoice process, prevent shipping until invoice process is
      *                   complete. ("X" status)
      *  07/18/2008  MCA  Exclude closed detail records.
      *  07/20/2009  MCA  Add ODUDA4-ODUDA0 and ODUDN4-ODUDN5 fields.
      *                   (ODUDN5 is shipping priority, and it was being
      *                    zeroed through the shipping process.)
      *  03/29/2010  MCA  Handle mulitple open in-process records.  Change was
      *                   made for Series IV dies.
      *  10/23/2012  MCA  HD5.0.
      *  03/04/2013  MCA  Add H1UCA1-H1UCA5, H1UDF6-H1UDF0, and H1UDN1=H1UDN5
      *                   fields. (OEUDF7 and OEUDF8 are show name and promotion
      *                   name respectively, and were being blanked through the
      *                   shipping process.)
      *  05/07/2013  MCA  Maintain turnaround number separation.
      *  06/26/2023  MCA  Delete any orphaned work records for order/turn prior
      *                   to creating new work records.
      *  08/15/2023  MCA  Add BLOCK(*NO) to OEDTWK, OESLWK, OECMWK, and OETXWK
      *                   file definitions.  This was happening anyway, and these
      *                   changes were done to not have messsage CPF4028 "Open
      *                   of member OEDTWK was changed to SEQONLY(*NO)." shown
      *                   in the job log.
      **************************************************************************
      *
     FOEORHD    IF   E           K DISK
     FOEORDT    IF   E           K DISK
     F**OEORHP01  IF   E           K DISK
     FOEORHP    IF   E           K DISK
     FOEORDP01  IF   E           K DISK
     FHDWHSM    IF   E           K DISK
     FHDIMST    IF   E           K DISK
     FHDIWHS    IF   E           K DISK
     FHDKIT     IF   E           K DISK
     FHDKTIT    IF   E           K DISK
     FHDSHQT    IF   E           K DISK
     FOETAXA    IF   E           K DISK
     FOEOCMT    IF   E           K DISK
     FOEHDWK    UF A E           K DISK
     FOEDTWK    O    E             DISK
     F                                     BLOCK(*NO)
     FOESLWK    O    E             DISK
     F                                     BLOCK(*NO)
     FOECMWK    O    E             DISK
     F                                     BLOCK(*NO)
     FOETXWK    O    E             DISK
     F                                     BLOCK(*NO)
     FIVCTRL    IF   E             DISK

     D* Prototype Copy Section *******************************************
      /COPY *LIBL/QPRPSRC,POEROC

     D QMHSNDPM        PR                  EXTPGM('QMHSNDPM')                   Send program message
     D  out_MsgID                     7A   CONST
     D  out_MsgFile                  20A   CONST
     D  out_MsgData                 512A   CONST
     D  out_MsgDtaLen                10I 0 CONST
     D  out_MsgType                  10A   CONST
     D  out_MsgStack                 10A   CONST
     D  out_MsgStkCtr                10I 0 CONST
     D  in_MsgKey                     4A
     D  io_MsgErrCode                10I 0

     D                 DS
     D OEOEF                   1     20
     D OEFL01                  1      1
     D OEFL02                  2      2
     D OEFL03                  3      3
     D OEFL04                  4      4
     D OEFL05                  5      5
     D OEFL06                  6      6
     D OEFL07                  7      7
     D OEFL08                  8      8
     D OEFL09                  9      9
     D OEFL10                 10     10
     D OEFL11                 11     11
     D OEFL12                 12     12
     D OEFL13                 13     13
     D OEFL14                 14     14
     D OEFL15                 15     15
     D OEFL16                 16     16
     D OEFL17                 17     17
     D OEFL18                 18     18
     D OEFL19                 19     19
     D OEFL20                 20     20

     D ent_Order#      S                   LIKE(OEORD#)
     D ent_Turn#       S                   LIKE(H1TURN)
     D ent_Control#    S                   LIKE(H1OCTL)
     D wk_Order#       S                   LIKE(OEORD#)
     D wk_Turn#        S                   LIKE(H1TURN)
     D wk_Control#     S                   LIKE(H1OCTL)
     D wk_Reship       S              1A   INZ(No)
     D wk_Item         S                   LIKE(ODITEM)
     D wk_Lot#         S             15A
     D wk_KitCost      S                   LIKE(ODCOST)
     D wk_ItemCost     S                   LIKE(ODCOST)
     D wk_TUCC         S                   LIKE(ODCOST)
     D wk_UCC1         S                   LIKE(ODCOST)
     D wk_UCC2         S                   LIKE(ODCOST)
     D wk_UCC3         S                   LIKE(ODCOST)
     D wk_UCC4         S                   LIKE(ODCOST)
     D wk_UCC5         S                   LIKE(ODCOST)
     D wk_H1OCTL       S              8S 0
     D wk_RcdCnt       S              7P 0
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0

     D Yes             C                   'Y'
     D No              C                   'N'

     C                   SELECT

     C                   WHEN      wk_Order# = *Hival
     C                   EVAL      *INLR = *ON

     C                   OTHER

     C                   DO

      *
      * Delete any orphaned work records for order/turn.
      *
     C                   EXSR      $DltOrphans

      *
      * Retrieve order control number.
      *
     C                   EVAL      wk_Control#=RetOrdCtl

      *
      * Build order header work record.
      *
     C                   EXSR      $BldH1
     C                   IF        not %found(OEORHD)
     C                   LEAVE
     C                   ENDIF                                                  not %found(OEORHD)

      *
      * Build order detail work records.
      *
     C                   EXSR      $BldO1

      *
      * Build order shipment work records.
      *
     C                   IF        wk_Reship = Yes
     C                   EXSR      $BldO3
     C                   ENDIF                                                  wk_Reship=Yes

      *
      * Build order comment work records.
      *
     C                   EXSR      $BldO4

      *
      * Build order tax work records.
      *
     C                   EXSR      $BldTW

      *
      * Build order payment work records.
      *
     C                   EXSR      $BldWP

      *
      * Build order user-defined data work records.
      *
     C                   EXSR      $BldUDD

     C                   ENDDO

     C                   ENDSL
      *
      * Return to caller.
      *
     C                   RETURN

     C/EJECT
     C********************************************************************
     C* Build order header work record.                                  *
     C********************************************************************
     CSR   $BldH1        BEGSR

     C                   DO

     C     wk_Order#     CHAIN     OEORHD
     C                   IF        not %found(OEORHD)
     C                   LEAVE
     C                   ENDIF                                                  not %found(OEORHD)

     C**** wk_Order#     CHAIN     OEORHP01
     C****               IF        %found(OEORHP01)
     C****               EVAL      wk_Reship = Yes
     C****               ELSE
     C****               EVAL      wk_Reship = No
     C****               ENDIF                                                  %found(OEORHP01)

     C****               IF        wk_Turn# = *Zero
     C**** wk_Order#     SETLL     OEORHP01
     C****               DOU       %eof(OEORHP01)  or  IHSTAT = 'S'
     C****                           or  IHSTAT = 'X'
     C**** wk_Order#     READE     OEORHP01
     C****               ENDDO                                                  %eof(OEORHP01) or
     C****               IF        %eof(OEORHP01)
     C****               EVAL      wk_Reship = No
     C**** wk_Order#     SETGT     OEORHP01
     C**** wk_Order#     READPE    OEORHP01
     C****               IF        %eof(OEORHP01)
     C****               EVAL      IHTURN = *Zero
     C****               ENDIF                                                  %eof(OEORHP01)
     C****               ELSE
     C****               EVAL      wk_Reship = Yes
     C****               ENDIF                                                  %eof(OEORHP01)
     C****               ELSE
     C**** wk_Order#     SETLL     OEORHP01
     C****               DOU       %eof(OEORHP01)  or  IHTURN = wk_Turn#
     C**** wk_Order#     READE     OEORHP01
     C****               ENDDO                                                  %eof(OEORHP01) or
     C****               IF        %eof(OEORHP01)
     C****               EVAL      wk_Reship = No
     C****               CLEAR     *ALL          OEORHPR
     C****               EVAL      IHTURN = wk_Turn#
     C****               ELSE
     C****               IF        IHSTAT = 'S'  or  IHSTAT = 'X'
     C****               EVAL      wk_Reship = Yes
     C****               ELSE
     C****               EVAL      wk_Reship = No
     C****               ENDIF                                                  IHSTAT='S' or
     C****               ENDIF                                                  %eof(OEORHP01)
     C****               ENDIF                                                  wk_Turn#=*Zero

     C                   IF        wk_Turn# <> *Zero
     C     wk_Turn#      CHAIN     OEORHP
     C                   IF        %found(OEORHP)  and
     C                               (IHSTAT = 'S'  or  IHSTAT = 'X')
     C                   EVAL      wk_Reship = Yes
     C                   ELSE
     C                   EVAL      wk_Reship = No
     C                   ENDIF                                                  %found(OEORHP) and
     C                   ELSE
     C                   EVAL      wk_Reship = No
     C                   ENDIF                                                  wk_Turn#<>*Zero

     C     wk_Control#   CHAIN     OEHDWK
     C****               EVAL      H1TURN = IHTURN
     C                   EVAL      H1TURN = wk_Turn#
     C                   EVAL      H1ORD# = OEORD#
     C                   EVAL      H1SHTO = OESHTO
     C                   EVAL      H1BLTO = OEBLTO
     C                   EVAL      H1DSHP = OEDSHP
     C                   EVAL      H1LOC# = OELOC#
     C                   EVAL      H1ORTY = OEORTY
     C                   EVAL      H1BDTE = OEBDTE
     C                   EVAL      H1RQDT = OERQDT
     C                   EVAL      H1DTE2 = OEDTE2
     C                   EVAL      H1DTE3 = OEDTE3
     C                   EVAL      H1DTE4 = OEDTE4
     C                   EVAL      H1SLSM = OESLSM
     C                   EVAL      H1SLS1 = OESLS1
     C                   EVAL      H1CTXC = OECTXC
     C                   EVAL      H1OEF = OEOEF
     C                   EVAL      H1HOLD = OEHOLD
     C                   EVAL      H1ORRF = OEORRF
     C                   EVAL      H1CTRM = OECTRM
     C                   EVAL      H1DSPC = OEDSPC
     C                   EVAL      H1SMPC = OESMPC
     C                   EVAL      H1SSPC = OESSPC
     C                   IF        wk_Turn# <> *Zero  and  %found(OEORHP)
     C                   EVAL      H1SVSV = IHSVSV
     C                   EVAL      H1SVDS = *Blanks
      /Free
        EXEC SQL Select SVSVDS
                   into :H1SVDS
                 From HDSHPV Where SVSVSV=:IHSVSV;
      /End-Free
     C                   ELSE
     C                   EVAL      H1SVSV = OESVSV
     C                   EVAL      H1SVDS = OESVDS
     C                   ENDIF                                                  wk_Turn#<>*Zero and
     C                   EVAL      H1BOAL = OEBOAL
     C                   EVAL      H1BANK = OEBANK
     C                   EVAL      H1STTX = OESTTX
     C                   EVAL      H1CNTX = OECNTX
     C                   EVAL      H1CTTX = OECTTX
     C                   EVAL      H1LOC1 = OELOC1
     C                   EVAL      H1LOC2 = OELOC2
     C                   EVAL      H1LOC3 = OELOC3
     C                   IF        wk_Reship = Yes
     C                   EVAL      H1FRT = IHOFRT
     C                   EVAL      H1CHRG = IHOMSG
     C                   EVAL      H1PKL# = IHPKL#
     C                   ELSE
     C                   EVAL      H1FRT = OETFRT
     C                   EVAL      H1CHRG = OETMSG
     C                   EVAL      H1PKL# = *Zero
     C                   ENDIF                                                   wk_Reship=Yes
     C                   EVAL      H1STTA = OETSTX
     C                   EVAL      H1CNTA = OETCTX
     C                   EVAL      H1CTTA = OETYTX
     C                   EVAL      H1LC1A = OETLC1
     C                   EVAL      H1LC2A = OETLC2
     C                   EVAL      H1LC3A = OETLC3
     C                   EVAL      H1OTOT = OETIVA
     C                   EVAL      H1ORRY = OEORRY
     C                   EVAL      H1RSDT = OERSDT
     C                   EVAL      H1REDT = OEREDT
     C                   EVAL      H1ACIV = OEACIV
     C                   EVAL      H1ACDT = OEACDT
     C                   EVAL      H1ACLC = OEACLC
     C                   EVAL      H1MAIL = OEMAIL
     C****               EVAL      H1CCAM = OECCAM
     C****               EVAL      H1CHGA = OECHGA
     C****               EVAL      H1CCNO = OECCNO
     C****               EVAL      H1CCTP = OECCTP
     C****               EVAL      H1CCAC = OECCAC
     C****               EVAL      H1CCED = OECCED
     C****               EVAL      H1JRID = OEJRID
     C                   EVAL      H1UCA1 = OEUCA1
     C                   EVAL      H1UCA2 = OEUCA2
     C                   EVAL      H1UCA3 = OEUCA3
     C                   EVAL      H1UCA4 = OEUCA4
     C                   EVAL      H1UCA5 = OEUCA5
     C                   EVAL      H1UDF1 = OEUDF1
     C                   EVAL      H1UDF2 = OEUDF2
     C                   EVAL      H1UDF3 = OEUDF3
     C                   EVAL      H1UDF4 = OEUDF4
     C                   EVAL      H1UDF5 = OEUDF5
     C                   EVAL      H1UDF6 = OEUDF6
     C                   EVAL      H1UDF7 = OEUDF7
     C                   EVAL      H1UDF8 = OEUDF8
     C                   EVAL      H1UDF9 = OEUDF9
     C                   EVAL      H1UDF0 = OEUDF0
     C                   EVAL      H1UDN1 = H1UDN1
     C                   EVAL      H1UDN2 = H1UDN2
     C                   EVAL      H1UDN3 = H1UDN3
     C                   EVAL      H1UDN4 = H1UDN4
     C                   EVAL      H1UDN5 = H1UDN5
     C                   EVAL      H1FBCD = OEFOBC
     C                   EVAL      H1MNCD = 'S'
     C                   EVAL      H1QTOO = No
     C                   EVAL      H1QTE# = *Zero
     C                   EVAL      H1CLCN = *Blank
     C                   EVAL      H1LLTC = OELTCD
     C                   EVAL      H1GNPO = *Blank
     C                   EVAL      H1LSTK = Yes
     C                   EVAL      H1TAXO = OETAXO
     C****               EVAL      H1PYC1 = OEPYC1
     C****               EVAL      H1PYA1 = OEPYA1
     C****               EVAL      H1PYC2 = OEPYC2
     C****               EVAL      H1PYA2 = OEPYA2

     C                   IF        %found(OEHDWK)
     C                   UPDATE    OEHDWKR
     C                   ELSE
     C                   EVAL      H1OCTL = wk_Control#
     C                   WRITE     OEHDWKR
     C                   ENDIF                                                  %found(OEHDWK)

     C                   ENDDO

     CSR                 ENDSR

     C/EJECT
     C********************************************************************
     C* Build order detail work records.                                 *
     C********************************************************************
     CSR   $BldO1        BEGSR

     C     wk_Order#     SETLL     OEORDT
     C                   DOU       %eof(OEORDT)
     C     wk_Order#     READE     OEORDT
     C                   IF        %eof(OEORDT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDT)

     C                   IF        ODORST = 'C'
     C                   ITER
     C                   ENDIF                                                  ODORST='C'

      *
      * Retrieve in-process detail record.
      *
     C                   IF        wk_Turn# = *Zero
     C                   CLEAR     *ALL          OEORDPR
     C                   ELSE
     C     IDKey         CHAIN     OEORDP01
     C                   IF        not %found(OEORDP01)
     C                   CLEAR     *ALL          OEORDPR
     C                   ENDIF                                                  not %found(OEORDP01)
     C                   ENDIF                                                  wk_Turn#=*Zero

      *
      * Retrieve master file records.
      *
     C     ODITEM        CHAIN     HDIMST
     C     IWKey         CHAIN     HDIWHS

      *
      * Populate work record fields.
      *
     C                   CLEAR                   OEDTWKR
     C                   EVAL      O1OCTL = wk_Control#
     C                   EVAL      O1ORL# = ODORL#
     C                   EVAL      O1BLKT = ODBLKT
     C                   EVAL      O1BLN# = ODBLN#
     C                   EVAL      O1LOC# = ODLOC#
     C                   EVAL      O1ORST = ODORST
     C                   EVAL      O1RQDT = ODRQDT
     C                   EVAL      O1CMPC = ODCMPC
     C                   EVAL      O1LNDS = ODLNDS
     C                   EVAL      O1OREC = ODOREC
     C                   EVAL      O1SHPF = ODSHPF
     C                   EVAL      O1ITEM = ODITEM
     C                   EVAL      O1IMDS = ODIMDS
     C                   EVAL      O1SHTO = ODSHTO
     C                   EVAL      O1PKL# = ODPKL#
     C                   EVAL      O1BOL# = ODBOL#
     C                   EVAL      O1SVSV = ODSVSV
     C                   EVAL      O1SVDS = ODSVDS
     C                   EVAL      O1PCLS = ODPCLS
     C                   EVAL      O1PGRP = ODPGRP
     C                   EVAL      O1QORD = ODQORD
     C                   EVAL      O1QSTD = ODQSTD
     C****               EVAL      O1QSTC = ODQSTC
     C                   EVAL      O1QSTC = IDQSTC
     C****               EVAL      O1QOPK = ODQOPK
     C                   EVAL      O1QOPK = IDQOPK
     C                   EVAL      O1QOBO = ODQOBO
     C                   EVAL      O1QRBO = ODQRBO
     C                   IF        ODQORD < *Zero
     C                   EVAL      O1BOAL = No
     C                   ELSE
     C                   EVAL      O1BOAL = ODBOAL
     C                   ENDIF                                                  ODQORD<*Zero
     C                   EVAL      O1DTLI = ODDTLI
     C                   EVAL      O1WH = ODWH
     C                   EVAL      O1PLT = ODPLT
     C                   EVAL      O1ORCS = ODORCS
     C                   EVAL      O1PRUM = ODPRUM
     C                   EVAL      O1TAXC = ODTAXC
     C                   EVAL      O1SLPR = ODSLPR
     C                   IF        ODCPOV = Yes                                 Cost Overridden
     C                   EVAL      O1COST = ODCOST
     C                   EVAL      O1UCC1 = ODUCC1
     C                   EVAL      O1UCC2 = ODUCC2
     C                   EVAL      O1UCC3 = ODUCC3
     C                   EVAL      O1UCC4 = ODUCC4
     C                   EVAL      O1UCC5 = ODUCC5
     C                   ELSE
     C                   EVAL      wk_Item = ODITEM
     C                   CALLB     'HHDRUC'      HHDRUC                         Get Current
     C                   EVAL      O1COST = wk_TUCC                             Unit Cost If
     C                   EVAL      O1UCC1 = wk_UCC1                             Not Overridden
     C                   EVAL      O1UCC2 = wk_UCC2
     C                   EVAL      O1UCC3 = wk_UCC3
     C                   EVAL      O1UCC4 = wk_UCC4
     C                   EVAL      O1UCC5 = wk_UCC5
     C                   ENDIF                                                  ODCPOV=Yes
     C                   EVAL      O1LIST = ODLIST
     C                   EVAL      O1STUM = ODSTUM
     C                   EVAL      O1ORUF = ODORUF
     C                   EVAL      O1LPOV = ODLPOV
     C                   EVAL      O1CPOV = ODCPOV
     C                   EVAL      O1LNUP = ODLNUP
     C                   EVAL      O1LSTP = ODLSTP
     C                   EVAL      O1LLTC = ODLLTC
     C                   EVAL      O1DSHP = ODDSHP
     C                   EVAL      O1DSQR = ODDSQR
     C                   EVAL      O1QAFG = ODQAFG
     C                   EVAL      O1VEN1 = IWVEND

     C                   DO
     C                   EVAL      O1STKL = No
     C                   IF        O1OREC <> 'S'  or  CISTKL <> Yes
     C                   LEAVE
     C                   ENDIF                                                  CISTKL<>Yes
     C     ODWH          CHAIN     HDWHSM
     C                   IF        not %found(HDWHSM)  or  WHSTKL <> Yes
     C                   LEAVE
     C                   ENDIF                                                  not %found(HDWHSM)
     C                   IF        not %found(HDIWHS)  or  IWSTKL <> Yes
     C                   LEAVE
     C                   ENDIF                                                  not %found(HDIWHS)
     C                   EVAL      O1STKL = Yes
     C                   ENDDO

     C                   IF        %found(HDIMST)
     C                   EVAL      O1IMWG = IMIMWG
     C                   EVAL      O1KIT = IMKIT
     C                   IF        IMKIT = 'K'
     C     ODITEM        CHAIN     HDKIT
     C                   IF        %found(HDKIT)
     C                   EVAL      O1KTRL = KTKTRL                              Kit Relief Code
     C                   EVAL      O1KTPR = KTKTPR                              Kit Pricing Code
     C                   IF        KTKTCS = 'C'  and                            Kit Cost By Cmp
     C                               ODCPOV = No                                No Cost Override
     C                   EXSR      $KitCost                                     Calc Kit Cost
     C                   ENDIF                                                  KTKTCS='C' and ODCPO
     C                   ENDIF                                                  %found(HDKIT)
     C                   ENDIF                                                  IMKIT='K'
     C                   ENDIF                                                  %found(HDIMST)

     C                   EVAL      O1PO# = ODPO#
     C                   EVAL      O1POL# = ODPOL#
     C                   EVAL      O1PORL = ODPORL
     C                   EVAL      O1GNPO = ODGNPO
     C                   EVAL      O1FCQL = ODFCQL
     C                   EVAL      O1FCTM = ODFCTM
     C                   EVAL      O1ACCQ = ODACCQ
     C                   EVAL      O1UDA1 = ODUDA1
     C                   EVAL      O1UDA2 = ODUDA2
     C                   EVAL      O1UDA3 = ODUDA3
     C                   EVAL      O1UDA4 = ODUDA4
     C                   EVAL      O1UDA5 = ODUDA5
     C                   EVAL      O1UDA6 = ODUDA6
     C                   EVAL      O1UDA7 = ODUDA7
     C                   EVAL      O1UDA8 = ODUDA8
     C                   EVAL      O1UDA9 = ODUDA9
     C                   EVAL      O1UDA0 = ODUDA0
     C                   EVAL      O1UDN1 = ODUDN1
     C                   EVAL      O1UDN2 = ODUDN2
     C                   EVAL      O1UDN3 = ODUDN3
     C                   EVAL      O1UDN4 = ODUDN4
     C                   EVAL      O1UDN5 = ODUDN5

      *
      * Write work record.
      *
     C                   WRITE     OEDTWKR

     C                   ENDDO                                                  %eof(OEORDT)

     CSR                 ENDSR

     C/EJECT
     C********************************************************************
     C* Build order shipment work records.                               *
     C********************************************************************
     CSR   $BldO3        BEGSR

     C     SQKey         SETLL     HDSHQT
     C                   DOU       %eof(HDSHQT)
     C     SQKey         READE     HDSHQT
     C                   IF        %eof(HDSHQT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDSHQT)

     C                   CLEAR                   OESLWKR
     C                   EVAL      O3OCTL = wk_Control#
     C                   EVAL      O3ORL# = SQORL#
     C                   EVAL      O3BLN# = SQBLN#
     C                   EVAL      O3ITEM = SQITEM
     C                   EVAL      O3CITM = SQCITM
     C                   EVAL      O3OITM = SQOITM
     C                   EVAL      O3WHS = SQWHS
     C                   EVAL      O3RATE = SQRATE
     C                   EVAL      O3STKR = SQSTKR
     C                   EVAL      O3AILE = SQAILE
     C                   EVAL      O3SLOC = SQSLOC
     C                   EVAL      O3LOTN = SQLOTN
     C                   EVAL      O3LQTY = SQLQTY
     C                   EVAL      O3WQTY = *Zero
     C                   EVAL      O3SID# = SQSID#
     C                   EVAL      O3QTYR = SQQTYR

     C                   WRITE     OESLWKR

     C                   ENDDO                                                  %eof(HDSHQT)

     CSR                 ENDSR

     C/EJECT
     C********************************************************************
     C* Build order comment work records.                                *
     C********************************************************************
     CSR   $BldO4        BEGSR

     C     wk_Order#     SETLL     OEOCMT
     C                   DOU       %eof(OEOCMT)
     C     wk_Order#     READE     OEOCMT
     C                   IF        %eof(OEOCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEOCMT)

     C                   CLEAR                   OECMWKR
     C                   EVAL      O4OCTL = wk_Control#
     C                   EVAL      O4ORL# = OCORL#
     C                   EVAL      O4BLN# = OCBLN#
     C                   EVAL      O4DOCT = OCDOCT
     C                   EVAL      O4CSEQ = OCCSEQ
     C                   EVAL      O4CMNT = OCCMNT

     C                   WRITE     OECMWKR

     C                   ENDDO                                                  %eof(OEOCMT)

     CSR                 ENDSR

     C/EJECT
     C********************************************************************
     C* Build order tax work records.                                    *
     C********************************************************************
     CSR   $BldTW        BEGSR

     C     TAKey         SETLL     OETAXA
     C                   DOU       %eof(OETAXA)
     C     TAKey         READE     OETAXA
     C                   IF        %EOF(OETAXA)
     C                   LEAVE
     C                   ENDIF

      *
      * Populate work record fields.
      *
     C                   CLEAR                   OETXWKR
     C                   EVAL      TWOCTL = wk_Control#
     C                   EVAL      TWTURN = TATURN
     C                   EVAL      TWORD# = TAORD#
     C                   EVAL      TWORL# = TAORL#
     C                   EVAL      TWSHTO = TASHTO
     C                   EVAL      TWDSHC = TADSHC
     C                   EVAL      TWTXBL = TATXBL
     C                   EVAL      TWCTXC = TACTXC
     C                   EVAL      TWTAXO = TATAXO
     C                   EVAL      TWSTTA = TASTTA
     C                   EVAL      TWCNTA = TACNTA
     C                   EVAL      TWCTTA = TACTTA
     C                   EVAL      TWL1TA = TAL1TA
     C                   EVAL      TWL2TA = TAL2TA
     C                   EVAL      TWL3TA = TAL3TA
     C                   EVAL      TWSTTX = TASTTX
     C                   EVAL      TWCNTX = TACNTX
     C                   EVAL      TWCTTX = TACTTX
     C                   EVAL      TWLOC1 = TALOC1
     C                   EVAL      TWLOC2 = TALOC2
     C                   EVAL      TWLOC3 = TALOC3

      *
      * Write work record.
      *
     C                   WRITE     OETXWKR

     C                   ENDDO                                                  %eof(OETAXA)

     CSR                 ENDSR

     C/EJECT
     C********************************************************************
     C* Build order payment work records.                                *
     C********************************************************************
     CSR   $BldWP        BEGSR

      /Free
       EXEC SQL Insert into OEPAYW
                (WPOCTL,WPPSEQ,WPPAYT,WPDESC,WPPAYA,
                 WPCHK,WPMEMO,WPCCTP,WPCCID,WPPCID)
        Select :wk_Control#,  OPPSEQ, OPPAYT,
               Case When OPPAYT<>' ' Then
                    (Select OMPD From OEPYMT a Where a.OMPC=c.OPPAYT)
                    When OPCCTP<>' ' Then
                    (Select OECCDS From OECCTM b Where b.OECCTP=c.OPCCTP)
               Else OPCHK End, OPPAYA, OPCHK,  OPMEMO, OPCCTP, OPCCID, OPPCID
        From OEPAYO c
        Where OPORD=:wk_Order#;
      /End-Free

     CSR                 ENDSR

     C/EJECT
     C********************************************************************
     C* Build order user-defined data work records.                      *
     C********************************************************************
     CSR   $BldUDD       BEGSR

      /Free
        EXEC SQL Insert into OEHDWKU
                 (WUOCTL,WUFLDN,WUFLDD,WUFLDR,WUFLDV)
          Select :wk_Control#,  OUFLDN,OUFLDD,OUFLDR,OUFLDV
          From OEOUHD
          Where OUORD=:OEORD#;

        EXEC SQL Insert into OEDTWKU
                 (WUOCTL,WULINE,WUFLDN,WUFLDD,WUFLDR,WUFLDV)
          Select :wk_Control#,  OULINE,OUFLDN,OUFLDD,OUFLDR,OUFLDV
          From OEOUDT
          Where OUORD=:OEORD#;
      /End-Free

     CSR                 ENDSR

     C/EJECT
     C********************************************************************
     C* Calculate Kit Cost By Component                                  *
     C********************************************************************
     CSR   $KitCost      BEGSR

     C                   EVAL      wk_KitCost = IWKITC
     C     ODITEM        SETLL     HDKTIT
     C                   DOU       %eof(HDKTIT)
     C     ODITEM        READE     HDKTIT
     C                   IF        %eof(HDKTIT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDKTIT)
     C                   EVAL      wk_Item = KIITEM
     C                   CALLB     'HHDRUC'      HHDRUC                         Get Unit Cost
     C                   EVAL(H)   wk_ItemCost = wk_TUCC * KIIMQT
     C                   EVAL      wk_KitCost = wk_KitCost + wk_ItemCost
     C                   ENDDO                                                  %eof(HDKTIT)
     C                   EVAL      O1COST = wk_KitCost
     C                   EVAL      O1UCC1 = wk_KitCost
     C                   EVAL      O1UCC2 = *Zero
     C                   EVAL      O1UCC3 = *Zero
     C                   EVAL      O1UCC4 = *Zero
     C                   EVAL      O1UCC5 = *Zero

     CSR                 ENDSR

     C/EJECT
     C********************************************************************
     C* Delete any orphaned work records for order/turn.                 *
     C********************************************************************
     CSR   $DltOrphans   BEGSR

      /Free
       DOU wk_RcdCnt = *Zero;
         wk_RcdCnt = *Zero;
         wk_H1OCTL = *Zero;
         EXEC SQL select count(*), coalesce(min(H1OCTL),0)
                   into :wk_RcdCnt, :wk_H1OCTL
                  from OEHDWK
                  where H1ORD#=:wk_Order# and H1TURN=:wk_Turn#;
         IF wk_RcdCnt > *Zero;
           EXEC SQL delete from OEDTWK
                  where O1OCTL=:wk_H1OCTL;
           EXEC SQL delete from OESLWK
                  where O3OCTL=:wk_H1OCTL;
           EXEC SQL delete from OECMWK
                  where O4OCTL=:wk_H1OCTL;
           EXEC SQL delete from OETXWK
                  where TWOCTL=:wk_H1OCTL;
           EXEC SQL delete from OEHDWK
                  where H1OCTL=:wk_H1OCTL;
           wk_MsgID = 'MSG0466';
           wk_MsgData = %triml(%editc(wk_Order#:'Z'));
           IF wk_Turn# <> *Zero;
             %subst(wk_MsgData:9:9) = %triml(%editc(wk_Turn#:'Z'));
           ELSE;
             %subst(wk_MsgData:9:1) = '0';
           ENDIF;                                                            // wk_Turn#<>*Zero
           %subst(wk_MsgData:18:8) = %triml(%editc(wk_H1OCTL:'Z'));
           EXSR $SndPM;
         ENDIF;                                                              // wk_RcdCnt>*Zero
       ENDDO;                                                                // wk_RcdCnt=*Zero
      /End-Free

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Send program message.                                                  *
      **************************************************************************
     CSR   $SndPM        BEGSR

     C                   EVAL      wk_MsgDataLen = %len(%trimr(wk_MsgData))

     C                   CALLP(E)  QMHSNDPM (wk_MsgID : 'GUPMSGF   *LIBL     ' :
     C                               wk_MsgData : wk_MsgDataLen : '*INFO' :
     C                               '*' : *Zero : in_MsgKey :
     C                               io_MsgErrCode)

     CSR                 ENDSR

     C/EJECT
     C********************************************************************
     C* Initialization.                                                  *
     C********************************************************************
     CSR   *INZSR        BEGSR

      *
      * Define entry parameter list.
      *
     C     *ENTRY        PLIST
     C     wk_Order#     PARM                    ent_Order#
     C     wk_Turn#      PARM                    ent_Turn#
     C                   PARM      wk_Control#   ent_Control#
     C

      *
      * Define called program parameter lists.
      *
     C     HHDRUC        PLIST                                                  Rtv Cost
     C                   PARM                    ODPLT
     C                   PARM                    wk_ITEM
     C                   PARM                    ODWH
     C                   PARM      *Blanks       wk_Lot#
     C                   PARM      *Zero         wk_TUCC
     C                   PARM      *Zero         wk_UCC1
     C                   PARM      *Zero         wk_UCC2
     C                   PARM      *Zero         wk_UCC3
     C                   PARM      *Zero         wk_UCC4
     C                   PARM      *Zero         wk_UCC5

      *
      * Define key lists.
      *
     C     IDKey         KLIST                                                  OEORDP01
     C                   KFLD                    wk_Turn#
     C                   KFLD                    ODORD#
     C                   KFLD                    ODORL#
     C                   KFLD                    ODBLN#

     C     IWKey         KLIST                                                  HDIWHS
     C                   KFLD                    ODITEM
     C                   KFLD                    ODWH

     C     SQKey         KLIST                                                  HDSHQT
     C                   KFLD                    wk_Turn#
     C                   KFLD                    wk_Order#

     C     TAKey         KLIST                                                  OETAXA
     C                   KFLD                    wk_Turn#
     C                   KFLD                    wk_Order#

      *
      * Retrieve the inventory control record.
      *
     C                   READ      IVCTRL

     CSR                 ENDSR
     C/EJECT
