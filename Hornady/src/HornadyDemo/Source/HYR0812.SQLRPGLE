     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2010')
     H BNDDIR('HYR0812')
      **************************************************************************
      * APPLICATION: Credit Card Processing                                    *
      *       TITLE: CurbstoneCard Complex Requests                            *
      *          BY: William Earl (MCA)                                        *
      *        DATE: January 2010                                              *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  06/07/2017  MCA  Curbstone C3 changes.                                *
      *  10/24/2019  MCA  For SettleFinal, use "RY" HYPCCRH records (if found) *
      *                   to create new REAUTHSALE or CREDIT transaction.      *
      *                   For MaxSettle, SettleAuths, and VoidAuths, skip "RY" *
      *                   HYPCCRH records when processing the order's MFUKEYs. *
      *                   Tkt #4714                                            *
      *  01/19/2021  MCA  Change call to HYR0802 for local void to instead     *
      *                   call HYR0802C3.  This should have happened back in   *
      *                   July 2017, when we converted from Curbstone C2 to C3.*
      *  06/17/2021  MCA  Change SettleFinal.  Added new "source" parameter.   *
      *                   If source is '-', and a new charge or a credit is    *
      *                   deemed required, run REAUTHSALE or CREDIT, as the    *
      *                   subprocedure has done since it was written.          *
      *                   Otherwise, instead of charging or crediting the      *
      *                   customer's card, write a record to HYPCCSF so that   *
      *                   a report can be generated for Accounting to follow   *
      *                   up with.                                             *
      *  05/27/2026  MCA  C3 changes.  Also changed *BNDDIR to call HYR0810C3. *
      *                   Should have been made in 2017.                       *
      *                                                                        *
      **************************************************************************

     H nomain

     FHYPCCRH   IF A E           K DISK    USROPN BLOCK(*NO)
     FHYPCCSF   O    E           K DISK    USROPN BLOCK(*NO)

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY QPRPSRC,HYR0812

      /COPY QPRPSRC,HYR0802C3

      /COPY QPRPSRC,HYR0804C3

      /COPY QPRPSRC,HYR0810C3

     D MaxSettle       PR
     D  Order#                        8P 0 CONST
     D  AmtReq                       11P 2 CONST
     D  TaxAmtReq                    11P 2 CONST
     D  FatalErr                      1A
     D  RtnCode                       2A
     D  MsgText                      35A

     D SettleAuths     PR
     D  Order#                        8P 0 CONST
     D  AmtReq                       11P 2 CONST
     D  TaxAmtReq                    11P 2 CONST
     D  FatalErr                      1A
     D  RtnCode                       2A
     D  MsgText                      35A

     D VoidAuths       PR
     D  Order#                        8P 0 CONST
     D  FatalErr                      1A
     D  RtnCode                       2A

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID

      **************************************************************************
      * Define program status data structure.                                  *
      **************************************************************************
     D                SDS
     D ps_UserID             254    263

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Pgm0802      S              1N   INZ(*OFF)
     D wk_Pgm0804      S              1N   INZ(*OFF)
     D wk_Pgm0810      S              1N   INZ(*OFF)

     P/EJECT
      **************************************************************************
      * Ensure shipment is settled in full.                                    *
      **************************************************************************
     P SettleFinal     B                   EXPORT

     D                 PI
     D Order#                         8P 0 CONST
     D AmtReq                        11P 2 CONST
     D TaxAmtReq                     11P 2 CONST
     D Source                         1A   CONST                                S=Ship, I=Invoice
     D NewCCAmt                      11P 2
     D FatalErr                       1A
     D RtnCode                        2A
     D MsgText                       35A

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Order#       S              8P 0
     D wk_AmtReq       S             11P 2
     D wk_TaxAmtReq    S             11P 2
     D wk_Source       S              1A
     D wk_NewCCAmt     S             11P 2
     D wk_FatalErr     S              1A
     D wk_RtnCode      S              2A
     D wk_MsgText      S             35A
     D wk_Settled$     S             11P 2
     D wk_StlRqs$      S             11P 2
     D wk_Auth$        S             11P 2
     D wk_Short$       S             11P 2
     D wk_Long$        S             11P 2
     D wk_TaxAmt       S             11P 2
     D wk_FatalErr2    S              1A
     D wk_RtnCode2     S              2A
     D wk_MsgText2     S             35A
     D wk_NewKey       S             15A
     D wk_OEBLTO       S              8S 0
     D wk_CMCNA1       S             26A

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_AmtReq = AmtReq
     C                   EVAL      wk_TaxAmtReq = TaxAmtReq
     C                   EVAL      wk_Source = Source
     C                   EVAL      wk_NewCCAmt = *Zero
     C                   EVAL      wk_FatalErr = *Blank
     C                   EVAL      wk_RtnCode = *Blanks
     C                   EVAL      wk_MsgText = *Blanks

     C                   EVAL      wk_Settled$ =
     C                               RtvOrdTranAmt(wk_Order#:'SETTLED')
     C                   EVAL      wk_StlRqs$ =
     C                               RtvOrdTranAmt(wk_Order#:'SETTLERQS')
     C                   EVAL      wk_Auth$ =
     C                               RtvOrdTranAmt(wk_Order#:'AUTHONLY')
     C                   EVAL      wk_Pgm0810 = *ON

     C                   SELECT

      *
      * Settle amount is correct without any action.
      *
     C                   WHEN      wk_Settled$ + wk_StlRqs$ = wk_AmtReq
     C                   IF        wk_Auth$ <> *Zero
     C                   CALLP(E)  VoidAuths (wk_Order#:wk_FatalErr2:
     C                               wk_RtnCode2)
     C                   EVAL      wk_RtnCode = 'UG'
     C                   IF        wk_StlRqs$ = *Zero
     C                   EVAL      wk_MsgText = 'No action required.  ' +
     C                               'Payment has been settled in full.'
     C                   ELSE
     C                   EVAL      wk_MsgText = 'No action required.  ' +
     C                               'Payment will be settled in full.'
     C                   ENDIF                                                  wk_StlRqs$=*Zero
     C                   ENDIF                                                  wk_Auth$<>*Zero
     C                   LEAVE

      *
      * Settle amount is deficient, and unsettled, but flagged to settle,
      * authorizations exist.
      *
     C                   WHEN      wk_Settled$ + wk_StlRqs$ < wk_AmtReq  and
     C                               wk_StlRqs$ > *Zero
     C                   CALLP(E)  MaxSettle (wk_Order#:wk_AmtReq:wk_TaxAmtReq:
     C                               wk_FatalErr2:wk_RtnCode2:wk_MsgText2)
     C                   CALLP(E)  ClrTranFlds ()
     C                   EVAL      wk_Settled$ =
     C                               RtvOrdTranAmt(wk_Order#:'SETTLED')
     C                   EVAL      wk_StlRqs$ =
     C                               RtvOrdTranAmt(wk_Order#:'SETTLERQS')
     C                   EVAL      wk_Auth$ =
     C                               RtvOrdTranAmt(wk_Order#:'AUTHONLY')
     C                   IF        wk_Auth$ <> *Zero
     C                   CALLP(E)  VoidAuths (wk_Order#:wk_FatalErr2:
     C                               wk_RtnCode2)
     C                   ENDIF                                                  wk_Auth$<>*Zero
     C                   IF        wk_Settled$ + wk_StlRqs$ = wk_AmtReq
     C                   EVAL      wk_RtnCode = 'UG'
     C                   EVAL      wk_MsgText =
     C                               'Payment will be settled in full.'
     C                   LEAVE
     C                   ENDIF                                                  wk_Settle$+wk_StlRqs

      *
      * Settle amount is deficient, and outstanding authorizations exist.
      *
     C                   WHEN      wk_Settled$ + wk_StlRqs$ < wk_AmtReq  and
     C                               wk_Auth$ > *Zero
     C                   CALLP(E)  SettleAuths (wk_Order#:wk_AmtReq:
     C                               wk_TaxAmtReq:wk_FatalErr2:wk_RtnCode2:
     C                               wk_MsgText2)
     C                   CALLP(E)  ClrTranFlds ()
     C                   EVAL      wk_Settled$ =
     C                               RtvOrdTranAmt(wk_Order#:'SETTLED')
     C                   EVAL      wk_StlRqs$ =
     C                               RtvOrdTranAmt(wk_Order#:'SETTLERQS')
     C                   EVAL      wk_Auth$ =
     C                               RtvOrdTranAmt(wk_Order#:'AUTHONLY')
     C                   IF        wk_Auth$ <> *Zero
     C                   CALLP(E)  VoidAuths (wk_Order#:wk_FatalErr2:
     C                               wk_RtnCode2)
     C                   ENDIF                                                  wk_Auth$<>*Zero
     C                   IF        wk_Settled$ + wk_StlRqs$ = wk_AmtReq
     C                   EVAL      wk_RtnCode = 'UG'
     C                   EVAL      wk_MsgText =
     C                               'Payment will be settled in full.'
     C                   LEAVE
     C                   ENDIF                                                  wk_Settle$+wk_StlRqs

     C                   ENDSL

      *
      * If we got here, we need to create a new transaction (either sale or refund).
      *
     C                   SELECT

     C                   WHEN      wk_Settled$ + wk_StlRqs$ < wk_AmtReq
     C                   IF        wk_Source = '-'
     C                   IF        not %open(HYPCCRH)
     C                   OPEN      HYPCCRH
     C                   ENDIF                                                  not %open(HYPCCRH)
     C     wk_Order#     SETGT     HYPCCRH                                      First - check for
     C                   DOU       %eof(HYPCCRH)  or  RHTYPE = 'RY'              archive record.
     C     wk_Order#     READPE    HYPCCRH                                       If found, use it.
     C                   ENDDO                                                   Otherwise, use
     C                   IF        %eof(HYPCCRH)                                 prior auth record.
     C     wk_Order#     SETGT     HYPCCRH
     C     wk_Order#     READPE    HYPCCRH
     C                   IF        %eof(HYPCCRH)
     C                   EVAL      RHUKEY = *Zero
     C                   ENDIF                                                  %eof(HYPCCRH)
     C                   ENDIF                                                  %eof(HYPCCRH)
     C                   EVAL      wk_Short$ = wk_AmtReq - wk_Settled$ +
     C                               wk_StlRqs$
     C                   EVAL      wk_TaxAmt = wk_TaxAmtReq
     C                   IF        wk_TaxAmt < *Zero
     C                   EVAL      wk_TaxAmt = *Zero
     C                   ENDIF                                                  wk_TaxAmt<*Zero
     C                   IF        wk_TaxAmt > *Zero
     C                   EVAL(H)   wk_TaxAmt =
     C                               wk_TaxAmt * (wk_Short$ / wk_AmtReq)
     C                   IF        wk_TaxAmt = *Zero
     C                   EVAL      wk_TaxAmt = .01
     C                   ENDIF                                                  wk_TaxAmt=*Zero
     C                   ENDIF                                                  wk_TaxAmt>*Zero
     C                   CALLP(E)  HYR0804C3 ('REAUTHSALE':RHUKEY:wk_Short$:
     C                               wk_TaxAmt:wk_Order#:wk_FatalErr:wk_RtnCode:
     C                               wk_NewKey:wk_MsgText)
     C                   EVAL      wk_Pgm0804 = *ON
     C                   IF        %error
     C                   EVAL      wk_RtnCode = 'UL'
     C                   EVAL      wk_FatalErr = 'H'
     C                   EVAL      wk_MsgText =
     C                               'ERROR ON CALL TO HYR0804C3 (REAUTHSALE)'
     C                   ENDIF                                                  %error
     C                   IF        wk_RtnCode = 'UG'
     C                   IF        not %open(HYPCCRH)
     C                   OPEN      HYPCCRH
     C                   ENDIF                                                  not %open(HYPCCRH)
     C                   EVAL      RHORD# = wk_Order#
     C                   EVAL      RHUKEY = wk_NewKey
     C                   EVAL      RHTYPE = 'RA'
     C                   EVAL      RHTYP2 = 'SA'
     C                   EVAL      RHAMT1 = wk_Short$
     C                   EVAL      RHUSER = ps_UserID
     C                   EVAL      RHTSTP = %timestamp
     C                   WRITE     HYRCCRH
     C                   ENDIF                                                  wk_RtnCode='UG'
     C                   ELSE
     C                   IF        not %open(HYPCCSF)
     C                   OPEN      HYPCCSF
     C                   ENDIF                                                  not %open(HYPCCSF)
      /Free
       wk_OEBLTO = *Zero;
       wk_CMCNA1 = *Blanks;
       EXEC SQL select OEBLTO into :wk_OEBLTO
                from OEORHD
                where OEORD#=:wk_Order#;
       EXEC SQL select CMCNA1 into :wk_CMCNA1
                from HDCUST
                where CMCUST=:wk_OEBLTO;
      /End-Free
     C                   EVAL      SFORD# = wk_Order#
     C                   EVAL      SFCUST = wk_OEBLTO
     C                   EVAL      SFNAME = wk_CMCNA1
     C                   EVAL      SFINVA = wk_AmtReq
     C                   EVAL      SFSTLA = wk_Settled$ + wk_StlRqs$
     C                   EVAL      SFSRC = wk_Source
     C                   EVAL      SFUSER = ps_UserID
     C                   EVAL      SFTSTP = %timestamp
     C                   EVAL      SFRPT = *Loval
     C                   WRITE     HYRCCSF
     C                   ENDIF                                                  wk_Source='-'

     C                   WHEN      wk_Settled$ + wk_StlRqs$ > wk_AmtReq
     C                   IF        wk_Source = '-'
     C                   IF        not %open(HYPCCRH)
     C                   OPEN      HYPCCRH
     C                   ENDIF                                                  not %open(HYPCCRH)
     C     wk_Order#     SETGT     HYPCCRH                                      First - check for
     C                   DOU       %eof(HYPCCRH)  or  RHTYPE = 'RY'              archive record.
     C     wk_Order#     READPE    HYPCCRH                                       If found, use it.
     C                   ENDDO                                                   Otherwise, use
     C                   IF        %eof(HYPCCRH)                                 prior auth record.
     C     wk_Order#     SETGT     HYPCCRH
     C     wk_Order#     READPE    HYPCCRH
     C                   IF        %eof(HYPCCRH)
     C                   EVAL      RHUKEY = *Zero
     C                   ENDIF                                                  %eof(HYPCCRH)
     C                   ENDIF                                                  %eof(HYPCCRH)
     C                   EVAL      wk_Long$ = wk_Settled$ + wk_StlRqs$ -
     C                               wk_AmtReq
     C                   EVAL      wk_TaxAmt = wk_TaxAmtReq
     C                   IF        wk_TaxAmt < *Zero
     C                   EVAL      wk_TaxAmt = *Zero
     C                   ENDIF                                                  wk_TaxAmt<*Zero
     C                   IF        wk_TaxAmt > *Zero
     C                   EVAL(H)   wk_TaxAmt =
     C                               wk_TaxAmt * (wk_Long$ / wk_AmtReq)
     C                   IF        wk_TaxAmt = *Zero
     C                   EVAL      wk_TaxAmt = .01
     C                   ENDIF                                                  wk_TaxAmt=*Zero
     C                   ENDIF                                                  wk_TaxAmt>*Zero
     C                   CALLP(E)  HYR0804C3 ('CREDIT':RHUKEY:wk_Long$:
     C                               wk_TaxAmt:wk_Order#:wk_FatalErr:wk_RtnCode:
     C                               wk_NewKey:wk_MsgText)
     C                   EVAL      wk_Pgm0804 = *ON
     C                   IF        %error
     C                   EVAL      wk_RtnCode = 'UL'
     C                   EVAL      wk_FatalErr = 'H'
     C                   EVAL      wk_MsgText =
     C                               'ERROR ON CALL TO HYR0804C3 (CREDIT)'
     C                   ENDIF                                                  %error
     C                   IF        wk_RtnCode = 'UG'
     C                   IF        not %open(HYPCCRH)
     C                   OPEN      HYPCCRH
     C                   ENDIF                                                  not %open(HYPCCRH)
     C                   EVAL      RHORD# = wk_Order#
     C                   EVAL      RHUKEY = wk_NewKey
     C                   EVAL      RHTYPE = 'RC'
     C                   EVAL      RHTYP2 = 'SA'
     C                   EVAL      RHAMT1 = wk_Long$
     C                   EVAL      RHUSER = ps_UserID
     C                   EVAL      RHTSTP = %timestamp
     C                   WRITE     HYRCCRH
     C                   ENDIF                                                  wk_RtnCode='UG'
     C                   ELSE
     C                   IF        not %open(HYPCCSF)
     C                   OPEN      HYPCCSF
     C                   ENDIF                                                  not %open(HYPCCSF)
      /Free
       wk_OEBLTO = *Zero;
       wk_CMCNA1 = *Blanks;
       EXEC SQL select OEBLTO into :wk_OEBLTO
                from OEORHD
                where OEORD#=:wk_Order#;
       EXEC SQL select CMCNA1 into :wk_CMCNA1
                from HDCUST
                where CMCUST=:wk_OEBLTO;
      /End-Free
     C                   EVAL      SFORD# = wk_Order#
     C                   EVAL      SFCUST = wk_OEBLTO
     C                   EVAL      SFNAME = wk_CMCNA1
     C                   EVAL      SFINVA = wk_AmtReq
     C                   EVAL      SFSTLA = wk_Settled$ + wk_StlRqs$
     C                   EVAL      SFSRC = wk_Source
     C                   EVAL      SFUSER = ps_UserID
     C                   EVAL      SFTSTP = %timestamp
     C                   EVAL      SFRPT = *Loval
     C                   WRITE     HYRCCSF
     C                   ENDIF                                                  wk_Source='-'


     C                   ENDSL

     C                   ENDDO

     C                   EVAL      wk_NewCCAmt =
     C                               RtvOrdTranAmt(wk_Order#:'SETTLED') +
     C                               RtvOrdTranAmt(wk_Order#:'SETTLERQS') +
     C                               RtvOrdTranAmt(wk_Order#:'AUTHONLY')

     C                   EVAL      NewCCAmt = wk_NewCCAmt
     C                   EVAL      FatalErr = wk_FatalErr
     C                   EVAL      RtnCode = wk_RtnCode
     C                   EVAL      MsgText = wk_MsgText

      *
      * Return to calling procedure.
      *
     C                   RETURN

     P SettleFinal     E

     P/EJECT
      **************************************************************************
      * Maximize any outstanding authorizations flagged for settlement.        *
      **************************************************************************
     P MaxSettle       B

     D                 PI
     D Order#                         8P 0 CONST
     D AmtReq                        11P 2 CONST
     D TaxAmtReq                     11P 2 CONST
     D FatalErr                       1A
     D RtnCode                        2A
     D MsgText                       35A

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Order#       S              8P 0
     D wk_AmtReq       S             11P 2
     D wk_TaxAmtReq    S             11P 2
     D wk_FatalErr     S              1A
     D wk_RtnCode      S              2A
     D wk_MsgText      S             35A
     D wk_Settled$     S             11P 2                                      Order settled $
     D wk_StlRqs$      S             11P 2                                      Order rqs stl $
     D wk_Auth$        S             11P 2                                      Order auth $
     D wk_Short$       S             11P 2                                      Short $
     D wk_StlRqs$2     S             11P 2                                      Tran rqs stl $
     D wk_Auth$2       S             11P 2                                      Tran auth $
     D wk_NewStl$      S             11P 2                                      New settle $
     D wk_TaxAmt       S             11P 2                                      New settle tax $
     D wk_CCFlags      S             21A
     D wk_FatalErr2    S              1A
     D wk_RtnCode2     S              2A
     D wk_MsgText2     S             35A
     D wk_NewKey       S             15A

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_AmtReq = AmtReq
     C                   EVAL      wk_TaxAmtReq = TaxAmtReq
     C                   EVAL      wk_FatalErr = *Blank
     C                   EVAL      wk_RtnCode = *Blanks
     C                   EVAL      wk_MsgText = *Blanks

     C                   EVAL      wk_Settled$ =
     C                               RtvOrdTranAmt(wk_Order#:'SETTLED')
     C                   EVAL      wk_StlRqs$ =
     C                               RtvOrdTranAmt(wk_Order#:'SETTLERQS')
     C                   EVAL      wk_Short$ = wk_AmtReq - wk_Settled$ +
     C                               wk_StlRqs$

     C                   IF        not %open(HYPCCRH)
     C                   OPEN      HYPCCRH
     C                   ENDIF                                                  not %open(HYPCCRH)
     C     wk_Order#     SETLL     HYPCCRH
     C                   DOU       %eof(HYPCCRH)
     C     wk_Order#     READE     HYPCCRH
     C                   IF        %eof(HYPCCRH)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPCCRH)
     C                   IF        RHTYPE = 'RY'
     C                   ITER
     C                   ENDIF                                                  RHTYPE='RY'
     C                   EVAL      wk_CCFlags = RtvTranData(RHUKEY:'FLAGS')
     C                   IF        %subst(wk_CCFlags:6:1) = '0'  and            Not settled yet
     C                               %subst(wk_CCFlags:4:1) = '1'  and          Requested to settle
     C                               %subst(wk_CCFlags:17:1) = '0'              Not locally voided
     C                   EVAL      wk_Auth$2 = RtvTranAmt(RHUKEY:'TRANAMT')
     C                   EVAL      wk_StlRqs$2 = RtvTranAmt(RHUKEY:'SETTLERQS')
     C                   IF        wk_StlRqs$2 < wk_Auth$2
     C                   IF        wk_Short$ > wk_Auth$2 - wk_StlRqs$2
     C                   EVAL      wk_NewStl$ = wk_Auth$2 - wk_StlRqs$2
     C                   ELSE
     C                   EVAL      wk_NewStl$ = wk_Short$
     C                   ENDIF                                                  wk_Short$>wk_Auth$2-
     C                   EVAL      wk_TaxAmt = wk_TaxAmtReq
     C                   IF        wk_TaxAmt < *Zero
     C                   EVAL      wk_TaxAmt = *Zero
     C                   ENDIF                                                  wk_TaxAmt<*Zero
     C                   IF        wk_TaxAmt > *Zero
     C                   EVAL(H)   wk_TaxAmt =
     C                               wk_TaxAmt * (wk_NewStl$ / wk_AmtReq)
     C                   IF        wk_TaxAmt = *Zero
     C                   EVAL      wk_TaxAmt = .01
     C                   ENDIF                                                  wk_TaxAmt=*Zero
     C                   ENDIF                                                  wk_TaxAmt>*Zero
     C                   CALLP(E)  HYR0804C3 ('FLAGSETTLE':RHUKEY:wk_NewStl$:
     C                               wk_TaxAmt:*Zero:wk_FatalErr2:wk_RtnCode2:
     C                               wk_NewKey:wk_MsgText2)
     C                   EVAL      wk_Pgm0804 = *ON
     C                   IF        %error
     C                   EVAL      wk_RtnCode2 = 'UL'
     C                   EVAL      wk_FatalErr2 = 'H'
     C                   EVAL      wk_MsgText2 =
     C                               'ERROR ON CALL TO HYR0804C3 (FLAGSETTLE)'
     C                   ENDIF                                                  %error
     C                   CALLP(E)  ClrTranFlds ()
     C                   EVAL      wk_StlRqs$ =
     C                               RtvOrdTranAmt(wk_Order#:'SETTLERQS')
     C                   EVAL      wk_Short$ = wk_AmtReq - wk_Settled$ +
     C                               wk_StlRqs$
     C                   IF        wk_Short$ = *Zero
     C                   EVAL      wk_Auth$ =
     C                               RtvOrdTranAmt(wk_Order#:'AUTHONLY')
     C                   IF        wk_Auth$ <> *Zero
     C                   CALLP(E)  VoidAuths (wk_Order#:wk_FatalErr2:
     C                               wk_RtnCode2)
     C                   ENDIF                                                  wk_Auth$<>*Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_Short$=*Zero
     C                   ENDIF                                                  wk_StlRqs$2<wk_Auth$
     C                   ENDIF                                                  %subst(wk_CCFlags:6:
     C                   ENDDO                                                  %eof(HYPCCRH)

     C                   IF        wk_Short$ = *Zero
     C                   EVAL      wk_RtnCode = 'UG'
     C                   EVAL      wk_MsgText =
     C                               'Payment will be settled in full.'
     C                   LEAVE
     C                   ENDIF                                                  wk_Short$=*Zero

     C                   EVAL      wk_Auth$ =
     C                               RtvOrdTranAmt(wk_Order#:'AUTHONLY')
     C                   IF        wk_Short$ > *Zero  and  wk_Auth$ > *Zero
     C                   EVAL      wk_TaxAmt = wk_TaxAmtReq
     C                   IF        wk_TaxAmt < *Zero
     C                   EVAL      wk_TaxAmt = *Zero
     C                   ENDIF                                                  wk_TaxAmt<*Zero
     C                   IF        wk_TaxAmt > *Zero
     C                   EVAL(H)   wk_TaxAmt =
     C                               wk_TaxAmt * (wk_Short$ / wk_AmtReq)
     C                   IF        wk_TaxAmt = *Zero
     C                   EVAL      wk_TaxAmt = .01
     C                   ENDIF                                                  wk_TaxAmt=*Zero
     C                   ENDIF                                                  wk_TaxAmt>*Zero
     C                   CALLP(E)  SettleAuths (wk_Order#:wk_Short$:
     C                               wk_TaxAmt:wk_FatalErr2:wk_RtnCode2:
     C                               wk_MsgText2)
     C                   ENDIF                                                  wk_Short$>*Zero and

     C                   ENDDO

     C                   EVAL      FatalErr = wk_FatalErr
     C                   EVAL      RtnCode = wk_RtnCode
     C                   EVAL      MsgText = wk_MsgText

      *
      * Return to calling procedure.
      *
     C                   RETURN

     P MaxSettle       E

     P/EJECT
      **************************************************************************
      * Settle any outstanding authorizations for an order.                    *
      **************************************************************************
     P SettleAuths     B

     D                 PI
     D Order#                         8P 0 CONST
     D AmtReq                        11P 2 CONST
     D TaxAmtReq                     11P 2 CONST
     D FatalErr                       1A
     D RtnCode                        2A
     D MsgText                       35A

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Order#       S              8P 0
     D wk_AmtReq       S             11P 2
     D wk_TaxAmtReq    S             11P 2
     D wk_FatalErr     S              1A
     D wk_RtnCode      S              2A
     D wk_MsgText      S             35A
     D wk_Settled$     S             11P 2                                      Order settled $
     D wk_StlRqs$      S             11P 2                                      Order rqs stl $
     D wk_Short$       S             11P 2                                      Short $
     D wk_Auth$2       S             11P 2                                      Tran auth $
     D wk_NewStl$      S             11P 2                                      New settle $
     D wk_TaxAmt       S             11P 2                                      New settle tax $
     D wk_CCFlags      S             21A
     D wk_FatalErr2    S              1A
     D wk_RtnCode2     S              2A
     D wk_MsgText2     S             35A
     D wk_NewKey       S             15A
     D wk_StaleCut     S              8A

      **************************************************************************
      * Define data structures.                                                *
      **************************************************************************
     D                 DS
     D td_TimeDate             1     14  0
     D td_Time                 1      6  0
     D td_Date                 7     14  0

     D                 DS
     D ds_YYMD                 1      8
     D ds_MDYY                 5     12  0
     D ds_YYYY1                1      4  0
     D ds_YYYY2                9     12  0
     D ds_MM                   5      6  0

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_AmtReq = AmtReq
     C                   EVAL      wk_TaxAmtReq = TaxAmtReq
     C                   EVAL      wk_FatalErr = *Blank
     C                   EVAL      wk_RtnCode = *Blanks
     C                   EVAL      wk_MsgText = *Blanks

     C                   EVAL      wk_Settled$ =
     C                               RtvOrdTranAmt(wk_Order#:'SETTLED')
     C                   EVAL      wk_StlRqs$ =
     C                               RtvOrdTranAmt(wk_Order#:'SETTLERQS')
     C                   EVAL      wk_Short$ = wk_AmtReq - wk_Settled$ +
     C                               wk_StlRqs$

      *
      * Determine sale authorization cutoff date.  Code uses 1 month prior to current
      * date, which could be anywhere from 28 to 31 days.  Anything prior to then will
      * be considered stale.
      *
     C                   TIME                    td_TimeDate
     C                   EVAL      ds_MDYY = td_Date
     C                   EVAL      ds_YYYY1 = ds_YYYY2
     C                   IF        ds_MM = 1
     C                   EVAL      ds_MM = 12
     C                   EVAL      ds_YYYY1 = ds_YYYY1 - 1
     C                   ELSE
     C                   EVAL      ds_MM = ds_MM - 1
     C                   ENDIF                                                  ds_MM=1
     C                   EVAL      wk_StaleCut = ds_YYMD

     C                   IF        not %open(HYPCCRH)
     C                   OPEN      HYPCCRH
     C                   ENDIF                                                  not %open(HYPCCRH)
     C     wk_Order#     SETLL     HYPCCRH
     C                   DOU       %eof(HYPCCRH)
     C     wk_Order#     READE     HYPCCRH
     C                   IF        %eof(HYPCCRH)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPCCRH)
     C                   IF        RHTYPE = 'RY'
     C                   ITER
     C                   ENDIF                                                  RHTYPE='RY'
     C                   EVAL      wk_CCFlags = RtvTranData(RHUKEY:'FLAGS')
     C                   IF        %subst(wk_CCFlags:6:1) = '0'  and            Not settled yet
     C                               %subst(wk_CCFlags:4:1) = '0'  and          Not rqs to settle
     C                               %subst(wk_CCFlags:2:1) = '1'  and          Authorized
     C                               %subst(wk_CCFlags:17:1) = '0'              Not locally voided
     C                   IF        RtvTranData(RHUKEY:'TRANDATE') < wk_StaleCut
     C                   ITER
     C                   ENDIF                                                  RtvTranData(RHUKEY:
     C                   EVAL      wk_Auth$2 = RtvTranAmt(RHUKEY:'TRANAMT')
     C                   IF        wk_Auth$2 = 1.00  and  wk_Auth$2 < wk_Short$
     C                   ITER
     C                   ENDIF                                                  wk_Auth$2=1.00 and
     C                   IF        wk_Auth$2 > *Zero
     C                   IF        wk_Short$ > wk_Auth$2
     C                   EVAL      wk_NewStl$ = wk_Auth$2
     C                   ELSE
     C                   EVAL      wk_NewStl$ = wk_Short$
     C                   ENDIF                                                  wkShort$>wk_Auth$2
     C                   EVAL      wk_TaxAmt = wk_TaxAmtReq
     C                   IF        wk_TaxAmt < *Zero
     C                   EVAL      wk_TaxAmt = *Zero
     C                   ENDIF                                                  wk_TaxAmt<*Zero
     C                   IF        wk_TaxAmt > *Zero
     C                   EVAL(H)   wk_TaxAmt =
     C                               wk_TaxAmt * (wk_NewStl$ / wk_AmtReq)
     C                   IF        wk_TaxAmt = *Zero
     C                   EVAL      wk_TaxAmt = .01
     C                   ENDIF                                                  wk_TaxAmt=*Zero
     C                   ENDIF                                                  wk_TaxAmt>*Zero
     C                   CALLP(E)  HYR0804C3 ('FLAGSETTLE':RHUKEY:wk_NewStl$:
     C                               wk_TaxAmt:*Zero:wk_FatalErr2:wk_RtnCode2:
     C                               wk_NewKey:wk_MsgText2)
     C                   EVAL      wk_Pgm0804 = *ON
     C                   IF        %error
     C                   EVAL      wk_RtnCode2 = 'UL'
     C                   EVAL      wk_FatalErr2 = 'H'
     C                   EVAL      wk_MsgText2 =
     C                               'ERROR ON CALL TO HYR0804C3 (FLAGSETTLE)'
     C                   ENDIF                                                  %error
     C                   CALLP(E)  ClrTranFlds ()
     C                   EVAL      wk_StlRqs$ =
     C                               RtvOrdTranAmt(wk_Order#:'SETTLERQS')
     C                   EVAL      wk_Short$ = wk_AmtReq - wk_Settled$ +
     C                               wk_StlRqs$
     C                   IF        wk_Short$ = *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_Short$=*Zero
     C                   ENDIF                                                  wk_Auth$2>*Zero
     C                   ENDIF                                                  %subst(wk_CCFlags:6:
     C                   ENDDO                                                  %eof(HYPCCRH)

     C                   ENDDO

     C                   EVAL      FatalErr = wk_FatalErr
     C                   EVAL      RtnCode = wk_RtnCode
     C                   EVAL      MsgText = wk_MsgText

      *
      * Return to calling procedure.
      *
     C                   RETURN

     P SettleAuths     E

     P/EJECT
      **************************************************************************
      * Void any outstanding authorizations for an order.                      *
      **************************************************************************
     P VoidAuths       B

     D                 PI
     D Order#                         8P 0 CONST
     D FatalErr                       1A
     D RtnCode                        2A

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Order#       S              8P 0
     D wk_FatalErr     S              1A
     D wk_RtnCode      S              2A

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_FatalErr = *Blank
     C                   EVAL      wk_RtnCode = *Blanks

     C                   IF        not %open(HYPCCRH)
     C                   OPEN      HYPCCRH
     C                   ENDIF                                                  not %open(HYPCCRH)

     C     wk_Order#     SETLL     HYPCCRH
     C                   DOU       %eof(HYPCCRH)
     C     wk_Order#     READE     HYPCCRH
     C                   IF        %eof(HYPCCRH)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPCCRH)
     C                   IF        RHTYPE = 'RY'
     C                   ITER
     C                   ENDIF                                                  RHTYPE='RY'
     C                   IF        RtvTranAmt(RHUKEY:'AUTHONLY') <> *Zero
     C                   CALLP(E)  HYR0802C3 ('RL':RHUKEY:wk_FatalErr:          Rqs local void
     C                               wk_RtnCode)
     C                   EVAL      wk_Pgm0802 = *ON
     C                   IF        %error
     C                   EVAL      wk_RtnCode = 'UL'
     C                   EVAL      wk_FatalErr = 'H'
     C                   ENDIF                                                  %error
     C                   ENDIF                                                  RtvTranAmt(RHUKEY:
     C                   ENDDO                                                  %eof(HYPCCRH)

     C                   ENDDO

     C                   EVAL      FatalErr = wk_FatalErr
     C                   EVAL      RtnCode = wk_RtnCode

      *
      * Return to calling procedure.
      *
     C                   RETURN

     P VoidAuths       E

     P/EJECT
      **************************************************************************
      * Shut down service program                                              *
      **************************************************************************
     P sd_HYR0812      B                   EXPORT

     D                 PI            10I 0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Error        S             10I 0
     D wk_FatalErr     S              1A
     D wk_RtnCode      S              2A
     D wk_MsgText      S             35A
     D wk_NewKey       S             15A

     C                   EVAL      wk_Error = *Zero

     C                   DO

     C                   IF        %open(HYPCCRH)
     C                   CLOSE     HYPCCRH
     C                   ENDIF                                                  %open(HYPCCRH)

     C                   IF        %open(HYPCCSF)
     C                   CLOSE     HYPCCSF
     C                   ENDIF                                                  %open(HYPCCSF)

     C                   IF        wk_Pgm0802
     C                   CALLP(E)  HYR0802C3 ('SHTD':*Zero:wk_FatalErr:
     C                               wk_RtnCode)
     C                   ENDIF                                                  wk_Pgm0802

     C                   IF        wk_Pgm0804
     C                   CALLP(E)  HYR0804C3 ('SHUTDOWN':*Zero:*Zero:
     C                               *Zero:*Zero:wk_FatalErr:wk_RtnCode:
     C                               wk_NewKey:wk_MsgText)
     C                   ENDIF                                                  wk_Pgm0804

     C                   IF        wk_Pgm0810
     C                   CALLP(E)  sd_HYR0810
     C                   ENDIF                                                  wk_Pgm0810

     C                   ENDDO

      *
      * Return the error code.
      *
     C                   RETURN    wk_Error

     P sd_HYR0812      E

     P/EJECT
