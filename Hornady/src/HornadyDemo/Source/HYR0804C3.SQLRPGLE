     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2017')
     H BNDDIR('HYR0804C3')
      **************************************************************************
      * APPLICATION: Credit Card Processing                                    *
      *       TITLE: CurbstoneCard C3 Request Interface 2                      *
      *          BY: William Earl (MCA)                                        *
      *        DATE: June 2017                                                 *
      *                                                                        *
      * Valid functions:                                                       *
      *              'REAUTH'      Retrieve prior authorization and            *
      *                            reauthorize card.                           *
      *              'REAUTHSALE'  Retrieve prior authorization and            *
      *                            reauthorize card.  If approved, will        *
      *                            request flag to settle, so the transaction  *
      *                            can be processed with the next settlement.  *
      *              'CREDIT'      Retrieve prior authorization and            *
      *                            reauthorize card for credit.  If approved,  *
      *                            will request flag to settle, so the         *
      *                            transaction can be processed with the next  *
      *                            settlement.                                 *
      *              'FLAGSETTLE'  Flag to settle.  Optionally provide         *
      *                            requested settlement amount.                *
      *              'SHUTDOWN'    Shut down program.                          *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  03/15/2018  MCA  Partial authorizations.                              *
      *  04/11/2018  MCA  EMV device programming.                              *
      *                                                                        *
      **************************************************************************

     FCCLOG     O    E             DISK    BLOCK(*NO) USROPN

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY QPRPSRC,HYR0804C3

      /COPY QPRPSRC,HYR0802C3

     D HasNumbers      PR             1N
     D  Field                        26A   CONST

      **************************************************************************
      * Define *Entry parameter list.                                          *
      **************************************************************************
     D HYR0804C3       PI
     D  in_Function                  10A   CONST
     D  in_UniqueKey                 15A   CONST
     D  in_TransAmt                  11P 2 CONST
     D  in_TaxAmt                    11P 2 CONST
     D  in_Order#                     8P 0 CONST
     D  out_FatalErr                  1A
     D  out_RtnCode                   2A
     D  out_NewKey                   15A
     D  out_MsgText                  35A

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID
     D wk_MerchID      S              5A   DTAARA(CCMRCHID)                     Merchant ID
     D wk_TestMerch    S             10A   DTAARA(CCMRCHTEST)                   TEST merchant
     D wk_Log          S              1A   DTAARA(CCLOG)                        Log transactions

      **************************************************************************
      * Define data structures.                                                *
      **************************************************************************
     D C#STRU_sv     E DS          2048    EXTNAME(C3FP0000) PREFIX(sv_)

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Function     S             10A
     D wk_UniqueKey    S             15A
     D wk_TransAmt     S             11P 2
     D wk_TaxAmt       S             11P 2
     D wk_Order#       S              8P 0
     D wk_FatalErr     S              1A
     D wk_RtnCode      S              2A
     D wk_FatalErr2    S              1A
     D wk_RtnCode2     S              2A
     D wk_NewKey       S             15A
     D wk_MsgText      S             35A
     D wk_OEORD#       S              8S 0
     D wk_OEBLTO       S              7S 0
     D wk_CMCUST       S              7S 0
     D wk_CMCNA2       S             26A
     D wk_CMCNA3       S             26A
     D wk_CMZIP        S             13A
     D wk_CMCTRY       S              3A
     D wk_BillingAddr  S             26A
     D wk_BillingZip   S             13A
     D wk_MFMRCH       S              5A
     D wk_MFSTID       S              8A
     D wk_MFADD2       S             35A
     D wk_LogMsg       S             52A
     D wk_Pgm0802      S              1N   INZ(*OFF)
     D sv_MFCARDLog    S                   LIKE(MFCARD)

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D ErrMsg01        C                   'Prior authorization not found (01).'

      **************************************************************************
      * Curbstone definitions.                                                 *
      **************************************************************************
      /COPY C3/QRPGLESRC,C3RC0003

     C/EJECT
      **************************************************************************
      * Main routine.                                                          *
      **************************************************************************

     C                   EVAL      wk_Function = in_Function
     C                   EVAL      wk_UniqueKey = in_UniqueKey
     C                   EVAL      wk_TransAmt = in_TransAmt
     C                   EVAL      wk_TaxAmt = in_TaxAmt
     C                   EVAL      wk_Order# = in_Order#
     C                   EVAL      wk_FatalErr = *Blank
     C                   EVAL      wk_RtnCode = *Blanks
     C                   EVAL      wk_NewKey = *Blanks
     C                   EVAL      wk_MsgText = *Blanks

     C                   SELECT

     C                   WHEN      wk_Function = 'SHUTDOWN'
     C                   EXSR      C@LAST                                       Shut down srv pgms
     C                   IF        wk_Pgm0802
     C                   CALLP(E)  HYR0802C3
     C                               ('SHTD':*Blanks:wk_FatalErr2:wk_RtnCode2)
     C                   ENDIF                                                  wk_Pgm0802
     C                   EVAL      *INLR = *ON

     C                   OTHER
     C                   EXSR      $Process

     C                   ENDSL

     C                   EVAL      out_FatalErr = wk_FatalErr
     C                   EVAL      out_RtnCode = wk_RtnCode
     C                   EVAL      out_NewKey = wk_NewKey
     C                   EVAL      out_MsgText = wk_MsgText

     C                   RETURN

     C/EJECT
      **************************************************************************
      * Process transaction.                                                   *
      **************************************************************************
     CSR   $Process      BEGSR

      *
      * Retrieve whether to log transactions.
      *
     C                   IN        wk_Log
     C                   IF        wk_Log = 'Y'  or                             Before and after
     C                               wk_Log = 'B'  or                           Before process only
     C                               wk_Log = 'A'                               After process only
     C                   IF        not %open(CCLOG)
     C                   OPEN      CCLOG
     C                   ENDIF                                                  not %open(CCLOG)
     C                   ENDIF                                                  wk_Log='Y'

      *
      * Clear data structure.
      *
     C                   CLEAR                   C#STRU

      *
      * Retrieve prior authorization data.
      *
     C                   IF        wk_Function <> 'FLAGSETTLE'
     C                   EXSR      $RtvCardData
     C                   SELECT
     C                   WHEN      MFUKEY <> wk_UniqueKey                       Response must match
     C                   EVAL      wk_FatalErr = 'E'
     C                   EVAL      wk_RtnCode = '99'
     C                   EVAL      wk_NewKey = *Blanks
     C                   EVAL      wk_MsgText = ErrMsg01
     C                   LEAVESR
     C                   WHEN      MFATAL = '8'  or                             Timeout
     C                               %subst(C#RMSG:1:3) <> 'TXN'  or            Unknown response
     C                               (MFRTRN <> 'UG'  and  MFTYPE <> 'RY')      Declined
     C                   EVAL      wk_FatalErr = MFATAL
     C                   EVAL      wk_RtnCode = MFRTRN
     C                   EVAL      wk_NewKey = *Blanks
     C                   EVAL      wk_MsgText = MFRTXT
     C                   LEAVESR
     C                   ENDSL

      *
      * Save data structure.
      *
     C                   EVAL      C#STRU_sv = C#STRU

      *
      * If prior authorization has not yet been locally voided, local void it.
      *
     C****               IF        (MFTYPE = 'RA'  or  MFTYPE = 'RC')  and
     C****                           MF$SLG = *OFF  and  MF$FTS = *OFF
     C****               IF        MF$LVD = *OFF
     C****               EXSR      $LocalVoid
     C****               ENDIF                                                  MF$LVD=*OFF
     C****               EVAL      MF$LVD = *Blank
     C****               ENDIF                                                  MFTYPE='RA' or
     C                   ENDIF                                                  wk_Function<>'FLAGSE

      *
      * Modify data structure.
      *
     C                   EXSR      $FillDS

      *
      * If logging requested, write log record (before processing).
      *
     C                   IF        wk_Log = 'Y'  or  wk_Log = 'B'
     C                   EVAL      sv_MFCARDLog = MFCARD
     C                   IF        %len(%trimr(MFCARD)) = 15
     C                   EVAL      MFCARD = '***********' + %subst(MFCARD:12:4)
     C                   ELSE
     C                   EVAL      MFCARD = '************' + %subst(MFCARD:13:4)
     C                   ENDIF                                                  %len(%trimr(MFCARD))
     C                   WRITE     CC9020
     C                   EVAL      MFCARD = sv_MFCARDLog
     C                   EVAL      sv_MFCARDLog = *Blanks
     C                   ENDIF                                                  wk_Log='Y' or

      *
      * Send request.
      *
     C                   EXSR      C@SEND

      *
      * Receive response.
      *
     C                   EXSR      C@RECV

      *
      * Retrieve the results.
      *
     C                   EXSR      $Result

      *
      * If prior authorization has not yet been locally voided, local void it.
      *
     C                   IF        wk_Function <> 'FLAGSETTLE'
     C                   IF        (sv_MFTYPE = 'RA'  or  sv_MFTYPE = 'RC')  and
     C                               sv_MF$SLG = *OFF  and  sv_MF$FTS = *OFF
     C                   IF        sv_MF$LVD = *OFF
     C                               and  %subst(wk_UniqueKey:6:1) <> 'Y'
     C                   EXSR      $LocalVoid
     C                   ENDIF                                                  sv_MF$LVD=*OFF
     C                   ENDIF                                                  sv_MFTYPE='RA' or
     C                   ENDIF                                                  wk_Function<>'FLAGSE

      *
      * If logging requested, write log record (after processing).
      *
     C                   IF        wk_Log = 'Y'  or  wk_log = 'A'
     C                   WRITE     CC9020
     C                   ENDIF                                                  wk_Log='Y' or

      *
      * Clear data.
      *
     C                   CLEAR                   C#STRU

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Retrieve the results.                                                  *
      **************************************************************************
     CSR   $Result       BEGSR

     C                   EVAL      wk_FatalErr = MFATAL
     C                   EVAL      wk_RtnCode = MFRTRN
     C                   EVAL      wk_NewKey = MFUKEY
     C                   EVAL      wk_MsgText = MFRTXT

      *
      * Handle partial authorizations.  Return to caller as if the authorization
      * was denied.
      *
     C                   IF        (wk_Function = 'REAUTH' and
     C                               wk_RtnCode = 'UG'  and
     C                               MFSETR <> *Zero)  or
     C                             (wk_Function = 'REAUTHSALE' and
     C                               wk_RtnCode = 'UG'  and
     C                               MFSETR <> MFAMT1)
     C                   IF        wk_Function = 'REAUTHSALE'
     C                   CALLP(E)  HYR0802C3                                    Rqs unflag to settle
     C                               ('RSSR':MFUKEY:wk_FatalErr2:wk_RtnCode2)
     C                   EVAL      wk_Pgm0802 = *ON
     C                   ENDIF                                                  wk_Function='REAUTHS
     C                   EVAL      wk_FatalErr = '$'
     C                   EVAL      wk_RtnCode = 'UL'
     C                   EVAL      wk_NewKey = *Blanks
     C                   EVAL      wk_MsgText = 'PARTIAL AUTHORIZATION'
     C                   ENDIF                                                  wk_Function='REAUTH'

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Modify the data structure.                                             *
      **************************************************************************
     CSR   $FillDS       BEGSR

      /Free
       wk_OEORD# = *Zero;
       wk_OEBLTO = *Zero;
       wk_CMCUST = *Zero;
       wk_CMCNA2 = *Blanks;
       wk_CMCNA3 = *Blanks;
       wk_CMZIP = *Blanks;
       wk_CMCTRY = *Blanks;
       wk_BillingAddr = *Blanks;
       wk_BillingZip = *Blanks;
       EXEC SQL select OEORD#, OEBLTO
                 into :wk_OEORD#, :wk_OEBLTO
                from OEORHD
                where OEORD#=:wk_Order#;
       IF wk_OEORD# = wk_Order#;
         EXEC SQL select CMCUST, CMCNA2, CMCNA3, CMZIP, CMCTRY
                   into :wk_CMCUST, :wk_CMCNA2, :wk_CMCNA3, :wk_CMZIP,
                        :wk_CMCTRY
                  from HDCUST
                  where CMCUST=:wk_OEBLTO;
         IF wk_CMCUST = wk_OEBLTO;
           IF wk_CMCNA3 = *Blanks  or  HasNumbers(wk_CMCNA2)  or
               not HasNumbers(wk_CMCNA3);
             wk_BillingAddr = wk_CMCNA2;
           ELSE;
             wk_BillingAddr = wk_CMCNA2;
           ENDIF;                                                            // wk_CMCNA3=*Blanks
           IF wk_CMCTRY = 'US';
             wk_BillingZip = %subst(wk_CMZIP:1:5);
           ELSE;
             wk_BillingZip = wk_CMZIP;
           ENDIF;                                                            // wk_CMCTRY='US'
         ENDIF;                                                              // wk_CMCUST=wk_OEBLTO
       ENDIF;                                                                // wk_OEORD#=wk_Order#;
       // IF wk_BillingAddr = *Blanks  or  wk_BillingZip=*Blanks;
       IF wk_BillingAddr = *Blanks  or
          ((wk_CMCTRY = 'US' or wk_CMCTRY = 'CA')  and wk_BillingZip=*Blanks);
         wk_BillingAddr = sv_MFADD1;
         wk_BillingZip = sv_MFZIPC;
         wk_LogMsg = 'Order #' + %triml(%editc(wk_Order#:'Z')) +
           ' CC addr/zip dftd from prior auth.';
         DSPLY wk_LogMsg;
       ENDIF;                                                                // wk_BillingAddr=*Blan
      /End-Free

      *
      * Clear data structure.
      *
     C                   CLEAR                   C#STRU

      *
      * Populate level I fields.
      *
     C                   SELECT
     C                   WHEN      wk_Function = 'REAUTH'
     C                   EVAL      MFUKEY = *Blanks
     C                   EVAL      MFKEYP = wk_UniqueKey
     C                   EVAL      MFTYPE = 'RA'                                 Request auth
     C                   EVAL      MFTYP2 = 'PA'                                 Preauth trans
     C                   WHEN      wk_Function = 'REAUTHSALE'
     C                   EVAL      MFUKEY = *Blanks
     C                   EVAL      MFKEYP = wk_UniqueKey
     C                   EVAL      MFTYPE = 'RA'                                 Request auth
     C                   EVAL      MFTYP2 = 'SA'                                 Sale trans
     C                   WHEN      wk_Function = 'CREDIT'
     C                   EVAL      MFUKEY = *Blanks
     C                   EVAL      MFKEYP = wk_UniqueKey
     C                   EVAL      MFTYPE = 'RC'                                 Rsq credit auth
     C                   EVAL      MFTYP2 = 'SA'                                 Flag to settle
     C                   WHEN      wk_Function = 'FLAGSETTLE'
      /Free
       wk_MFMRCH=*Blanks;
       wk_MFSTID=*Blanks;
       wk_MFADD2=*Blanks;
       EXEC SQL Select MFMRCH, MFSTID, MFADD2
                 into :wk_MFMRCH, :wk_MFSTID, :wk_MFADD2
                From C3/C3FP9020
                Where MFUKEY=:wk_UniqueKey;
      /End-Free
     C                   EVAL      MFUKEY = wk_UniqueKey
     C                   IF        wk_MFSTID = 'PAX'
     C                   EVAL      MFTYPE = 'CS'
     C                   EVAL      MFMRCH = wk_MFMRCH
     C                   EVAL      MFSTID = wk_MFSTID
     C                   EVAL      MFADD2 = wk_MFADD2
     C                   ELSE
     C                   EVAL      MFTYPE = 'RS'
     C                   ENDIF                                                  wk_MFSTID='PAX'
     C                   EVAL      MFSETR = wk_TransAmt
     C                   LEAVESR
     C                   ENDSL

     C****               IF        sv_MFTRK1 = *Blanks
     C****               EVAL      MFCARD = sv_MFCARD
     C****               EVAL      MFEDAT = sv_MFEDAT
     C****               EVAL      MFNAME = sv_MFNAME
     C****               EVAL      MFADD1 = sv_MFADD1
     C****               EVAL      MFZIPC = sv_MFZIPC
     C****               ENDIF                                                  sv_MFTRK1=*Blanks
     C                   EVAL      MFNAME = sv_MFNAME
     C                   EVAL      MFADD1 = wk_BillingAddr
     C                   EVAL      MFZIPC = wk_BillingZip
     C                   EVAL      MFAMT1 = wk_TransAmt
     C                   EVAL      MFTRK1 = sv_MFTRK1
     C                   EVAL      MFUSER = P#USID

     C                   IF        wk_DatabaseID = *Blanks
     C                   IN        wk_DatabaseID
     C                   ENDIF                                                  wk_DatabaseID=*Blank
     C                   IN        wk_MerchID
     C                   IF        wk_DatabaseID = 'HY'
     C                               or  wk_DatabaseID = 'SS'
     C****               EVAL      MFMRCH = '00001'
     C                   EVAL      MFMRCH = wk_MerchID
     C                   ELSE
     C                   IN        wk_TestMerch
     C                   SELECT
     C                   WHEN      wk_TestMerch = 'SIMULATOR'
     C                   EVAL      MFMRCH = '99999'
     C                   WHEN      wk_TestMerch = 'TESTSERVER'
     C                   EVAL      MFMRCH = '97777'                             TSYS test
     C****               IF        sv_MFTRK1 = *Blanks
     C****               EVAL      MFMRCH = '98771'
     C****               ELSE
     C****               EVAL      MFMRCH = '98774'
     C****               ENDIF                                                  sv_MFTRK1=*Blanks
     C                   WHEN      wk_TestMerch = 'LIVE'
     C****               EVAL      MFMRCH = '00001'
     C                   EVAL      MFMRCH = wk_MerchID
     C                   OTHER
     C                   EVAL      MFMRCH = '99999'
     C                   ENDSL
     C                   ENDIF                                                  wk_DatabaseID='HY'
     C****               IF        sv_MFCVV2 = *Blanks  and
     C****                           (MFMRCH = '00001'  or                      Live
     C****                            MFMRCH = '99999')  and                    Simulator
     C****                            sv_MFTRK1 = *Blanks  and
     C****                            sv_MFRVNA <> 'DISC'                       DiscoverCard
     C****               EVAL      MFCVV2 = '20'
     C****               ELSE
     C****               EVAL      MFCVV2 = sv_MFCVV2
     C****               ENDIF                                                  sv_MFCVV2=*Blanks
     C****               IF        sv_MFTRK1 = *Blanks
     C                   EVAL      MFMETH = '01'                                Manual keyed
     C****               ELSE
     C****               IF        MFMRCH = '00001'  or                         Live
     C****                            MFMRCH = '99999'                          Simulator
     C****               EVAL      MFMETH = '06'                                Swiped tracks 1 & 2
     C****               EVAL      MFMETH = '03'                                Swiped track 1
     C****               ELSE
     C****               EVAL      MFMETH = '03'                                Swiped track 1
     C****               ENDIF                                                  MFMRCH='00001' or
     C****               ENDIF                                                  sv_MFTRK1=*Blanks
     C****               EVAL      MFREFR = sv_MFREFR
     C                   EVAL      MFREFR = %editc(wk_Order#:'X') + '000'

      *
      * Populate level II fields.
      *
     C                   EVAL      MFAMT2 = wk_TaxAmt
     C                   IF        wk_TaxAmt = *Zero
     C                   EVAL      MFLTXF = '2'
     C                   ELSE
     C                   EVAL      MFLTXF = '1'
     C                   ENDIF                                                  wk_TaxAmt=*Zero
     C                   EVAL      MFDSTZ = sv_MFDSTZ
     C****               EVAL      MFORDR = sv_MFORDR
     C                   EVAL      MFORDR = %triml(%editc(wk_Order#:'Z'))
     C                   EVAL      MFQACI = 'Y'

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Retrieve prior authorization data.                                     *
      **************************************************************************
     CSR   $RtvCardData  BEGSR

      *
      * Populate data structure.
      *
     C                   EVAL      MFUKEY = wk_UniqueKey
     C                   EVAL      MFTYPE = 'RP'                                Retrieve card data

      *
      * Send request.
      *
     C                   EXSR      C@SEND

      *
      * Receive response.
      *
     C                   EXSR      C@RECV

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Local void prior authorization.                                        *
      **************************************************************************
     CSR   $LocalVoid    BEGSR

      *
      * Populate data structure.
      *
     C                   EVAL      MFUKEY = wk_UniqueKey
     C                   EVAL      MFTYPE = 'RL'                                Local void

      *
      * Send request.
      *
     C                   EXSR      C@SEND

      *
      * Receive response.
      *
     C                   EXSR      C@RECV

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Curbstone subroutines.                                                 *
      **************************************************************************
      /COPY C3/QRPGLESRC,C3RC0004

     P/EJECT
      **************************************************************************
      * Determine whether a field contains any numbers.                        *
      **************************************************************************
     P HasNumbers      B
     D                 PI             1N
     D Field                         26A   CONST

     D wk_Field        S             26A
     D wk_HasNumbers   S              1N
     D wk_ptr          S              3P 0

      /Free
       wk_Field = Field;
       wk_HasNumbers = *OFF;

       FOR wk_ptr = 1 to 26;
         IF %subst(wk_Field:wk_ptr:1) >= '0' and
            %subst(wk_Field:wk_ptr:1) <= '9';
           wk_HasNumbers = *ON;
           LEAVE;
         ENDIF;
       ENDFOR;                                                               // wk_ptr=1 to 26

      /End-Free

     C                   RETURN    wk_HasNumbers

     P HasNumbers      E

     P/EJECT
