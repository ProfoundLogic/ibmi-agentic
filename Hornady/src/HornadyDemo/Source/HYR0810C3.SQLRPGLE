     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2017')
      **************************************************************************
      * APPLICATION: Credit Card Processing                                    *
      *       TITLE: CurbstoneCard C3 Data Retrieval                           *
      *          BY: William Earl (MCA)                                        *
      *        DATE: June 2017                                                 *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  03/15/2018  MCA  Partial authorizations.                              *
      *  04/10/2018  MCA  EMV device programming.                              *
      *  10/23/2019  MCA  Skip "RY" HYPCCRH records when retrieving            *
      *                   transaction data.  Tkt #4714                         *
      *  12/05/2020  MCA  Use local copy of Curbstone transactions for         *
      *                   subprocedure RtvOrdTranAmt, instead of retrieving    *
      *                   the data from the Curbstone server.  This change was *
      *                   needed because of Curbstone's monthly purge of       *
      *                   transactions older than 90 days old.  Without this   *
      *                   change, the pick ticket, shipping, and invoicing     *
      *                   programs were unaware of older-than-90-days prior    *
      *                   charges settled against orders, which caused them to *
      *                   duplicate the charges.  The Curbstone purge is new   *
      *                   within the past year, so this wasn't an issue until  *
      *                   recently.  Tkt #5854                                 *
      *                                                                        *
      **************************************************************************

     H nomain

     FHYPCCRH   IF   E           K DISK    USROPN

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY QPRPSRC,HYR0810C3

     D RtvTransRcd     PR
     D  UniqueKey                    15A   CONST
     D  Unmask                        1N   CONST

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID

      **************************************************************************
      * Curbstone definitions.                                                 *
      **************************************************************************
      /COPY C3/QRPGLESRC,C3RC0003

     P/EJECT
      **************************************************************************
      * Retrieve order payment card transaction amount.                        *
      **************************************************************************
     P RtvOrdTranAmt   B                   EXPORT

     D                 PI            11P 2
     D Order#                         8P 0 CONST
     D AmtType                       10A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Order#       S              8P 0
     D wk_AmtType      S             10A
     D wk_OrdAmt       S             11P 2

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_AmtType = AmtType
     C                   EVAL      wk_OrdAmt = *Zero

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
     C****               CALLP(E)  RtvTransRcd(RHUKEY:*OFF)
     C****               IF        not %error  and  MFUKEY = RHUKEY  and
     C****                           MFRTRN = 'UG'
      /Free
       CLEAR C#STRU;
       EXEC SQL select MFUKEY, MFTYPE, MFTYP2, MFRTRN,
                       MF$RAG, MF$FTS, MF$SLG, MF$LVD,
                       MFATAL, MFVALU, MFREFR, MFORDR,
                       MFAMT1, MFSETR, MFSETA
                 into :MFUKEY, :MFTYPE, :MFTYP2, :MFRTRN,
                      :MF$RAG, :MF$FTS, :MF$SLG, :MF$LVD,
                      :MFATAL, :MFVALU, :MFREFR, :MFORDR,
                      :MFAMT1, :MFSETR, :MFSETA
                from C3/C3FP9020
                where MFUKEY=:RHUKEY;
      /End-Free
     C                   IF        MFUKEY = RHUKEY  and
     C                               MFRTRN = 'UG'
     C                   IF        MFTYPE = 'RC'
     C                               or  MFTYPE = 'CC'
     C                   EVAL      MFSETA = MFSETA * -1
     C                   EVAL      MFSETR = MFSETR * -1
     C                   EVAL      MFAMT1 = MFAMT1 * -1
     C                   ENDIF                                                  MFTYPE='RC'
     C                   IF        MF$LVD = '0'
     C                   SELECT
     C                   WHEN      wk_AmtType = 'SETTLED'  and  MF$SLG = '1'
     C                   IF        MFSETA = *Zero
     C                   EVAL      wk_OrdAmt = wk_OrdAmt + MFSETR
     C                   ELSE
     C                   EVAL      wk_OrdAmt = wk_OrdAmt + MFSETA
     C                   ENDIF                                                  MFSETA=*Zero
     C                   WHEN      wk_AmtType = 'SETTLERQS'  and  MF$FTS = '1'
     C                               and  MF$SLG = '0'
     C                   EVAL      wk_OrdAmt = wk_OrdAmt + MFSETR
     C                   WHEN      wk_AmtType = 'AUTHONLY'  and  MF$RAG = '1'
     C                               and  MF$FTS = '0'
     C****               EVAL      wk_OrdAmt = wk_OrdAmt + MFAMT1
     C**** NOTE: MFVALU contains any amount due for partial authorizations.
     C                   EVAL      wk_OrdAmt = wk_OrdAmt + MFAMT1 - MFVALU
     C                   ENDSL
     C                   ELSE
     C                   IF        wk_AmtType = 'LOCALVOID'
     C                   EVAL      wk_OrdAmt = wk_OrdAmt + MFAMT1
     C                   ENDIF                                                  wk_AmtType='LOCALVOI
     C                   ENDIF                                                  MF$LVD='0'
     C****               ENDIF                                                  not %error and
     C                   ENDIF                                                  MHUKEY=RHUKEY and
     C                   ENDDO                                                  %eof(HYPCCRH)

     C                   ENDDO

      *
      * Return the order transaction amount.
      *
     C                   RETURN    wk_OrdAmt

     P RtvOrdTranAmt   E

     P/EJECT
      **************************************************************************
      * Retrieve payment card transaction amount.                              *
      **************************************************************************
     P RtvTranAmt      B                   EXPORT

     D                 PI            11P 2
     D UniqueKey                     15A   CONST
     D AmtType                       10A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_UniqueKey    S             15A
     D wk_AmtType      S             10A
     D wk_TranAmt      S             11P 2

     C                   DO

     C                   EVAL      wk_UniqueKey = UniqueKey
     C                   EVAL      wk_AmtType = AmtType
     C                   EVAL      wk_TranAmt = *Zero

     C                   CALLP(E)  RtvTransRcd(wk_UniqueKey:*OFF)
     C                   IF        not %error  and  MFUKEY = wk_UniqueKey  and
     C                               MFRTRN = 'UG'
     C                   IF        MFTYPE = 'RC'
     C                               or  MFTYPE = 'CC'
     C                   EVAL      MFSETA = MFSETA * -1
     C                   EVAL      MFSETR = MFSETR * -1
     C                   EVAL      MFAMT1 = MFAMT1 * -1
     C                   ENDIF                                                  MFTYPE='RC'
     C                   IF        MF$LVD = '0'
     C                   SELECT
     C                   WHEN      wk_AmtType = 'SETTLED'  and  MF$SLG = '1'
     C                   IF        MFSETA = *Zero
     C                   EVAL      wk_TranAmt = wk_TranAmt + MFSETR
     C                   ELSE
     C                   EVAL      wk_TranAmt = wk_TranAmt + MFSETA
     C                   ENDIF                                                  MFSETA=*Zero
     C                   WHEN      wk_AmtType = 'SETTLERQS'  and  MF$FTS = '1'
     C                               and  MF$SLG = '0'
     C                   EVAL      wk_TranAmt = wk_TranAmt + MFSETR
     C                   WHEN      wk_AmtType = 'AUTHONLY'  and  MF$RAG = '1'
     C                               and  MF$FTS = '0'
     C****               EVAL      wk_TranAmt = wk_TranAmt + MFAMT1
     C**** NOTE: MFVALU contains any amount due for partial authorizations.
     C                   EVAL      wk_TranAmt = wk_TranAmt + MFAMT1 - MFVALU
     C                   WHEN      wk_AmtType = 'TRANAMT'
     C                   EVAL      wk_TranAmt = wk_TranAmt + MFAMT1
     C                   ENDSL
     C                   ELSE
     C                   SELECT
     C                   WHEN      wk_AmtType = 'LOCALVOID'
     C                   EVAL      wk_TranAmt = wk_TranAmt + MFAMT1
     C                   WHEN      wk_AmtType = 'TRANAMT'
     C                   EVAL      wk_TranAmt = wk_TranAmt + MFAMT1
     C                   ENDSL
     C                   ENDIF                                                  MF$LVD='0'
     C                   ENDIF                                                  not %error and

     C                   ENDDO

      *
      * Return the transaction amount.
      *
     C                   RETURN    wk_TranAmt

     P RtvTranAmt      E

     P/EJECT
      **************************************************************************
      * Retrieve order payment card transaction date (yyyymmdd).               *
      **************************************************************************
     P RtvOrdTranDate  B                   EXPORT

     D                 PI             8A
     D Order#                         8P 0 CONST
     D DateType                      10A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Order#       S              8P 0
     D wk_DateType     S             10A
     D wk_Date         S              8A

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_DateType = DateType
     C                   EVAL      wk_Date = *Blanks

     C                   IF        wk_DateType = 'SETTLED1'  or
     C                               wk_DateType = 'AUTH1'
     C                   EVAL      wk_Date = *Hival
     C                   ENDIF                                                  wk_DateType='SETTLED

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
     C                   CALLP(E)  RtvTransRcd(RHUKEY:*OFF)
     C                   IF        not %error  and  MFUKEY = RHUKEY  and
     C                               MFRTRN = 'UG'
     C                   SELECT
     C                   WHEN      wk_DateType = 'SETTLED1'  and  MF$SLG = '1'  First settle
     C                   IF        MFSETD < wk_Date
     C                   EVAL      wk_Date = MFSETD
     C                   ENDIF                                                  MFSETD<wk_Date
     C                   WHEN      wk_DateType = 'SETTLED2'  and  MF$SLG = '1'  Last settle
     C                   IF        MFSETD > wk_Date
     C                   EVAL      wk_Date = MFSETD
     C                   ENDIF                                                  MFSETD>wk_Date
     C                   WHEN      wk_DateType = 'AUTH1'                        First auth
     C                   IF        MFDATE < wk_Date
     C                   EVAL      wk_Date = MFDATE
     C                   ENDIF                                                  MFDATE<wk_Date
     C                   WHEN      wk_DateType = 'AUTH2'                        Last auth
     C                   IF        MFDATE > wk_Date
     C                   EVAL      wk_Date = MFDATE
     C                   ENDIF                                                  MFDATE>wk_Date
     C                   ENDSL
     C                   ENDIF                                                  not %error and
     C                   ENDDO                                                  %eof(HYPCCRH)

     C                   ENDDO

     C                   IF        wk_Date = *Hival
     C                   EVAL      wk_Date = *Blanks
     C                   ENDIF                                                  wk_Date=*Hival

      *
      * Return the order transaction date.
      *
     C                   RETURN    wk_Date

     P RtvOrdTranDate  E

     P/EJECT
      **************************************************************************
      * Retrieve payment card transaction date (yyyymmdd).                     *
      **************************************************************************
     P RtvTranDate     B                   EXPORT

     D                 PI             8A
     D UniqueKey                     15A   CONST
     D DateType                      10A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_UniqueKey    S             15A
     D wk_DateType     S             10A
     D wk_Date         S              8A

     C                   DO

     C                   EVAL      wk_UniqueKey = UniqueKey
     C                   EVAL      wk_DateType = DateType
     C                   EVAL      wk_Date = *Blanks

     C                   CALLP(E)  RtvTransRcd(wk_UniqueKey:*OFF)
     C                   IF        not %error  and  MFUKEY = wk_UniqueKey  and
     C                               MFRTRN = 'UG'
     C                   SELECT
     C                   WHEN      wk_DateType = 'SETTLED'  and  MF$SLG = '1'   Settle
     C                   EVAL      wk_Date = MFSETD
     C                   WHEN      wk_DateType = 'AUTH'                         Auth
     C                   EVAL      wk_Date = MFDATE
     C                   ENDSL
     C                   ENDIF                                                  not %error and

     C                   ENDDO

      *
      * Return the transaction date.
      *
     C                   RETURN    wk_Date

     P RtvTranDate     E

     P/EJECT
      **************************************************************************
      * Retrieve payment card transaction data.                                *
      **************************************************************************
     P RtvTranData     B                   EXPORT

     D                 PI           256A   VARYING
     D UniqueKey                     15A   CONST
     D DataType                      10A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_UniqueKey    S             15A
     D wk_DataType     S             10A
     D wk_Data         S            256A   VARYING
     D wk_Unmask       S              1N

     C                   DO

     C                   EVAL      wk_UniqueKey = UniqueKey
     C                   EVAL      wk_DataType = DataType
     C                   EVAL      wk_Data = *Blanks

     C                   IF        wk_DataType = 'NAME'  or
     C                               wk_DataType = 'EXPIREDATE'
     C                   EVAL      wk_Unmask = *ON
     C                   ELSE
     C                   EVAL      wk_Unmask = *OFF
     C                   ENDIF                                                  wk_DataType='NAME'

     C                   CALLP(E)  RtvTransRcd(wk_UniqueKey:wk_Unmask)
     C                   IF        not %error  and  MFUKEY = wk_UniqueKey  and
     C                               MFRTRN = 'UG'
     C                   SELECT
     C                   WHEN      wk_DataType = 'TYPE'                         Transaction type
     C                   EVAL      %len(wk_Data) = 4
     C                   EVAL      wk_Data = MFTYPE + MFTYP2
     C                   WHEN      wk_DataType = 'CARDNBR'                      Card number
     C                   EVAL      %len(wk_Data) = 20
     C                   EVAL      wk_Data = MFCARD
     C                   WHEN      wk_DataType = 'EXPIREDATE'                   Expiration date
     C                   EVAL      %len(wk_Data) = 4
     C                   EVAL      wk_Data = MFEDAT
     C                   WHEN      wk_DataType = 'NAME'                         Cardholder name
     C                   EVAL      %len(wk_Data) = 40
     C                   EVAL      wk_Data = MFNAME
     C                   WHEN      wk_DataType = 'ADDRESS'                      Cardholder address
     C                   EVAL      %len(wk_Data) = 123
     C                   EVAL      wk_Data = MFADD1 + MFADD2 + MFCITY + MFSTAT +
     C                               MFZIPC
     C                   WHEN      wk_DataType = 'FLAGS'                        Flags
     C                   EVAL      %len(wk_Data) = 21
     C                   EVAL      wk_Data = MF$RAQ + MF$RAG + MF$RAD +
     C                                       MF$FTS + MF$SLQ + MF$SLG + MF$SLD +
     C                                       MF$FCQ + MF$FCG +MF$FCD +
     C                                       MF$RSD +
     C                                       MF$TRP + MF$TRR +
     C                                       MF$RVQ + MF$RVG + MF$RVD +
     C                                       MF$LVD +
     C                                       MF$RED + MF$RER +
     C                                       MF$IED + MF$IER
     C                   WHEN      wk_DataType = 'RTNVEND'                      Returned vendor
     C                   EVAL      %len(wk_Data) = 16
     C                   EVAL      wk_Data = MFRVND
     C                   WHEN      wk_DataType = 'RTNVENDABR'                   Returned vendor abbr
     C                   EVAL      %len(wk_Data) = 4
     C                   EVAL      wk_Data = MFRVNA
     C                   WHEN      wk_DataType = 'AUTHUSER'                     Auth user ID
     C                   EVAL      %len(wk_Data) = 10
     C                   EVAL      wk_Data = MFUSER
     C                   WHEN      wk_DataType = 'SETTLEUSER'                   Settle user ID
     C                   EVAL      %len(wk_Data) = 10
     C                   EVAL      wk_Data = MFSUSR
     C                   WHEN      wk_DataType = 'ENTRYMETH'                    Entry method
     C                   EVAL      %len(wk_Data) = 2
     C                   EVAL      wk_Data = MFMETH
     C                   WHEN      wk_DataType = 'REFERENCE'                    Reference
     C                   EVAL      %len(wk_Data) = 35
     C                   EVAL      wk_Data = MFREFR
     C                   WHEN      wk_DataType = 'ORDER'                        Order number
     C                   EVAL      %len(wk_Data) = 16
     C                   EVAL      wk_Data = MFORDR
     C                   WHEN      wk_DataType = 'TRANAMT'                      Transaction amount
     C                   EVAL      %len(wk_Data) = 15
     C                   EVAL      wk_Data = %editc(MFAMT1:'X')
     C                   WHEN      wk_DataType = 'TAXAMT'                       Tax amount
     C                   EVAL      %len(wk_Data) = 15
     C                   EVAL      wk_Data = %editc(MFAMT2:'X')
     C                   WHEN      wk_DataType = 'STLRQSAMT'                    Requested settle amt
     C                   EVAL      %len(wk_Data) = 15
     C                   EVAL      wk_Data = %editc(MFSETR:'X')
     C                   WHEN      wk_DataType = 'STLAMT'                       Settled amount
     C                   EVAL      %len(wk_Data) = 15
     C                   IF        MFSETA = *Zero  and  MF$SLG = '1'
     C                   EVAL      wk_Data = %editc(MFSETR:'X')
     C                   ELSE
     C                   EVAL      wk_Data = %editc(MFSETA:'X')
     C                   ENDIF                                                  MFSETA=*Zero
     C                   WHEN      wk_DataType = 'TAXFLAG'                      Local tax flag
     C                   EVAL      %len(wk_Data) = 1
     C                   EVAL      wk_Data = MFLTXF
     C                   WHEN      wk_DataType = 'DESTZIP'                      Destination zip code
     C                   EVAL      %len(wk_Data) = 9
     C                   EVAL      wk_Data = MFDSTZ
     C                   WHEN      wk_DataType = 'APPROVAL'                     Approval code
     C                   EVAL      %len(wk_Data) = 10
     C                   EVAL      wk_Data = MFAPPR
     C                   WHEN      wk_DataType = 'RSPTEXT'                      Response text
     C                   EVAL      %len(wk_Data) = 35
     C                   EVAL      wk_Data = MFRTXT
     C                   WHEN      wk_DataType = 'STLTEXT'                      Settle text
     C                   EVAL      %len(wk_Data) = 35
     C                   EVAL      wk_Data = MFSTXT
     C                   WHEN      wk_DataType = 'AVSRSP'                       AVS response
     C                   EVAL      %len(wk_Data) = 2
     C                   EVAL      wk_Data = MFRAVS
     C                   WHEN      wk_DataType = 'CVVRSP'                       CVV2 response
     C                   EVAL      %len(wk_Data) = 2
     C                   EVAL      wk_Data = MFRCVV
     C                   WHEN      wk_DataType = 'TRANDATE'                     Transaction date
     C                   EVAL      %len(wk_Data) = 8
     C                   EVAL      wk_Data = MFDATE
     C                   WHEN      wk_DataType = 'TRANTIME'                     Transaction time
     C                   EVAL      %len(wk_Data) = 6
     C                   EVAL      wk_Data = MFTIME
     C                   WHEN      wk_DataType = 'SHIPDATE'                     Shipped date
     C                   EVAL      %len(wk_Data) = 8
     C                   EVAL      wk_Data = MFSHPD
     C                   WHEN      wk_DataType = 'PROCDATE'                     Processed date
     C                   EVAL      %len(wk_Data) = 8
     C                   EVAL      wk_Data = MFPRCD
     C                   WHEN      wk_DataType = 'PROCTIME'                     Processed time
     C                   EVAL      %len(wk_Data) = 6
     C                   EVAL      wk_Data = MFPRCT
     C                   WHEN      wk_DataType = 'STLDATE'                      Settlement date
     C                   EVAL      %len(wk_Data) = 8
     C                   EVAL      wk_Data = MFSETD
     C                   WHEN      wk_DataType = 'STLTIME'                      Settlement time
     C                   EVAL      %len(wk_Data) = 6
     C                   EVAL      wk_Data = MFSETT
     C                   WHEN      wk_DataType = 'STLDATEN'                     Network settle date
     C                   EVAL      %len(wk_Data) = 4
     C                   EVAL      wk_Data = MFSTLD
     C                   ENDSL
     C                   ENDIF                                                  not %error and

     C                   ENDDO

      *
      * Return the transaction data.
      *
     C                   RETURN    wk_Data

     P RtvTranData     E

     P/EJECT
      **************************************************************************
      * Clear transaction fields.                                              *
      **************************************************************************
     P ClrTranFlds     B                   EXPORT

     C                   CLEAR                   C#STRU

      *
      * Return to calling procedure.
      *
     C                   RETURN

     P ClrTranFlds     E

     P/EJECT
      **************************************************************************
      * Retrieve CurbstoneCard transaction record.                             *
      **************************************************************************
     P RtvTransRcd     B

     D                 PI
     D UniqueKey                     15A   CONST
     D Unmask                         1N   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_UniqueKey    S             15A
     D wk_Unmask       S              1N

     C                   DO

     C                   EVAL      wk_UniqueKey = UniqueKey
     C                   EVAL      wk_Unmask = Unmask

      *
      * If the transaction is already in the data structure, exit the subprocedure.
      *
     C                   IF        wk_UniqueKey = MFUKEY  and  MFRTRN = 'UG'
     C                               and (not wk_Unmask  or  MFEDAT <> '****')
     C                   LEAVE
     C                   ENDIF                                                  wk_UniqueKey=MFUKEY

      *
      * Clear data structure.
      *
     C                   CLEAR                   C#STRU

      *
      * Populate data structure.
      *
     C                   EVAL      MFUKEY = wk_UniqueKey
     C                   IF        wk_Unmask
     C                   EVAL      MFTYPE = 'RQ'                                Retrieve card data
     C                   ELSE
     C                   EVAL      MFTYPE = 'RP'                                Retrieve card data
     C                   ENDIF                                                  wk_Unmask

      *
      * Send request.
      *
     C                   EXSR      C@SEND

      *
      * Receive response.
      *
     C                   EXSR      C@RECV

     C                   ENDDO

     C                   RETURN

     C/EJECT
      **************************************************************************
      * Curbstone subroutines.                                                 *
      **************************************************************************
      /COPY C3/QRPGLESRC,C3RC0004

     P RtvTransRcd     E

     P/EJECT
      **************************************************************************
      * Shut down service program                                              *
      **************************************************************************
     P sd_HYR0810      B                   EXPORT

     D                 PI            10I 0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Error        S             10I 0

     C                   EVAL      wk_Error = *Zero

     C                   DO

     C                   IF        %open(HYPCCRH)
     C                   CLOSE     HYPCCRH
     C                   ENDIF                                                  %open(HYPCCRH)

     C                   EXSR      C@LAST

     C                   ENDDO

      *
      * Return the error code.
      *
     C                   RETURN    wk_Error

     C/EJECT
      **************************************************************************
      * Curbstone subroutines.                                                 *
      **************************************************************************
      /COPY C3/QRPGLESRC,C3RC0004

     P sd_HYR0810      E

     P/EJECT
