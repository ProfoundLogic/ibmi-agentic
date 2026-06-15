     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2009')
      **************************************************************************
      * APPLICATION: Order Entry / Shipping / Invoicing                        *
      *       TITLE: Subprocedures - Orders                                    *
      *          BY: William Earl (MCA)                                        *
      *        DATE: January 2010                                              *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  XX/XX/XXXX  XXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *
      *                                                                        *
      **************************************************************************

     Hnomain

     FOEORHH    IF   E           K DISK    USROPN
     FOEIVHH02  IF   E           K DISK    USROPN

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY *LIBL/QPRPSRC,HYR9906

     P/EJECT
      **************************************************************************
      * Retrieve order invoiced total amount.                                  *
      **************************************************************************
     P RtvOrdInvc$     B                   EXPORT

     D                 PI            11P 2
     D Order#                         8P 0 CONST
     D CurInvcCycl                    1N   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Order#       S              8P 0
     D wk_Invc$        S             11P 2
     D wk_CurInvcCycl  S              1N

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_CurInvcCycl = CurInvcCycl
     C                   EVAL      wk_Invc$ = *Zero

      *
      * Retrieve completely finished invoice amounts.
      *
     C                   IF        not %open(OEORHH)
     C                   OPEN      OEORHH
     C                   ENDIF                                                  not %open(OEORHH)
     C     wk_Order#     SETLL     OEORHH
     C                   DOU       %eof(OEORHH)
     C     wk_Order#     READE     OEORHH
     C                   IF        %eof(OEORHH)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORHH)
     C                   IF        HHSEQ# = *Zero
     C                   ITER
     C                   ENDIF                                                  HHSEQ#=*Zero
     C                   EVAL      wk_Invc$ = wk_Invc$ + HHTIVA
     C                   ENDDO                                                  %eof(OEORHH)

      *
      * Retrieve invoice amounts currently in an invoice cycle.
      *
     C                   IF        wk_CurInvcCycl
     C                   IF        not %open(OEIVHH02)
     C                   OPEN      OEIVHH02
     C                   ENDIF                                                  not %open(OEIVHH02)
     C     wk_Order#     SETLL     OEIVHH02
     C                   DOU       %eof(OEIVHH02)
     C     wk_Order#     READE     OEIVHH02
     C                   ENDDO                                                  %eof(OEIVHH02)
     C                   IF        HISTAT = 'C'
     C                   ITER
     C                   ENDIF                                                  HISTAT='C'
     C                   EVAL      wk_Invc$ = wk_Invc$ + HIINVA
     C                   ENDIF                                                  wk_CurInvcCycl

     C                   ENDDO

      *
      * Return the order invoiced total amount.
      *
     C                   RETURN    wk_Invc$

     P RtvOrdInvc$     E

     P/EJECT
      **************************************************************************
      * Shut down service program                                              *
      **************************************************************************
     P sd_HYR9906      B                   EXPORT

     D                 PI            10I 0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Error        S             10I 0

     C                   EVAL      wk_Error = *Zero

     C                   DO

     C                   IF        %open(OEORHH)
     C                   CLOSE     OEORHH
     C                   ENDIF                                                  %open(OEORHH)

     C                   IF        %open(OEIVHH02)
     C                   CLOSE     OEIVHH02
     C                   ENDIF                                                  %open(OEIVHH02)

     C                   ENDDO

      *
      * Return the error code.
      *
     C                   RETURN    wk_Error

     P sd_HYR9906      E

     P/EJECT
