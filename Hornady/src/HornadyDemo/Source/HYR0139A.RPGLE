     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2019')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Pallet Report subprocedures                               *
      *          BY: William Earl (MCA)                                        *
      *        DATE: February 2019                                             *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  XX/XX/XXXX  XXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      **************************************************************************

     Hnomain

     FHYPPLTR   IF A E           K DISK    BLOCK(*NO) USROPN
     FHYLPLLT2  IF   E           K DISK    USROPN

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY QPRPSRC,HYR0139A

      **************************************************************************
      * Program status data structure.                                         *
      **************************************************************************
     D ps_            SDS
     D  ps_Job               244    253
     D  ps_User              254    263

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************

     P/EJECT
      **************************************************************************
      * Write pallet report record.                                            *
      **************************************************************************
     P WrtPRLog        B                   EXPORT
     D                 PI            10I 0
     D Pallet#                        9S 0 CONST
     D Function                      10A   CONST
      *** *PRINT1 - Printed by auto-prompt
      *** *PRINT2 - Printed manually
      *** *RESET  - Carton added-to or removed-from pallet after report printed
      *** *SKIP   - Auto-prompt skipped by user

     D wk_Pallet#      S                   LIKE(PRPSRL#)
     D wk_Function     S             10A
     D wk_Error        S             10I 0

     C                   DO

     C                   EVAL      wk_Pallet# = Pallet#
     C                   EVAL      wk_Function = Function
     C                   EVAL      wk_Error = *Zero

     C                   IF        not %open(HYPPLTR)
     C                   OPEN      HYPPLTR
     C                   ENDIF                                                  not %open(HYPPLTR)

     C                   EVAL      PRPSRL# = wk_Pallet#
     C                   EVAL      PRFUNC = wk_Function
     C                   EVAL      PRUSER = ps_User
     C                   EVAL      PRWS = ps_Job
     C                   EVAL      PRTSTP = %timestamp
     C                   WRITE     HYRPLTR

     C                   ENDDO

     C                   RETURN    wk_Error

     P WrtPRLog        E

     P/EJECT
      **************************************************************************
      * Retrieve pallet report status for a pallet.                            *
      **************************************************************************
     P RtvPRSts1       B                   EXPORT
     D                 PI            10A
     D Pallet#                        9S 0 CONST

     D wk_Pallet#      S                   LIKE(PRPSRL#)
     D wk_Status       S             10A
      *** *PRINT
      *** *NOTPRINT

     C                   DO

     C                   EVAL      wk_Pallet# = Pallet#
     C                   EVAL      wk_Status = *Blanks

     C                   IF        not %open(HYPPLTR)
     C                   OPEN      HYPPLTR
     C                   ENDIF                                                  not %open(HYPPLTR)

     C     wk_Pallet#    SETGT     HYPPLTR
     C     wk_Pallet#    READPE    HYPPLTR
     C                   IF        not %eof(HYPPLTR)  and
     C                              (PRFUNC = '*PRINT1'  or
     C                               PRFUNC = '*PRINT2')
     C                   EVAL      wk_Status = '*PRINT'
     C                   ELSE
     C                   EVAL      wk_Status = '*NOTPRINT'
     C                   ENDIF                                                  not %eof(HYPPLTR)

     C                   ENDDO

     C                   RETURN    wk_Status

     P RtvPRSts1       E

     P/EJECT
      **************************************************************************
      * Retrieve pallet report status for a shipment.                          *
      **************************************************************************
     P RtvPRSts2       B                   EXPORT
     D                 PI            10A
     D BOL#                           8S 0 CONST

     D wk_BOL#         S                   LIKE(PMBOL#)
     D wk_Status       S             10A
      *** *PRINT
      *** *NOTPRINT

     C                   DO

     C                   EVAL      wk_BOL# = BOL#
     C                   EVAL      wk_Status = *Blanks

     C                   IF        not %open(HYLPLLT2)
     C                   OPEN      HYLPLLT2
     C                   ENDIF                                                  not %open(HYLPLLT2)1

     C                   IF        not %open(HYPPLTR)
     C                   OPEN      HYPPLTR
     C                   ENDIF                                                  not %open(HYPPLTR)

     C     wk_BOL#       SETLL     HYLPLLT2
     C                   IF        not %equal(HYLPLLT2)
     C                   EVAL      wk_Status = '*NOTPRINT'
     C                   LEAVE
     C                   ENDIF                                                  not %equal(HYLPLLT2)

     C                   DOU       %eof(HYLPLLT2)
     C     wk_BOL#       READE     HYLPLLT2
     C                   IF        %eof(HYLPLLT2)
     C                   EVAL      wk_Status = '*PRINT'
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLPLLT2)

     C     PMPSRL#       SETGT     HYPPLTR
     C     PMPSRL#       READPE    HYPPLTR
     C                   IF        %eof(HYPPLTR)  or
     C                              (PRFUNC <> '*PRINT1'  and
     C                               PRFUNC <> '*PRINT2')
     C                   EVAL      wk_Status = '*NOTPRINT'
     C                   LEAVE
     C                   ENDIF                                                  not %eof(HYPPLTR)

     C                   ENDDO                                                  %eof(HYLPLLT2)

     C                   ENDDO

     C                   RETURN    wk_Status

     P RtvPRSts2       E

     P/EJECT
      **************************************************************************
      * Shut down service program                                              *
      **************************************************************************
     P sd_HYR0139A     B                   EXPORT

     D                 PI            10I 0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Error        S             10I 0

     C                   EVAL      wk_Error = *Zero

     C                   DO

     C                   IF        %open(HYPPLTR)
     C                   CLOSE     HYPPLTR
     C                   ENDIF                                                  %open(HYPPLTR)

     C                   IF        %open(HYLPLLT2)
     C                   CLOSE     HYLPLLT2
     C                   ENDIF                                                  %open(HYLPLLT2)

     C                   ENDDO

      *
      * Return the error code.
      *
     C                   RETURN    wk_Error

     P sd_HYR0139A     E

     P/EJECT
