     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2010')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Write HYPEDWK Records and Copy to HYPELD                  *
      *          BY: William Earl (MCA)                                        *
      *        DATE: June 2010                                                 *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  03/06/2018  MCA  Increase field sizes from 5,2 to 6,2.                *
      *  11/08/2018  MCA  Fix bug - BldELD and ClrEDWK did not move the        *
      *                   in_Order# to wk_Order#.  This was not a problem with *
      *                   the old shipping program (HYR0110) because it had    *
      *                   previously (in the same invovation stack) called     *
      *                   this program.  It was a problem with the BoxLogix    *
      *                   programming.                                         *
      *                                                                        *
      **************************************************************************

     FHYPEDWK   UF A E           K DISK
     FOEORDT    IF   E           K DISK
     FHYPELD    O    E           K DISK    USROPN BLOCK(*NO)

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR0152         PR                  EXTPROC('HYR0152')
     D  Function                      1A   CONST
     D  Order#                        8P 0 CONST
     D  OrdLin#                       3P 0 CONST
     D  Blanket#                      3P 0 CONST
     D  BoxWgt                        6P 2 CONST
     D  HazMatNQ                      6P 2 CONST
     D  PowderWgt                     6P 2 CONST
     D  WgtUOM                        2A   CONST
     D  Serial#                       9P 0 CONST

      **************************************************************************
      * Define *Entry parameter list.                                          *
      **************************************************************************
     D HYR0152         PI
     D  in_Function                   1A   CONST
     D  in_Order#                     8P 0 CONST
     D  in_OrdLin#                    3P 0 CONST
     D  in_Blanket#                   3P 0 CONST
     D  in_BoxWgt                     6P 2 CONST
     D  in_HazMatNQ                   6P 2 CONST
     D  in_PowderWgt                  6P 2 CONST
     D  in_WgtUOM                     2A   CONST
     D  in_Serial#                    9P 0 CONST

      **************************************************************************
      * Define program status data structure.                                  *
      **************************************************************************
     D                SDS
     D ps_JobNumber          264    269

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Function     S                   LIKE(in_Function)
     D wk_Order#       S                   LIKE(EWORD#)
     D wk_OrdLin#      S                   LIKE(EWORL#)
     D wk_Blanket#     S                   LIKE(EWBLN#)
     D wk_BoxWgt       S                   LIKE(EWBWGT)
     D wk_HazMatNQ     S                   LIKE(EWHMNQ)
     D wk_PowderWgt    S                   LIKE(EWPWGT)
     D wk_WgtUOM       S                   LIKE(EWUOMW)
     D wk_Serial#      S                   LIKE(EWSRL#)

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D Yes             C                   'Y'
     D No              C                   'N'
     D WrtEDWK         C                   '1'
     D BldELD          C                   '2'
     D ClrEDWK         C                   '3'
     D ShutDown        C                   '9'

     C/EJECT

      **************************************************************************
      * Mainline.                                                              *
      **************************************************************************

     C                   EVAL      wk_Function = in_Function

     C                   SELECT
     C                   WHEN      wk_Function = WrtEDWK
     C                   EXSR      $WrtEDWK
     C                   WHEN      wk_Function = BldELD
     C                   EXSR      $BldELD
     C                   WHEN      wk_Function = ClrEDWK
     C                   EXSR      $ClrEDWK
     C                   WHEN      wk_Function = Shutdown
     C                   EVAL      *INLR = *ON
     C                   ENDSL

     C                   RETURN

     C/EJECT
      **************************************************************************
      * Write HYPEDWK record.                                                  *
      **************************************************************************
     CSR   $WrtEDWK      BEGSR

     C                   EVAL       wk_Order# = in_Order#
     C                   EVAL       wk_OrdLin# = in_OrdLin#
     C                   EVAL       wk_Blanket# = in_Blanket#
     C                   EVAL       wk_BoxWgt = in_BoxWgt
     C                   EVAL       wk_HazMatNQ = in_HazMatNQ
     C                   EVAL       wk_PowderWgt = in_PowderWgt
     C                   EVAL       wk_WgtUOM = in_WgtUOM
     C                   EVAL       wk_Serial# = in_Serial#

     C     key_OD        CHAIN     OEORDT
     C                   IF        not %found(OEORDT)
     C                   EVAL      ODITEM = *Blanks
     C                   EVAL      ODIMDS = *Blanks
     C                   ENDIF                                                  not %found(OEORDT)
     C                   EVAL      EWORD# = wk_Order#
     C                   EVAL      EWORL# = wk_OrdLin#
     C                   EVAL      EWBLN# = wk_Blanket#
     C                   EVAL      EWITEM = ODITEM
     C                   EVAL      EWIMDS = ODIMDS
     C                   EVAL      EWBWGT = wk_BoxWgt
     C                   EVAL      EWHMNQ = wk_HazMatNQ
     C                   EVAL      EWPWGT = wk_PowderWgt
     C                   EVAL      EWUOMW = wk_WgtUOM
     C                   EVAL      EWSRL# = wk_Serial#
     C                   EVAL      EWTSTP = %timestamp
     C                   EVAL      EWJOBN = ps_JobNumber

     C                   WRITE     HYREDWK

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Build HYPELD records from HYPEDWK records.                             *
      **************************************************************************
     CSR   $BldELD       BEGSR

     C                   EVAL       wk_Order# = in_Order#

     C                   IF        not %open(HYPELD)
     C                   OPEN      HYPELD
     C                   ENDIF                                                  not %open(HYPELD)

     C     wk_Order#     SETLL     HYPEDWK
     C                   DOU       %eof(HYPEDWK)
     C     wk_Order#     READE     HYPEDWK
     C                   IF        %eof(HYPEDWK)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPEDWK)
     C                   EXSR      $MoveEWtoED
     C                   WRITE     HYRELD
     C                   DELETE    HYREDWK
     C                   ENDDO                                                  %eof(HYPEDWK)

     C                   FEOD      HYPELD

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Clear HYPEDWK records for order.                                       *
      **************************************************************************
     CSR   $ClrEDWK      BEGSR

     C                   EVAL       wk_Order# = in_Order#

     C     wk_Order#     SETLL     HYPEDWK
     C                   DOU       %eof(HYPEDWK)
     C     wk_Order#     READE     HYPEDWK
     C                   IF        %eof(HYPEDWK)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPEDWK)
     C                   DELETE    HYREDWK
     C                   ENDDO                                                  %eof(HYPEDWK)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move HYPEDWK fields (EW) to HYPELD fields (ED).                        *
      **************************************************************************
     CSR   $MoveEWtoED   BEGSR

     C                   EVAL      EDORD# = EWORD#
     C                   EVAL      EDORL# = EWORL#
     C                   EVAL      EDBLN# = EWBLN#
     C                   EVAL      EDITEM = EWITEM
     C                   EVAL      EDIMDS = EWIMDS
     C                   EVAL      EDBWGT = EWBWGT
     C                   EVAL      EDHMNQ = EWHMNQ
     C                   EVAL      EDPWGT = EWPWGT
     C                   EVAL      EDUOMW = EWUOMW
     C                   EVAL      EDSRL# = EWSRL#
     C                   EVAL      EDTSTP = EWTSTP

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization.                                                *
      **************************************************************************
     CSR   *InzSR        BEGSR

     C     key_OD        KLIST                                                  OEORDT
     C                   KFLD                    wk_Order#
     C                   KFLD                    wk_OrdLin#
     C                   KFLD                    wk_Blanket#

     CSR                 ENDSR
     C/EJECT
