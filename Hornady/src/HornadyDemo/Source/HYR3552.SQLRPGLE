     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2024')
     H DFTACTGRP(*NO) ACTGRP('HDSOPT')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Pallet Banner Print - Automatic                           *
      *          BY: William Earl (MCA)                                        *
      *        DATE: July 2024                                                 *
      *                                                                        *
      *       NOTES: 1) This program will print pallet banners for all pallets *
      *                 associated with the passed BOL that have not already   *
      *                 been printed with the BOL's current PRO number.        *
      *              2) The pallet banners will be printed at the output queue *
      *                 defined in HYPWSOQ for outq type 'PLTBNRAUTO'.         *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  XX/XX/XXXX  XXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *
      **************************************************************************

     FHYLPLLT2  IF   E           K DISK
     FHYPWSOQ   IF   E           K DISK

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR3552         PR                  EXTPGM('HYR3552')
     D  BOL#                          8S 0 CONST
     D  Caller                       10A   CONST
     D  RtnCode                       2A

     D HYC3550         PR                  EXTPGM('HYC3550')                    Pallet banner print
     D   out_Pallet#                  9P 0 CONST
     D   out_BannerTy                 1A   CONST
     D   out_PrintTyp                 1A   CONST
     D   out_OutQ                    10A   CONST
     D   out_Drawer                   3P 0 CONST
     D   out_Copies                   3P 0 CONST
     D   out_HoldSplf                 4A   CONST
     D   out_Caller                  10A   CONST
     D   in_RtnCode                   2A

      **************************************************************************
      * Define *Entry parameter list.                                          *
      **************************************************************************
     D HYR3552         PI
     D  in_BOL#                       8S 0 CONST
     D  in_Caller                    10A   CONST
     D  out_RtnCode                   2A

      **************************************************************************
      * Define program status data structure.                                  *
      **************************************************************************
     D ps_            SDS
     D  ps_PgmName             1     10
     D  ps_JobName           244    253
     D  ps_UserID            254    263

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_BOL#         S                   LIKE(PMBOL#)
     D wk_Caller       S             10A
     D wk_RtnCode      S              2A
     D wk_SDBOL#       S              8S 0
     D wk_SDPRO#       S             30A
     D wk_BPPRO#       S             30A
     D wk_OutQ         S                   LIKE(WOOUTQ)
     D wk_Drawer       S              3P 0
     D wk_Copies       S              3P 0
     D wk_Wkstn        S                   LIKE(ps_JobName)
     D wk_Len          S              3P 0
     D wk_DCDftFnd     S              1N
     D wk_RcdCnt       S              7P 0
     D wk_RC3550       S              2A
     D kf_WSID         S                   LIKE(WOWSID)
     D kf_OQTY         S                   LIKE(WOOQTY) INZ('PLTBNRAUTO')

      **************************************************************************
      * Main routine.                                                          *
      **************************************************************************

     C                   EVAL      wk_BOL# = in_BOL#
     C                   EVAL      wk_Caller = in_Caller
     C                   EVAL      wk_RtnCode = *Zeros

     C                   DO

      /Free
       wk_SDBOL# = *Zero;
       wk_SDPRO# = *Blanks;
       EXEC SQL select SDBOL#, SDPRO#
                 into :wk_SDBOL#, :wk_SDPRO#
                from OEBOLH
                where SDBOL#=:wk_BOL#;
      /End-Free
     C                   IF        wk_SDBOL# = *Zero
     C                   EVAL      wk_RtnCode = '01'
     C                   LEAVE
     C                   ENDIF                                                  wk_SDBOL#=*Zero

     C                   EXSR      $RtvOutQ
     C                   SELECT
     C                   WHEN      wk_OutQ = 'PRT16'
     C                   EVAL      wk_Drawer = 5
     C                   OTHER
     C                   EVAL      wk_Drawer = 3
     C                   ENDSL
     C                   EVAL      wk_Copies = 1

     C     wk_BOL#       SETLL     HYLPLLT2
     C                   IF        not %equal(HYLPLLT2)
     C                   EVAL      wk_RtnCode = '02'
     C                   LEAVE
     C                   ENDIF                                                  %found(HYLPLLT2)

     C                   DOU       %eof(HYLPLLT2)
     C     wk_BOL#       READE     HYLPLLT2
     C                   IF        %eof(HYLPLLT2)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLPLLT2)

      /Free
       wk_BPPRO# = *Hival;
       EXEC SQL select BPPRO# into :wk_BPPRO#
                from HYPPBPS
                where BPPSRL#=:PMPSRL#
                order by BPSEQ# desc
                fetch first row only;
      /End-Free

      *
      * If a pallet banner with the current PRO number has already been
      * printed, skip it.
      *
     C                   IF        wk_BPPRO# = wk_SDPRO#
     C                   ITER
     C                   ENDIF                                                  wk_BPPRO#=wk_SDPRO#

     C                   CALLP(E)  HYC3550 (PMPSRL#:'1':'2':wk_OutQ:wk_Drawer:
     C                               wk_Copies:'*NO':wk_Caller:wk_RC3550)

     C                   ENDDO                                                  %eof(HYLPLLT2)

     C                   ENDDO

     C                   EVAL      out_RtnCode = wk_RtnCode

     C                   EVAL      *INLR = *ON
     C                   RETURN

     C/EJECT
      **************************************************************************
      * Retrieve output queue.                                                 *
      **************************************************************************
     CSR   $RtvOutQ      BEGSR

      *
      * Check for absolute workstation output queue assignment.
      *
     C                   EVAL      kf_WSID = ps_JobName
     C     key_WO        CHAIN     HYPWSOQ
     C                   IF        not %found(HYPWSOQ)

      *
      * Check for generic workstation output queue assignment.
      *
     C                   EVAL      wk_Wkstn = ps_JobName
     C                   EVAL      wk_Len = %checkr(' ':wk_Wkstn)
     C                   DOU       wk_Len = *Zero
     C                   EVAL      kf_WSID =
     C                               %triml(%subst(wk_Wkstn:1:wk_Len)) + '*'
     C     key_WO        CHAIN     HYPWSOQ
     C                   IF        %found(HYPWSOQ)
     C                   LEAVE
     C                   ENDIF                                                  %found(HYPWSOQ)
     C                   EVAL      wk_Len = wk_Len - 1
     C                   ENDDO                                                  wk_Len=*Zero

      *
      * Retrieve default output queue for the workstation's DC location.
      *
     C                   IF        wk_Len = *Zero
       /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from GUPTDAT
                where TDTABL='ALDAWKSTN'  and TDKEY1=:ps_JobName;
       /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      kf_WSID = '*WEST'
     C                   ELSE
     C                   EVAL      kf_WSID = '*ALDA'
     C                   ENDIF                                                  wk_RcdCnt= *Zero
     C     key_WO        CHAIN     HYPWSOQ
     C                   EVAL      wk_DCDftFnd = %found(HYPWSOQ)

      *
      * Retrieve default output queue.
      *
     C                   IF        not wk_DCDftFnd
     C                   EVAL      kf_WSID = '*DFT'
     C     key_WO        CHAIN     HYPWSOQ
     C                   IF        not %found(HYPWSOQ)
     C                   EVAL      WOOUTQ = '*JOB'
     C                   ENDIF                                                  not %found(HYPWSOQ)
     C                   ENDIF                                                  not wk_DCDftFnd

     C                   ENDIF                                                  wk_Len=*Zero

     C                   ENDIF                                                  not %found(HYPWSOQ)

     C                   EVAL      wk_OutQ = WOOUTQ

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization.                                                *
      **************************************************************************
     CSR   *InzSR        BEGSR

      *
      * Define  Key Lists
      *
     C     key_WO        KLIST                                                  HYPWSOQ
     C                   KFLD                    kf_WSID
     C                   KFLD                    kf_OQTY

     CSR                 ENDSR

     C/EJECT
