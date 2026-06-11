     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA019R1- Recurring Comp Duplicate Processing
     H*****************************************************************
     H*  Description:
     H*                                                               *
     H*    This pgm receives request for payment data and will        *
     H*    determine if this information is a duplicate
     H*
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W8082R02                                                   *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  Created: 10/21/16  C:309  Project:  C114WAT                  *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*
     H*****************************************************************
      *  Display File
     FWA019F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *

     Fwmcrl1    if   e           k disk    rename(pcr01:pcr01)
     F
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)

      *
     C     *LIKE         DEFINE    CRco#         @@CO#
     C     *LIKE         DEFINE    CRfnd         @@FND
     C     *LIKE         DEFINE    CRfyr         @@FYR
     C     *LIKE         DEFINE    CRcase        @@CASE
     C     *LIKE         DEFINE    CRseq#        @@SEQ#
     C     *LIKE         DEFINE    CRbgdt        @@FROM
     C     *LIKE         DEFINE    CRendt        @@TO
     C     *LIKE         DEFINE    CRamt         @@AMT
     C     *LIKE         DEFINE    CRrecd        @@RECD
     C     *LIKE         DEFINE    CRstat        @@RETP
     C     *LIKE         DEFINE    CRfein        @@FEIN
     C     *LIKE         DEFINE    CRmod         @@MOD

     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#                          Company #
     C                   PARM                    @@FND                          Fund
     C                   PARM                    @@FYR                          Fund Year
     C                   PARM                    @@CASE                         Case
     C                   PARM                    @@SEQ#                         RP Seq #
     C                   PARM                    @@FROM                         From Date
     C                   PARM                    @@TO                           To Date
     C                   PARM                    @@AMT                          Amount
     C                   PARM                    @@RECD                         Reserve Code
     C                   PARM                    @@FEIN                         FEIN #
     C                   PARM                    @@DFDD           51            Fund Name
     C                   PARM                    @@DCNM           36            Claimant Name
     C                   PARM                    @@DPNM           36            Provider Name
     C                   PARM                    I90               1            *IN90
     C                   PARM                    @@RETP                         Reserve Type
     C                   PARM                    @@MOD                          Modifier
     C                   PARM                    @@RTCD            1
      *
     C     Q1PRMS        IFGT      16
     C                   MOVE      @@RTCD        XXRTCD            1
     C                   ENDIF                                                  Q1PRMS > 16
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
      /Free
       // Check for Exact Duplicate
        Setll (@@Co#:@@Fnd:@@Fyr:@@Case) pcr01;
        Reade (@@Co#:@@Fnd:@@Fyr:@@Case) pcr01;
        Dow Not %Eof(wmcrl1);
           If @@ReCd = CRrecd and @@Fein = CRfein
            and @@From = CRbgdt and @@To = CRendt
            and @@Amt = CRamt;

              *In65 = *On  ;
              *In66 = *On  ;
              Exsr  CRASGN ;
              Exfmt WDWMSG ;
              *In90 = *On  ;
              I90   = *On  ;
              leave;
           EndIf;
           Reade (@@Co#:@@Fnd:@@Fyr:@@Case) pcr01;
        EndDo;

       // Check for Possible Duplicate
        If *In66 = *Off ;
           Setll (@@Co#:@@Fnd:@@Fyr:@@Case) pcr01;
           Reade (@@Co#:@@Fnd:@@Fyr:@@Case) pcr01;
           Dow Not %Eof(wmcrl1);
              If @@ReCd = CRrecd and @@Fein = CRfein;
                 If (@@From >= CRbgdt and @@From <= CRendt) or
                  (@@To   >= CRbgdt and @@To <= CRendt)     or
                  (@@From <= CRbgdt and @@To >= CRendt);

                    *In65 = *On  ;
                    *In67 = *On  ;
                    Exsr  CRASGN ;
                    Exfmt WDWMSG ;
                    *In90 = *On  ;
                    I90   = *On  ;
                    XXRTCD='P';
                    leave;
                 EndIf;
              EndIf;
              Reade (@@Co#:@@Fnd:@@Fyr:@@Case) pcr01;
           EndDo;
        EndIf;
      /End-Free

     C     Q1PRMS        IFGT      16
     C                   MOVE      XXRTCD        @@RTCD
     C                   ENDIF                                                  Q1PRMS > 16
     C                   SETON                                        LR
      *
      *================================================================
      *  CRASGN - Assign Fields in Duplicate Window using WMCRP Fields
      *================================================================
     C     CRASGN        BEGSR
      *
     C                   Z-ADD     CRFND         SCDFND
     C                   Z-ADD     CRFYR         SCDFYR
     C                   Z-ADD     CRCASE        SCDCAS
     C                   MOVEL     CRRECD        SCDRC
     C                   MOVEL     *Blanks       SCDTY
     C                   Z-ADD     CRFEIN        SCDFEI
     C                   Z-ADD     CRFEIN        SCDFID
     C                   Z-ADD     CRMOD         SCDMD
     C                   Z-ADD     CRBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDFDT
     C                   Z-ADD     CRENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTDT
     C                   Z-ADD     CRAMT         SCDAMT
      *
     C                   MOVE      *BLANK        SCDIND
     C     *IN66         IFEQ      *ON
     C                   MOVE      '*'           SCDIND
     C                   MOVE      '1'           XXRTCD
     C                   ENDIF
     C                   MOVE      *BLANK        SCDST

     C                   Z-ADD     CRLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDRDT

     C                   Z-ADD     *Zeros        SCDCHK
     C                   Z-ADD     *Zeros        SCDEOB
     C                   MOVEL     CRPNAM        SCDREQ
     C                   ENDSR
