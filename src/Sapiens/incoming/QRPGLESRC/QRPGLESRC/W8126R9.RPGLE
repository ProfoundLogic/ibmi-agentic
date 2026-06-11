    H* (C) Copyright Progressive Data Solutions, Inc., 2001 - 2003
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*
    H*****************************************************************
    H*    PROGRAM: W8126R9                                           *
    H*                                                               *
    H*    DESCRIPTION: CLARENDON DATA EXTRACT PROCESS                *
    H*                                                               *
    H*    CREATED BY:  C:53   10/12/01   PROJECT# WC970              *
    H*                                                               *
    H*    CALLED FROM:                                               *
    H*      W8126C - MAIN CL                                         *
    H*    CALLS TO:                                                  *
    H*      NONE                                                     *
    H*****************************************************************
      * Revised:                                                      *
      *                                                               *
      * 11/30/04 C:136  Support: WA121 44786                          *
      *                 Change code so that outstanding check file for*
      *                 a particular monthend is duplicatable         *
      *                 regardless of when the preprocessor is run.   *
      *                                                               *
      * 07/22/03  C:61  support wa099 32376                           *
      *                 Change to use Wxpgl.                          *
    H*****************************************************************
     H DATFMT(*USA)
      ******* beg chg ****** 07/22/03 *********************************
     F********wxpgp     if   e             disk
     Fwxpgl     if   e             disk
      ******* end chg ****** 07/22/03 *********************************
     Fwmgcl     if   e           k disk
     Fwdcrla    if   e           k disk
     Foutsp     o  a e             disk
     D xxnbr           s             20  2
     D Datusa          s               D   DATFMT(*USA)
     /eject
      *
     C     *Entry        Plist
     C                   Parm                    @@co#             3
     C                   Parm                    @@todt            8
      *
     C     wdcrp_key     klist
     C                   kfld                    pgco#
     C                   kfld                    pggrp
      *
     C     Wmgcl_Key     Klist
     C                   Kfld                    Pgco#
     C                   Kfld                    Pggrp
      *
     C     DTCYMD        Plist
     C                   Parm                    SRDTI             8 0
     C                   Parm      *BLANKS       SRFFMT            8
     C                   Parm      '*CYMD'       SRTFMT            8
     C                   Parm                    SRDTO            10
     C                   Parm                    SRERR             1
      *
      *
     C     *Like         Define    Crckdt        Kktodt
     C                   MOVEL     @@TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKTODT
    C/eject
     *  USE GROUP CONTROL FILE
     *
     *
      ******* beg chg ****** 07/22/03 *********************************
     C***********        Dou       %Eof(Wxpgp)
     C                   Dou       %Eof(Wxpgl)
      ******* end chg ****** 07/22/03 *********************************
     *
      ******* beg chg ****** 07/22/03 *********************************
     C**********         Read      Wxpgp
     C                   Read      Wxpgl
      ******* end chg ****** 07/22/03 *********************************
     *
     C                   If        %Eof
     C                   Leave
     c                   Endif
     *
     C                   If        Pgflag <> 'I'
     C                   Iter
     C                   Endif
     *
     C     Wmgcl_Key     Chain     Wmgcl
      *
     C                   If        not %Found
     C                   Eval      Gcpgid  = '888888'
     C                   Endif
     *
    C                   Exsr      Proc_Pmts
     *
     C                   Enddo
     *
     C                   move      *on           *inlr
      /Eject
     **********************************************************************
     * Process Payment File
     **********************************************************************
     *
    C     Proc_Pmts     Begsr
     *
     C     Wdcrp_Key     Setll     Pcr01
      *** beg chg ***** 11/30/04 *************************************
    C*****************  Dou       %Eof(Wdcrla)
    C*****Wdcrp_Key***  Reade     Wdcrla
    C*****************  If        %Eof
    C*****************  Leave
    C*****************  Endif
    C*****************  If        Crrtp  <> 'C' or
    C*****************            Cractp <> ' ' or
    C*****************            Crckdt  > Kktodt
    C*****************  Iter
    C*****************  Endif
     C     wdcrp_key     reade     pcr01
     C                   dow       not %eof
      *
     C                   if        CRRTP = 'C' and CRCKDT <= kktodt
      *
     C                   if        CRACTP = ' ' or
     C                             CRACTP = 'X' and CRACDT > kktodt
      *** end chg ***** 11/30/04 *************************************
     *
     C                   Clear                   Osp01
     *
     C                   movel(p)  gcpgid        sppgid
     C                   move      *zeros        spck#
     C                   move      crchk#        spck#
     C                   Eval      Spck## = Crchk#
    C                   Eval      xxnbr = crckam
     C                   Eval      spckam = %editc(xxnbr:'P')
     C                   Eval      Spckam# = Xxnbr
     *
     C                   clear                   spckdt
     C                   if        crckdt <> *zeros
     C     *CYMD         movel     crckdt        Datusa
     C     *USA          movel     Datusa        spckdt
     C                   endif
     *
     C                   Write     Osp01
     *
      *** beg add ***** 11/30/04 *************************************
     c                   endif                                                  CRRTP = 'C'
     c                   endif                                                  CRACTP = ' '
      *
     C     wdcrp_key     reade     pcr01
      *** end add ***** 11/30/04 *************************************
     C                   Enddo
     *
     C                   Endsr
     *
