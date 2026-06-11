      *****************************************************************
      * (C) Copyright Progressive Data Solutions, Inc., 1996 - 1999
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      *****************************************************************
      *TITLE W9761R - AGENT PREMIUM/LOSS BY STATE                     *
      *****************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      *   11/06/03  C: 132   PROJECT: WA093   SEQ: 39845              *
      *                      Correct bypass of Loss records           *
      *                      - Correct add of monthly loss additon    *
      *                        in Move_Flds2 subroutine               *
      *                                                               *
      *   10/06/03  C: 132   PROJECT: WA093   SEQ: 39036              *
      *                      Bypass output of records if do not match *
      *                      Group Number Parm                        *
      *                                                               *
      *   10/03/03  C: 132   PROJECT: WA093   SEQ: 39036              *
      *                      Bypass output of records Not in WDELP    *
      *                      File                                     *
      *                                                               *
      *   09/25/03  C: 132   PROJECT: WA093   SEQ: 39036              *
      *                      Add break on Input Fund year to  not     *
      *                      create zero agent records                *
      *                                                               *
      *****************************************************************
     FSmcol     IF   E           K Disk
     FWmupl     IF   E           K Disk
     FSmupl1    IF   E           K Disk
     F                                     Rename(Pup01:Pup99)
     FSmorl     IF   E           K Disk
     FQfeup     IF   E           K Disk
     FWxl2l1    UF A E           K Disk
     F                                     Rename(Pl201:Pl201a)
     FWxl2l2    UF A E           K Disk
     F                                     Rename(Pl201:Pl201b)
     FQfe2p     IF   E           K Disk
     FWdell     IF   E           K Disk
     FWmahl     IF   E           K Disk
     FWmaal1    IF   E           K Disk
     FWmagl     IF   E           K Disk
     FWmrsl     IF   E           K Disk
     FWmcml1    IF   E           K Disk
      **
     D QQfrdt          S              8
     D QQtodt          S              8
     D Datusa          S               D   DATFMT(*USA)
     D Frdate          S               D   DATFMT(*USA)
     D Todate          S               D   DATFMT(*USA)
     D Fromdt          S              7  0
     D Todtusa         S               D   DATFMT(*USA)
     D Cxdtusa         S               D   DATFMT(*USA)
     D Risk_St         S              2
      *
     D                 DS
     D  Asof                   1      6  0
     D  Asofmn                 1      2  0
     D  Asofyr                 5      6  0
      *
     D                 DS
     D  Run                    1      5  0
     D  Runyr                  2      3  0
     D  Runmth                 4      5  0
      *
     D                 DS
     D  Dsdttm                 1     12  0
     D  Dstime                 1      6  0
     D  Dsdate                 7     12  0
      *
     D PSDS          ESDS                  Extname(Szq1p)
      *
     C                   Eval      *Inlr = *Off
      /eject
      **********************************************************************
      * M A I N L I N E   R O U T I N E                                    *
      **********************************************************************
      *
     C                   Exsr      Proc_Qfeup
     C                   Exsr      Proc_Qfe2p
      *
     C                   Eval      *Inlr = *On
      *
      /eject
      **********************************************************************
      * Process the QfeUP file
      **********************************************************************
      *
     C     Proc_Qfeup    Begsr
      *
      *  Read calendar earned/unearned work file
      *
     C                   Dou       %Eof(Qfeup)
     C                   Read      Qfeup
      *
     C                   If        %Eof(Qfeup)
     C                   Leave
     C                   Endif
      *
      ***** BEG ADD ***** 09/25/03 ******************************************
      ***** BEG CHG ***** 11/06/03 ******************************************
     C                   If        eufnd <> xxfnd AND zzfnd = *zeros
     C*************      If        eufnd <> xxfnd
     C*************      If        zzfnd = *zeros
      ***** END CHG ***** 11/06/03 ******************************************
     C                   z-add     eufnd         xxfnd
     C                   endif
      *
     C                   If        zzfnd > *zeros AND eufnd <> zzfnd
      ***** BEG CHG ***** 10/06/03 ******************************************
     C********           iter
     C                   goto      PQfeEnd
      ***** END CHG ***** 10/06/03 ******************************************
     C                   endif
      ***** BEG DEL ***** 11/06/03 ******************************************
     c************       endif
      ***** END DEL ***** 11/06/03 ******************************************
      * If no agent #, bypass record
     C                   If        euagt# = *ZEROS
     C                   iter
     c                   endif
      ***** END ADD ***** 09/25/03 ******************************************
      *
     C                   Move      Eurcym        Run
     C                   If        Asofmn <> Runmth or
     C                             Asofyr <> Runyr
     C                   Iter
     C                   Endif
      *
     C                   If        QQsort = '1' or
     C                             QQsort = '2'
     C                   Exsr      Uef_Wxl2l1
     C                   Else
     C                   Exsr      Uef_Wxl2l2
     C                   Endif
      *
      ***** BEG CHG ***** 10/06/03 ******************************************
     C     PQfeEnd       tag
      ***** END CHG ***** 10/06/03 ******************************************
     C                   Enddo
      *
     C                   Endsr
      *
      /eject
      *****************************************************************
      *  Process Qfe2P                                                *
      *****************************************************************
      *
     C     Proc_Qfe2p    Begsr
      *
     C                   Dou       %Eof(Qfe2p)
     C                   Read      Qfe2p
      *
     C                   If        %Eof(Qfe2p)
     C                   Leave
     C                   Endif
      *
      ***** BEG ADD ***** 09/25/03 ******************************************
      * If FND iput parm is not zeros, then only process that fund
      ***** BEG CHG ***** 11/06/03 ******************************************
     C**************     If        e2fnd <> xxfnd
     C                   If        e2fnd <> xxfnd AND zzfnd = *zeros
     C                   z-add     e2fnd         xxfnd
     c                   endif
      ***** END CHG ***** 11/06/03 ******************************************
     C                   If        zzfnd > *zeros AND e2fnd <> zzfnd
      ***** BEG CHG ***** 10/06/03 ******************************************
     C********           iter
     C                   goto      Qfe2End
      ***** END CHG ***** 10/06/03 ******************************************
     c                   endif
      ***** BEG DEL ***** 11/06/03 ******************************************
     c**************     endif
      ***** END DEL ***** 11/06/03 ******************************************
      * If no agent #, bypass record
     C                   If        e2agt# = *ZEROS
     C                   iter
     c                   endif
      ***** END ADD ***** 09/25/03 ******************************************
      *
     C                   If        E2prt <> 'Y'
     C                   Iter
     C                   Endif
      *
     C                   Move      E2rcym        Run
     C                   If        Asofmn <> Runmth or
     C                             Asofyr <> Runyr
     C                   Iter
     C                   Endif
      *
     C     Wmahl_Key     Setgt     Wmahl
     C                   Dou       %Eof(Wmahl)
     C     Wmahl_Key     Readpe    Wmahl
     C                   If        %Eof(Wmahl)
     C                   Leave
     C                   Endif
     C                   If        Ahstat = 'A'
     C                   Leave
     C                   Endif
     C                   Enddo
      *
     C     Wdell_Key     Chain     Wdell
      *
      ***** BEG ADD ***** 10/03/03 ******************************************
     C                   If        %Found(Wdell)
      ***** END CHG ***** 10/03/03 ******************************************
   81C                   If        QQsort = '1' or
   81C                             QQsort = '2'
   81C                   Exsr      U2f_Wxl2l1
   81C                   Else
   81C                   Exsr      U2f_Wxl2l2
   81C                   Endif
   81 *
      ***** BEG ADD ***** 10/03/03 ******************************************
   81C                   Endif
   81 *
      ***** BEG ADD ***** 11/06/03 ******************************************
     C     Qfe2End       tag
      ***** END ADD ***** 11/06/03 ******************************************
   81 *
      ***** END ADD ***** 10/03/03 ******************************************
   81C                   Enddo
   81 *
      ***** BEG DEL ***** 11/06/03 ******************************************
     C*****Qfe2End       tag
      ***** END DEL ***** 11/06/03 ******************************************
     C                   Endsr
      *
      /eject
      *****************************************************************
      *  Update information in file WXL2P using logical WXL2L1        *
      *****************************************************************
      *
     C     Uef_Wxl2l1    Begsr
      *
     C     Wmaal1_Key    Chain     Wmaal1
     C                   If        Not %Found(Wmaal1)
     C                   Clear                   Paa01
      ***** BEG ADD ***** 10/03/03 ******************************************
     C                   goto      UEFend
      ***** END ADD ***** 10/03/03 ******************************************
     C                   Else
     C                   Movel     Aauwtr        L2uwtr
      *
     C     Smupl1_Key    Chain     Smupl1
     C                   If        %Found(Smupl1)
     C                   Movel     Upusnm        L2uwnm
     C                   Endif
     C                   Endif
      *
     C     Wxl2l1_Key    Chain     Wxl2l1
      *
     C                   If        %Found(Wxl2l1)
     C                   Exsr      Add_Flds
     C                   Update    Pl201a
     C                   Else
     C                   Exsr      Move_Flds
     C                   Write     Pl201a
     C                   Endif
      *
      ***** BEG ADD ***** 10/03/03 ******************************************
     C     UEFend        tag
      ***** END ADD ***** 10/03/03 ******************************************
     C                   Endsr
      *
      /eject
      *****************************************************************
      *  Update information in file WXL2P using logical WXL2L2        *
      *****************************************************************
      *
     C     Uef_Wxl2l2    Begsr
      *
     C     Wmaal1_Key    Chain     Wmaal1
     C                   If        Not %Found(Wmaal1)
     C                   Clear                   Paa01
     C                   Else
     C                   Movel     Aauwtr        L2uwtr
      *
     C     Smupl1_Key    Chain     Smupl1
     C                   If        %Found(Smupl1)
     C                   Movel     Upusnm        L2uwnm
     C                   Endif
     C                   Endif
      *
     C     Wmagl_Key     Chain     Wmagl
      *
     C                   If        Not %Found(Wmagl)
     C                   Clear                   Pag01
     C                   Else
     C                   Movel     Agmkrp        L2mrep
     C                   Movel     Agcont        L2mrnm
     C                   Endif
      *
     C     Wxl2l2_Key    Chain     Wxl2l2
      *
     C                   If        %Found(Wxl2l2)
     C                   Exsr      Add_Flds
     C                   Update    Pl201b
     C                   Else
     C                   Exsr      Move_Flds
     C                   Write     Pl201b
     C                   Endif
      *
     C                   Endsr
      *
      /eject
      *****************************************************************
      *  Update information in file WXL2P using logical WXL2L1        *
      *****************************************************************
      *
     C     U2f_Wxl2l1    Begsr
      *
     C     Wxl2l1_Key1   Chain     Wxl2l1
      *
     C                   If        %Found(Wxl2l1)
     C                   Exsr      Add_Flds2
     C                   Update    Pl201a
     C                   Else
     C                   Exsr      Move_Flds2
     C                   Write     Pl201a
     C                   Endif
      *
     C                   Endsr
      *
      /eject
      *****************************************************************
      *  Update information in file WXL2P using logical WXL2L2        *
      *****************************************************************
      *
     C     U2f_Wxl2l2    Begsr
      *
     C     Wxl2l2_Key1   Chain     Wxl2l2
      *
     C                   If        %Found(Wxl2l2)
     C                   Exsr      Add_Flds2
     C                   Update    Pl201b
     C                   Else
     C                   Exsr      Move_Flds2
     C                   Write     Pl201b
     C                   Endif
      *
     C                   Endsr
      *
      /eject
      *****************************************************************
      *  Add Qfeup fields to Wxl2P fields                             *
      *****************************************************************
      *
     C     Add_Flds      Begsr
      *
     C                   Eval      L2MDWR = Eumdwr + L2MDWR
     C                   Eval      L2MDEA = Eumdea + L2MDEA
     C                   Eval      L2MDIN = 0
     C                   Eval      L2MDLR = 0
     C                   Eval      L2YDWR = Euydwr + L2YDWR
     C                   Eval      L2YDEA = Euydea + L2YDEA
     C                   Eval      L2yDIN = 0
     C                   Eval      L2yDLR = 0
      *
     C                   If        Eunwrw = 'N'
     C                   Endif
      *
     C                   If        Eunwrw = 'R'
     C                   Eval      L2rnew = 1 + L2rnew
     C                   Endif
      *
     C                   Endsr
      *
      /eject
      *****************************************************************
      *  Move Qfeup fields to Wxl2P fields                            *
      *****************************************************************
      *
     C     Move_Flds     Begsr
      *
     C                   Clear                   Pl201a
     C                   Clear                   Pl201b
      *
     C     Wmaal1_Key    Chain     Wmaal1
     C                   If        Not %Found(Wmaal1)
     C                   Clear                   Paa01
     C                   Else
     C                   Movel     Aauwtr        L2uwtr
      *
     C     Smupl1_Key    Chain     Smupl1
     C                   If        %Found(Smupl1)
     C                   Movel     Upusnm        L2uwnm
     C                   Endif
     C                   Endif
      *
     C     Wmagl_Key     Chain     Wmagl
      *
     C                   If        Not %Found(Wmagl)
     C                   Clear                   Pag01
     C                   Else
     C                   Movel     Agmkrp        L2mrep
     C                   Movel     Agcont        L2mrnm
     C                   Endif
      *
     C                   If        QQsort = '1'
     C                   Movel     Euagnm        L2sort
     C                   Move      Euagt#        L2sort
     C                   Endif
      *
     C                   If        QQsort = '2'
     C                   Movel     Aauwtr        L2sort
     C                   Movel     Euagnm        L2srt2
     C                   Move      Euagt#        L2srt2
     C                   Endif
      *
     C                   If        QQsort = '3'
     C                   Movel     Agmkrp        L2sort
     C                   Movel     Euagnm        L2srt2
     C                   Move      Euagt#        L2srt2
     C                   Endif
      *
     C                   If        Eunwrw = 'N'
     C                   Endif
      *
     C                   If        Eunwrw = 'R'
     C                   Eval      L2rnew = 1 + L2rnew
     C                   Endif
      *
     C                   Eval      L2CO#  = EUCO#
     C                   Eval      L2AGT# = EUAGT#
     C                   Eval      L2AGNM = EUAGNM
     C                   Eval      L2ST   = EUJRST
     C                   Eval      L2RTCD = EURTCD
     C                   Eval      L2MDWR = EUMDWR
     C                   Eval      L2MDEA = EUMDEA
     C                   Eval      L2MDIN = 0
     C                   Eval      L2MDLR = 0
     C                   Eval      L2YDWR = EUYDWR
     C                   Eval      L2YDEA = EUYDEA
     C                   Eval      L2YDIN = 0
     C                   Eval      L2YDLR = 0
      *
     C                   Endsr
      *
      /eject
      *****************************************************************
      *  Add Qfe2p fields to Wxl2P fields                             *
      *****************************************************************
      *
     C     Add_Flds2     Begsr
      *
     C                   If        E2nwrw = 'R'
     C                   Eval      L2rnew = 1 + L2rnew
      *
     C                   If        E2cxdt <> *Zero
     C     *Usa          Move      E2cxdt        Cxdtusa
     C                   If        Cxdtusa <= Todtusa
     C                   Eval      L2cxrw = 1 + L2cxrw
     C                   Endif
      *
     C                   Endif
     C                   Endif
      *
     C                   Eval      L2mdin = E2acmp + L2mdin
     C                   Eval      L2mdin = E2amed + L2mdin
     C                   Eval      L2mdin = E2aleg + L2mdin
     C                   Eval      L2mdin = E2aoth + L2mdin
     C                   Eval      L2mdlr = 0
     C                   Eval      L2ydin = E2tcmp + L2ydin
     C                   Eval      L2ydin = E2tmed + L2ydin
     C                   Eval      L2ydin = E2tleg + L2ydin
     C                   Eval      L2ydin = E2toth + L2ydin
     C                   Eval      L2ydlr = 0
      *
     C                   Endsr
      *
      /eject
      *****************************************************************
      *  Move Qfe2p fields to Wxl2P fields                            *
      *****************************************************************
      *
     C     Move_Flds2    Begsr
      *
     C                   Clear                   Pl201a
     C                   Clear                   Pl201b
      *
     C     Wmaal1_Key2   Chain     Wmaal1
     C                   If        Not %Found(Wmaal1)
     C                   Clear                   Paa01
     C                   Else
     C                   Movel     Aauwtr        L2uwtr
      *
     C     Smupl1_Key    Chain     Smupl1
     C                   If        %Found(Smupl1)
     C                   Movel     Upusnm        L2uwnm
     C                   Endif
     C                   Endif
      *
     C     Wmagl_Key2    Chain     Wmagl
      *
     C                   If        Not %Found(Wmagl)
     C                   Clear                   Pag01
     C                   Else
     C                   Movel     Agmkrp        L2mrep
     C                   Movel     Agcont        L2mrnm
     C                   Endif
      *
     C                   If        QQsort = '1'
     C                   Movel     E2anam        L2sort
     C                   Move      E2agt#        L2sort
     C                   Endif
      *
     C                   If        QQsort = '2'
     C                   Movel     Aauwtr        L2sort
     C                   Movel     E2anam        L2srt2
     C                   Move      E2agt#        L2srt2
     C                   Endif
      *
     C                   If        QQsort = '3'
     C                   Movel     Agmkrp        L2sort
     C                   Movel     E2anam        L2srt2
     C                   Move      E2agt#        L2srt2
     C                   Endif
      *
     C                   If        E2nwrw = 'R'
     C                   Eval      L2rnew = 1 + L2rnew
      *
     C                   If        E2cxdt <> *Zero
     C     *Usa          Move      E2cxdt        Cxdtusa
      *
     C                   If        Cxdtusa <= Todtusa
     C                   Eval      L2cxrw = 1 + L2cxrw
     C                   Endif
      *
     C                   Endif
     C                   Endif
      *
     C                   Eval      L2CO#  = E2CO#
     C                   Eval      L2AGT# = E2AGT#
     C                   Eval      L2AGNM = E2anam
     C                   Eval      L2ST   = E2jrst
     C                   Eval      L2RTCD = Elrtcd
     C                   Eval      L2MDWR = 0
     C                   Eval      L2MDEA = 0
      ***** BEG CHG ***** 11/06/03 ******************************************
     C**************     Eval      L2mdin = E2acmp + E2tmed
     C                   Eval      L2mdin = E2acmp + E2amed
      ***** END CHG ***** 11/06/03 ******************************************
     C                   Eval      L2mdin = E2aleg + L2mdin
     C                   Eval      L2mdin = E2aoth + L2mdin
     C                   Eval      L2mdlr = 0
     C                   Eval      L2YDWR = 0
     C                   Eval      L2YDEA = 0
     C                   Eval      L2ydin = E2tcmp + E2tmed
     C                   Eval      L2ydin = E2tleg + L2ydin
     C                   Eval      L2ydin = E2toth + L2ydin
     C                   Eval      L2ydlr = 0
      *
     C                   Endsr
      *
      /eject
     **********************************************************************
     *  Get Risk State from Rate Set Code Master File
     **********************************************************************
      *
     C     Get_Risk_St   Begsr
      *
     C     Wmrsl_Key     Chain     Wmrsl
      *
     C                   If        %Found(Wmrsl)
     C                   Eval      Risk_St = Rsjrst
     C                   Else
     C                   Eval      Risk_St = *blank
     C                   Endif
      *
     C                   Endsr
      /eject
      *****************************************************************
      *  INITIALIZATION                ** SUBROUTINE **   S R 1 S T   *
      *****************************************************************
      *
     C     *Inzsr        Begsr
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     Smcol_Key     Klist
     C                   Kfld                    Xxco#
      *
      *   KEY LIST FOR THE USER FILE (FOR UNDERWRITER NAME)
      *
     C     Smupl1_Key    Klist
     C                   Kfld                    Aauwtr
      *
     C     Smupl1_Key1   Klist
     C                   Kfld                    Agmkrp
      *
      *   KEY LIST FOR THE USER PARAMETER FILE
      *
     C     Wmupl_Key     Klist
     C                   Kfld                    Q1user
      *
      *   KEY LIST FOR THE OVERRIDE FILE
      *
     C     Smorl_Key     Klist
     C                   Kfld                    Q1pgm
     C                   Kfld                    Xxprt             1
      *
      *  FUND FILE
      *
     C     Wmfnl_Key     Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Eufnd
      *
      *   KEY LIST FOR THE EMPLOYER FILE
      *
     C     Wmeml_Key     Klist
     C                   Kfld                    Euco#
     C                   Kfld                    Eufnd
     C                   Kfld                    Euemp#
     C                   Kfld                    Eudiv
      *
      *  Reserve History file
      *
     C     Wmrhl3_Key    Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Jan1dt
      *
     C     Wmrhl3_Key1   Klist
     C                   Kfld                    Xxco#
      *
     C     Wmrhl4_Key    Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Xxfnd
     C                   Kfld                    Jan1dt
      *
     C     Wmrhl4_Key1   Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Xxfnd
      *
     C     Wxl2l1_Key    Klist
     C                   Kfld                    Euco#
     C                   Kfld                    Euagt#
     C                   Kfld                    Aauwtr
     C                   Kfld                    Eujrst
     C                   Kfld                    Eurtcd
      *
     C     Wxl2l1_Key1   Klist
     C                   Kfld                    E2co#
     C                   Kfld                    E2agt#
     C                   Kfld                    Aauwtr
     C                   Kfld                    E2jrst
     C*********          Kfld                    E2rtcd
      *
     C     Wxl2l2_Key    Klist
     C                   Kfld                    Euco#
     C                   Kfld                    Euagt#
     C                   Kfld                    AAuwtr
     C                   Kfld                    Agmkrp
     C                   Kfld                    Eust
     C                   Kfld                    Eurtcd
      *
     C     Wxl2l2_Key1   Klist
     C                   Kfld                    Euco#
     C                   Kfld                    Euagt#
     C                   Kfld                    Aauwtr
     C                   Kfld                    Agmkrp
     C                   Kfld                    Eust
     C                   Kfld                    Eurtcd
      *
     C     Wmrsl_Key     Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Elrtcd
      *
      * Key list for agent history file
      *
     C     Wmahl_Key     Klist
     C                   Kfld                    E2co#
     C                   Kfld                    E2fnd
     C                   Kfld                    E2emp#
     C                   Kfld                    E2div
     C                   Kfld                    E2fyr
     *
     * Key For Agent file Wmaal1
      *
     C     Wmaal1_Key    Klist
     C                   Kfld                    Euco#
     C                   Kfld                    Eufnd
     C                   Kfld                    Euagt#
      *
     C     Wmaal1_Key2   Klist
     C                   Kfld                    E2co#
     C                   Kfld                    E2fnd
     C                   Kfld                    E2agt#
      *
      * Key For The Agent Master File
      *
     C     Wmagl_Key     Klist
     C                   Kfld                    Euco#
     C                   Kfld                    Aafein
     C                   Kfld                    Aamod
      *
     C     Wmagl_Key2    Klist
     C                   Kfld                    E2co#
     C                   Kfld                    Aafein
     C                   Kfld                    Aamod
      *
      * Key for Employer Detail file
      *
     C     Wdell_Key     Klist
     C                   Kfld                    E2co#
     C                   Kfld                    E2fnd
     C                   Kfld                    E2emp#
     C                   Kfld                    E2div
     C                   Kfld                    E2fyr
      *
     C     *Entry        Plist
     C                   Parm                    QQco#             3
     C                   Parm                    QQfnd             3
     C                   Parm                    QQfrdt            8
     C                   Parm                    QQtodt            8
     C                   Parm                    QQasof            6
     C                   Parm                    QQsort            1
      *
     C                   Time                    Dsdttm
      *
     C                   Move      QQasof        Asof
     C                   Move      QQco#         Xxco#             3 0
     C                   Move      QQfnd         Xxfnd             3 0
      ***** BEG ADD ***** 09/25/03 ******************************************
     C                   Move      QQfnd         Zzfnd             3 0
      ***** END ADD ***** 09/25/03 ******************************************
     C     *Usa0         Move      QQfrdt        Datusa
     C     *Ymd          Move      Datusa        Jan1dt            7 0
     C     *Usa0         Move      QQtodt        Todtusa
      *
     C     Wmupl_Key     Chain     Wmupl                              60
     C     Smcol_Key     Chain     Smcol                              60
      *
      * GET THE DISTRIBUTION INFORMATION
      *
     C                   Move      '0'           XXPRT
      *
     C     Smorl_Key     Chain     Smorl                              60
      *
     C                   If        *IN60 = *Off
     C                   Endif
      *
     C                   Move      *On           *INOF
      *
     C                   Endsr
