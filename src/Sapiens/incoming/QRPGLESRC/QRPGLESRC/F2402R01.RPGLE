     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H option(*nodebugio:*srcstmt)
     H*dftactgrp(*no)****************
     H dftactgrp(*no) actgrp(*caller)
    H*
     H/TITLE F2402R01
     H**************************************************************************************
     H*  Description: FS Rate Table Maintenance                                            *
     H*                                                                                    *
     H*  Called from:                                                                      *
     H*  F2402R                                                                            *
     H*                                                                                    *
     H*  Calls to:                                                                         *
     H*  None                                                                              *
     H*                                                                                    *
     H*  Created: 12/09/09   Programmer C:198   Project:TX064                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  10/23/23 C:079  Project: FS661                                                    *
     H*                  Cosmetic Screen Adjustment - Recompile only                       *
     H*                                                                                    *
     H*  10/29/18 C:079  Project: FS535                                                    *
     H*                  Add fields RTUF1NEHR and RTUF2NEHR                                *
     H*                                                                                    *
     H*  10/19/18 C:079  Project: FS535                                                    *
     H*                  Rearrange fields                                                  *
     H*                                                                                    *
     H*  10/26/16 C:309  Project: FS490                                                    *
     H*                  Added new fields for "Operating rates for Not Meaningful EHR user"*
     H*                                                                                    *
     H*  04/04/16 C:212  Project: OK022                                                    *
     H*                  Add field SCPDIEM on SCR02 and related processing                  *
     H*                                                                                    *
     H*  09/16/15 C:212  Support: FS473                                                    *
     H*                  Add USH&L Fields on SCR02 and related processing                   *
     H*                                                                                    *
     H*  11/13/14 C:212  Support: FS448                                                    *
     H*                  Add Field "LIP Adj Power" on SCR02.                                *
     H*                                                                                    *
     H*  08/22/14 C:296  Support: 85317                                                    *
     H*                  Add Field "Short stay adjustment" on SCR02.                        *
     H*                                                                                    *
     H*  04/12/13 C:277  PROJECT: SY316                                                    *
     H*                  Recompile with *caller Activation group                           *
     H*                                                                                    *
     H*  11/19/12 C:277  PROJECT: FS407                                                    *
     H*                  Add Field Case mix adjustment on SCR02                            *
     H*                                                                                    *
     H*  07/26/12 C:277  PROJECT: TN016                                                    *
     H*                  Add Fields for state Tennessee on screen SCR02                    *
     H*                                                                                    *
     H*  10/17/11 C:251  PROJECT: 114MAR                                                   *
     H*                  Add Fields for IRF to Screen 2                                    *
     H*                                                                                    *
     H*  10/06/11 C:277  PROJECT: 114MZO                                                   *
     H*                  Add Field SNF Labor Percent to Screen 2                           *
     H*                                                                                    *
     H*  01/19/11 C:227  Project: TX067                                                    *
     H*                  Changed edit on Medicare Release level.                           *
     H*                                                                                    *
     H*  12/10/10 C:251  SUPPORT: 73333                                                    *
     H*                  Add State name to field                                           *
     H*                                                                                    *
     A*  10/01/10 C:198  Project: WV009                                                    *
     A*                  Add field - Release                                               *
     A*                                                                                    *
     A*  09/27/10 C:227  Project: MS019                                                    *
     A*                  Add additional field - Outlier Multiplier                         *
     A*                                                                                    *
     A*  07/28/10 C:248  SUPPORT: 73333                                                    *
     A*                  Add additional screen fileds                                      *
     A*                                                                                    *
     H*  02/16/10 C:220  Project: FS363                                                    *
     H*                  Add 2nd screen                                                    *
     H**************************************************************************************
     Ff2402f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fftrtp     uf a e             disk
     F                                     rename(prt01:dbrcdf)
     Fftrtl2    if   e           k disk
     F                                     rename(prt01:prt02)
     Fftgtl     if   e           k disk
     Fftgtp     if   e             disk    rename(pgt01:pgt02)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'FS rate table information'
     D comapi          c                   'FA007R'
     D #ofscr          c                   2
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     D kkst            s                   like(rtst)
     D kkefdt          s                   like(rtefdt)
     D kktype          s                   like(rttype)
     D kkkey           s                   like(gtkey)
     D kktbid          s                   like(gttbid)
     D kkmrel          s                   like(rtmrel)
    D*
    D*---------------------------------------------------------------------
    D*  Internal Data Structure
    D*---------------------------------------------------------------------
     D*
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@st              2
     C                   parm                    stname##         40
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     gtky          klist
     C                   kfld                    ##co#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
     C*
     C     rtky          klist
     C                   kfld                    kkst
     C                   kfld                    kktype
     C                   kfld                    kkefdt
     C                   kfld                    kkmrel
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C*** F10=Search on type
     C*
     C                   eval      *in72 = *off
     C                   eval      *in73 = *off
     C                   eval      *in75 = *off
     C                   eval      *in76 = *off
     C                   setoff                                       6667
     C*
     C*** On F6= Add, require type and effective date
     C                   if        sctype = *blanks
     C                   eval      *in72 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C*
     C                   if        scefdt = 0
     C                   eval      *in75 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C*
     C                   if        *in40 = *on
     C                   if        scmrel = ' '
     C                   eval      *in66 = *on
     C                   eval      *in67 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C                   endif
     C*** Invalid type and effective date
     C                   if        sctype <> *blanks
     C                   movel     'FSRT'        kktbid
     C                   movel     sctype        kkkey
     C*
     C     gtky          chain     pgt01                              99
     C                   if        *in99 = *on
     C                   eval      *in73 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C*
     C                   endif
     C*
     C                   if        scefdt <> *zeros
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C*
     C                   if        srerr = '1'
     C                   eval      *in76 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C*** duplicate key
     C                   if        *in40 = *on
     C*
     C                   eval      kkst   = scst
     C                   eval      kktype = sctype
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
     C                   eval      kkmrel = scmrel
     C                   eval      kktype = sctype
     C*
     C     rtky          chain     prt02                              99
     C                   if        *in99 = *off
     C                   eval      *in77 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C*
     C                   endif
     C                   endsr
    C*================================================================
    C* KJDO   - F10 on type on subset screen
    C*================================================================
     C     kjdo          begsr
     C                   select
     C                   when      hfld = 'SCTYPE'
     C                   call      'F2380R'
     C                   parm      '2'           ##call            1
     C                   parm      ' '           ##kc              1
     C                   parm      ' '           ##kl              1
     C                   parm      0             xxrrn             9 0
     C                   parm      'FSRT'        ##tbid            4
     C                   if        xxrrn <> 0
     C     xxrrn         chain     pgt02
     C                   movel     gtkey         sctype
     C                   seton                                        9074
     C                   else
     C                   seton                                        9071
     C                   endif
     C                   endsl
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   eval      sctype = rttype
     C                   movel     rtstat        scstat
     C*
     C                   z-add     rtefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C*
     C                   move      rtmrel        scmrel
     C                   eval      scneut = rtneut
     C                   eval      scopbr = rtopbr
     C                   eval      sccpbr = rtcpbr
     C                   eval      scoutl = rtoutl
     C                   eval      scmul1 = rtmul1
     C                   eval      scmul2 = rtmul2
     C                   eval      scmul3 = rtmul3
     C                   eval      scapc  = rtapc
     C                   eval      scopl1 = rtopl1
     C                   eval      scopn1 = rtopn1
     C                   eval      scopl2 = rtopl2
     C                   eval      scopn2 = rtopn2
     C                   eval      scopl3 = rtopl3
     C                   eval      scopn3 = rtopn3
     C                   eval      scopl4 = rtopl4
     C                   eval      scopn4 = rtopn4
     C                   eval      scotof = rtotof
     C                   eval      scoplb = rtoplb
     C                   eval      scoplw = rtoplw
     C                   eval      scopnl = rtopnl
     C                   eval      scopnw = rtopnw
     C                   eval      scurad = rturad
     C                   eval      scmgfa = rtmgfa
     C                   eval      scanes = rtanes
     C                   eval      scsurg = rtsurg
     C                   eval      scmedi = rtmedi
     C                   eval      scradi = rtradi
     C                   eval      scpath = rtpath
     C                   eval      scudfa = rtudfa
     C                   eval      scudf1 = rtudf1
     C*** Beg Add *** 10/29/18 ****************************************
     C                   eval      scuf1nehr = rtuf1nehr
     C                   eval      scuf2nehr = rtuf2nehr
     C*** End Add *** 10/29/18 ****************************************
     C                   eval      scapc1 = rtapc1
     C                   eval      scCONV = RTCONV
     C                   eval      scELSC = RTELSC
     C                   eval      scSNFL = RTSNFL
     C                   eval      scomul = rtomul
     C                   eval      scrccr = rtrccr
     C                   eval      scuccr = rtuccr
     C                   if        rtdesc <> *blanks
     C                   movel     rtdesc        scname#
     C                   endif
     C                   eval      scbncv = rtbncv
     C                   eval      scilpc = rtilpc
     C                   eval      scinpc = rtinpc
     C                   eval      scirad = rtirad
     C*** Beg Add *** 07/26/12 ****************************************
      /Free
          sconcf = rtoncf;
          scgscf = rtgscf;
          scrdcf = rtrdcf;
          sclbcf = rtlbcf;
          scmdcf = rtmdcf;
          scptcf = rtptcf;
          scemcf = rtemcf;
          scpocf = rtpocf;
          scdmcf = rtdmcf;
          scamcf = rtamcf;
          scopcf = rtopcf;
          scsncf = rtsncf;
          scchcf = rtchcf;

       //*** Beg Add *** 11/19/12 ********************************
          sccmix = RTcmix;
       //*** End Add *** 11/19/12 ********************************

       //*** Beg Add *** 08/22/14 ********************************
       //Populate screen field "short stay adjustment".
          scssAd = RTssAd;
       //*** End Add *** 08/22/14 ********************************

       //*** Beg Add *** 11/13/14 ********************************
       //Populate screen field "LIP Adj Power"
          scLipPow = RTLipPow;
       //*** End Add *** 11/13/14 ********************************

       //*** Beg Add *** 09/16/15 ********************************
       // Populate FTRTP USH&L fields
          scUsMult1  = RTUSMULT1;
          scUsMark1  = RTUSMARK1;
          scUsMark2  = RTUSMARK2;
          scUsLOSMk1 = RTUSLOSMK1;
          scUsLOSMk2 = RTUSLOSMK2;
       //*** End Add *** 09/16/15 ********************************

       //*** Beg Add *** 04/04/16 ********************************
       // Populate FTRTP Inpatient Per Diem
          scPDiem = RTPDiem;
       //*** End Add *** 04/04/16 ********************************

      /End-Free
     C*** End Add *** 07/26/12 ****************************************

     C*** Beg Add *** 10/26/16 ****************************************
      /Free
          scolne1  = RTolne1     ;
          sconlne1 = RTonlne1    ;
          scolne2  = RTolne2     ;
          sconlne2 = RTonlne2    ;
          scolne3  = RTolne3     ;
          sconlne3 = RTonlne3    ;
          scolne4  = RTolne4     ;
          sconlne4 = RTonlne4    ;
      /End-Free
     C*** End Add *** 10/26/16 ****************************************
       //*** Beg Add *** 06/21/18 **********************************
      /Free
          scMul4 = RTmul4;
      /End-Free
       //*** End Add *** 06/21/18 **********************************

     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      rtst = scst
     C                   movel     stname##      rtdesc
     C                   eval      rttype = sctype
     C                   movel     'A'           rtstat
     C*
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rtefdt
     C                   eval      rtmrel = scmrel
     C*
     C                   eval      rtneut = scneut
     C                   eval      rtopbr = scopbr
     C                   eval      rtcpbr = sccpbr
     C                   eval      rtoutl = scoutl
     C                   eval      rtmul1 = scmul1
     C                   eval      rtmul2 = scmul2
     C                   eval      rtmul3 = scmul3
     C                   eval      rtapc  = scapc
     C                   eval      rtopl1 = scopl1
     C                   eval      rtopn1 = scopn1
     C                   eval      rtopl2 = scopl2
     C                   eval      rtopn2 = scopn2
     C                   eval      rtopl3 = scopl3
     C                   eval      rtopn3 = scopn3
     C                   eval      rtopl4 = scopl4
     C                   eval      rtopn4 = scopn4
     C                   eval      rtotof = scotof
     C                   eval      rtoplb = scoplb
     C                   eval      rtoplw = scoplw
     C                   eval      rtopnl = scopnl
     C                   eval      rtopnw = scopnw
     C                   eval      rturad = scurad
     C                   eval      rtmgfa = scmgfa
     C                   eval      rtanes = scanes
     C                   eval      rtsurg = scsurg
     C                   eval      rtmedi = scmedi
     C                   eval      rtradi = scradi
     C                   eval      rtpath = scpath
     C                   eval      rtudfa = scudfa
     C                   eval      rtudf1 = scudf1
     C*** Beg Add *** 10/29/18 ****************************************
     C                   eval      rtuf1nehr = scuf1nehr
     C                   eval      rtuf2nehr = scuf2nehr
     C*** End Add *** 10/29/18 ****************************************
     C                   eval      rtapc1 = scapc1
     C                   eval      RTCONV = scCONV
     C                   eval      RTELSC = scELSC
     C                   eval      RTSNFL = scSNFL
     C                   eval      rtomul = scomul
     C                   eval      rtrccr = scrccr
     C                   eval      rtuccr = scuccr
     C                   eval      rtbncv = scbncv
     C                   eval      rtilpc = scilpc
     C                   eval      rtinpc = scinpc
     C                   eval      rtirad = scirad
     C*** Beg Add *** 07/26/12 ****************************************
      /Free
          rtoncf = sconcf;
          rtgscf = scgscf;
          rtrdcf = scrdcf;
          rtlbcf = sclbcf;
          rtmdcf = scmdcf;
          rtptcf = scptcf;
          rtemcf = scemcf;
          rtpocf = scpocf;
          rtdmcf = scdmcf;
          rtamcf = scamcf;
          rtopcf = scopcf;
          rtsncf = scsncf;
          rtchcf = scchcf;

       //*** Beg Add *** 11/19/12 ********************************
          RTcmix = sccmix;
       //*** End Add *** 11/19/12 ********************************

       //*** Beg Add *** 08/22/14 ********************************
       //Populate FTRTP field "short stay adjustment".
          RTssAd = scssAd;
       //*** End Add *** 08/22/14 ********************************

       //*** Beg Add *** 11/13/14 ********************************
       //Populate FTRTP field "LIP Adj Power"
          RTLipPow = scLipPow;
       //*** End Add *** 11/13/14 ********************************

       //*** Beg Add *** 09/16/15 ********************************
       // Populate FTRTP USH&L fields
          RTUSMULT1  = scUsMult1;
          RTUSMARK1  = scUsMark1;
          RTUSMARK2  = scUsMark2;
          RTUSLOSMK1 = scUsLOSMk1;
          RTUSLOSMK2 = scUsLOSMk2;
       //*** End Add *** 09/16/15 ********************************

       //*** Beg Add *** 04/04/16 ********************************
       // Populate FTRTP Inpatient Per Diem
          RTPDiem = scPDiem;
       //*** End Add *** 04/04/16 ********************************

      /End-Free
     C*** End Add *** 07/26/12 ****************************************

     C*** Beg Add *** 10/26/16 ****************************************
      /Free
          RTolne1  = scolne1    ;
          RTonlne1 = sconlne1   ;
          RTolne2  = scolne2    ;
          RTonlne2 = sconlne2   ;
          RTolne3  = scolne3    ;
          RTonlne3 = sconlne3   ;
          RTolne4  = scolne4    ;
          RTonlne4 = sconlne4   ;
      /End-Free
     C*** End Add *** 10/26/16 ****************************************
       //*** Beg Add *** 06/21/18 **********************************
      /Free
          RTmul4 = scmul4;
      /End-Free
       //*** End Add *** 06/21/18 **********************************

     C*** Maintenance fields
     C                   move      q1pgm         rtpgm
     C                   move      q1user        rtuser
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rtdt
     C                   move      dstime        rttime
     C                   move      q1pgm         rtipgm
     C                   move      q1user        rtiusr
     C                   movel     srdto         rtidt
     C                   move      dstime        rtitim
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C     *inkj         caseq     *on           kjdo
     C                   endcs
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C*
     C                   seton                                        lr
     C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   movel     @@st          scst
     C                   movel     stname##      scname#
     C                   if        @@mode = 'A'
     C                   seton                                        40
     C                   else
     C                   setoff                                       40
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   select
     C                   when      @@mode = 'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl
     C                   endsr
    C*================================================================
    C* CHGSTS - Change status
    C*================================================================
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C                   if        *in99 = *off
     C                   if        rtstat = 'I'
     C                   eval      rtstat = 'A'
     C                   else
     C                   eval      rtstat = 'I'
     C                   endif
     C                   movel     q1user        rtuser
     C                   movel     q1pgm         rtpgm
     C                   time                    dsdttm
     C                   movel     dstime        rttime
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rtdt
     C                   update    dbrcdf
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      ftrtp
     C                   endsr
