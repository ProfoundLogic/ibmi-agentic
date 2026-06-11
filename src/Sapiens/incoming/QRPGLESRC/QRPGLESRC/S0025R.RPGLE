     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H/TITLE M0021R - Work with Environment Library List
    H*****************************************************************
    H*  REVISIONS:                                                   *
    H*                                                               *
    H*  06/02/23  C:300 Support: 111653                              *
    H*                  Do not show the prompts for the CEU report if*
    H*                  the client ID number is greater than 152     *
    H*                                                               *
    H*  11/22/13  C:190 Project: 129WSB                              *
    H*                  Add fields for policy year and Member        *
    H*                  experience report start date.                *
    H*                                                               *
    H*  06/29/09  C:191 Project: WC1806                              *
     H*                  Add the new fields for EFT flags             *
    H*                                                               *
    H*  03/18/08  C:190 Project: WC1638                              *
     H*                  Add the new field BUGCKS to SCR02.           *
    H*                                                               *
    H*  04/03/06  C:132 Project: WC1430                              *
    H*                  Add text on screen 2 "Generate daily         *
    H*                  Producer Performance Report", with edits.    *
    H*                                                               *
    H*  07/25/02  C:132 Project: WA093  Seq: 37882                   *
    H*                  Chg text on screen 2 'Member Profile Report" *
    H*                  to "Member Experience Report"                *
    H*                                                               *
    H*  11/22/02  C:142 Project: WC1100                              *
    H*                  Added screen 2                               *
    H*                                                               *
    H*  11/06/00  C:69  Project: WC1013                              *
    H*                  Rewrote program in RPG/IV. Put in logic for  *
    H*                  saving to save files.                        *
    H*                                                               *
    H*****************************************************************
     Fs0025f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     Fsmbup     uf a e             disk
     F*** BEGIN Add ****** 06/29/09 ************************************
     Fssylf     if   e           k disk
     F                                     ignore(ssy01)
     F                                     ignore(spc01)
     F********** needed ************       ******(swc01)
     F                                     ignore(sfb01)
     F                                     ignore(src01)
     F                                     ignore(sdr01)
     F                                     ignore(sbc01)
     F                                     ignore(scs01)
     F                                     ignore(sgl01)
     F                                     ignore(sp201)
     F*** END   Add ****** 06/29/09 ************************************
    D*
     D psds          esds                  extname(szq1p)
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D optnm           c                   const('Month-end Backup -
     D                                     Options')
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##data          s             80
     D ##err           s              1
     D ##leng          s              3  0
     D ##objn          s             10
     D ##objt          s              7
      ***** beg add ***** 11/22/13 ***********************************************
     D intFromDate     s              7  0
      ***** end add ***** 11/22/13 ***********************************************
      *** Begin Add ****** 06/02/23 ************************************
     d clientID        s              3    inz('000')
      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
      *** End   Add ****** 06/02/23 ************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*=====================================================================
     D* Procedure prototypes
     D*=====================================================================
     D/copy sprstring
     D/copy sprdate
     D
    C*
     C*** BEGIN Add ****** 06/29/09 ************************************
     C     syky          klist
     C                   kfld                    xxsys             2
     *
      * GET SYSTEM CONTROL FILE
      *
     C                   setoff                                       515253
     C                   eval      xxsys = 'WC'
     C     syky          chain     swc01
     C                   if        %found and wcfl17 = 'Y'
     C                   eval      *in51 = *on
     C                   endif
      *
     C                   if        %found and wcfl35 = 'Y'
     C                   eval      *in52 = *on
     C                   endif
      *
     C                   if        %found and wcfl24 = 'Y'
     C                   eval      *in53 = *on
     C                   endif
     C*** END   Add ****** 06/29/09 ************************************
      *** Begin Add ****** 06/02/23 ************************************
      /Free
       visbCeuRpt = *Off;
       GetClientID(clientID);
       If clientID > '152';
          visbCeuRpt = *On;
       Endif;
      /End-Free
      *** End   Add ****** 06/02/23 ************************************
    C*
     C                   movel(p)  optnm         scconm
     C                   eval      scconm = centerFld(scconm)
    C*
     C     1             chain(n)  smbup                              99
     C                   if        *in99 = *off
     C                   exsr      mvdbsc
     C                   endif
    C/eject
B01  C                   dou       *inkc = *on
     C                             or *inkl = *on
     C                             or *in90 = *off
     C                   exfmt     scr01
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)
      *
B02  C                   if        *inkc = *off
     C                             and *inkl = *off
     C                   exsr      errck1
      *
B03  C                   if        *in90 = *off
      *
      ***** Begin Add *** 11/22/2002 ***************************************
     C                   if        scwcms = 'Y'
     C                   exsr      scrn02
      *
B02  C                   if        *inkc = *on
     C                   leave
     C                   endif
      *
B02  C                   if        *inkl = *on
B02  C                   eval      *inkl = *off
     C                   iter
     C                   endif
E03  C                   endif
      *
      ***** End   Add *** 11/22/2002 ***************************************
     C                   exsr      finish
E03  C                   endif
      *
E02  C                   endif
      *
E01  C                   enddo
    C*
     C                   eval      *inlr = *on
      /EJECT
    C*================================================================
    C*  ERRCK1 - Error checking for display
    C*================================================================
     C     errck1        begsr
    C*
     C                   if        *in90 = *off
     C                             and scsavf <> 'Y'
     C                             and scsavf <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in72 = *on
     C                   endif
    C*
     C                   if        *in90 = *off
     C                   if        scsavf = 'N'
     C                   if        scprmt <> 'Y'
     C                             and scprmt <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in74 = *on
     C                   endif
     C                   if        *in90 = *off
     C                   call      'SRCHKOBJ'
     C                   parm      scdevn        ##objn
     C                   parm      '*DEVD'       ##objt
     C     *in90         parm                    ##err
     C                   if        *in90 = *on
     C                   eval      *in71 = *on
     C                   endif
     C                   endif
     C                   else
     C                   call      'SRCHKOBJ'
     C                   parm      scsflb        ##objn
     C                   parm      '*LIB '       ##objt
     C     *in90         parm                    ##err
     C                   if        *in90 = *on
     C                   eval      *in73 = *on
     C                   endif
     C                   endif
     C                   endif
    C*
     C                   if        *in90 = *off
     C                             and scwcms <> 'Y'
     C                             and scwcms <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in89 = *on
     C                   endif
    C*
     C                   if        *in90 = *off
     C                             and scwcmc <> 'Y'
     C                             and scwcmc <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in88 = *on
     C                   endif
    C*
     C                   if        *in90 = *off
     C                             and scdart <> 'Y'
     C                             and scdart <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in87 = *on
     C                   endif
    C*
     C                   if        *in90 = *off
     C                             and scpcms <> 'Y'
     C                             and scpcms <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in86 = *on
     C                   endif
    C*
     C                   if        *in90 = *off
     C                             and scap <> 'Y'
     C                             and scap <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in85 = *on
     C                   endif
    C*
     C                   if        *in90 = *off
     C                             and scgl <> 'Y'
     C                             and scgl <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in84 = *on
     C                   endif
    C*
     C                   if        *in90 = *off
     C                             and scfil <> 'Y'
     C                             and scfil <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in83 = *on
     C                   endif
    C*
     C                   if        *in90 = *off
     C                             and scmfil <> 'Y'
     C                             and scmfil <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in82 = *on
     C                   endif
    C*
     C                   if        *in90 = *off
     C                             and scqte <> 'Y'
     C                             and scqte <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in81 = *on
     C                   endif
    C*
     C                   if        *in90 = *off
     C                             and scprtr <> 'Y'
     C                             and scprtr <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in80 = *on
     C                   endif
     C                   endsr
      /EJECT
      ***** Begin Add *** 11/22/2002 ***************************************
    C*================================================================
    C*  Scrn02 - Process screen 2 if WCMS is Yes
    C*================================================================
     C     scrn02        begsr
     *
B01  C                   dou       *inkc = *on
     C                             or *inkl = *on
     C                             or *in90 = *off
     C                   movel     bucrpt        scceurpt
     C                   movel     bumrpt        scmprpt
     C                   movel     bucque        scceuque
     C                   movel     bumque        scmpque
     *
      ***** BEG ADD ***** 04/03/06 ***********************************************
     C                   movel     buprpt        scpprpt
     C                   movel     bupque        scppque
      ***** END ADD ***** 04/03/06 ***********************************************
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C                   movel     buaeft        scaeft
     C                   movel     buceft        scceft
     C                   movel     bueeft        sceeft
     C*** END   ADD ****** 06/29/09 ************************************
      ***** beg add ***** 11/22/13 ***********************************************
      /free
       scPolYear  = BUfyri;
       scMbrExpDt = DtMDYY(BUmesd);
      /end-free
      ***** end Add ***** 11/22/13 ***********************************************
     C                   exfmt     scr02
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C                   movea     '00000'       *in(61)
     C*** END   ADD ****** 06/29/09 ************************************
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)
      ***** beg add ***** 11/22/13 ***********************************************
      /free
       erMbrExDt = *Off;
       msMbrExDt = ' ';
      /end-free
      ***** end add ***** 11/22/13 ***********************************************
     *
B02  C                   if        *inkc = *off
     C                             and *inkl = *off
     C                   exsr      errck2
     *
B03  C                   if        *in90 = *off
     C                   leave
E03  C                   endif
     *
E03  C                   endif
     *
E03  C                   enddo
     *
     C                   endsr
      /EJECT
    C*================================================================
    C*  Errck2 - Validate screen 02
    C*================================================================
     C     errck2        begsr
     *
     C                   do
     *
     C                   if        scceurpt <> 'Y' and
     C                             scceurpt <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in75 = *on
     C                   leave
     C                   endif
     C*
     C                   if        scceurpt = 'Y'
     C                   call      'S0025C'
     C                   parm                    flag              1
     C                   parm      scceuque      queue            10
     C                   if        flag = 'N'
     C                   eval      *in90 = *on
     C                   eval      *in76 = *on
     C                   leave
     C                   endif
     C                   endif
     *
     C                   if        scmprpt <> 'Y' and
     C                             scmprpt <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in77 = *on
     C                   leave
     C                   endif
     C*
     C                   if        scmprpt = 'Y'
     C                   call      'S0025C'
     C                   parm                    flag              1
     C                   parm      scmpque       queue            10
     C                   if        flag = 'N'
     C                   eval      *in90 = *on
     C                   eval      *in78 = *on
     C                   leave
     C                   endif
     C                   endif
     *
      ***** beg add ***** 11/22/13 ***********************************************
      /free
       If scMbrExpDt <> *zero and DtCheck(scMbrExpDt) = *zero;
             msMbrExDt  = 'WCR0500';
             erMbrExDt  = *on;
             *in90 = *on;
       Else;
          scMbrExpDt = DtCheck(scMbrExpDt);
       Endif;
      /end-free
      ***** end add ***** 11/22/13 ***********************************************
      ***** BEG ADD ***** 04/03/06 ***********************************************
     *
     C                   if        scpprpt <> 'Y' and
     C                             scpprpt <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in79 = *on
     C                   leave
     C                   endif
     C                   if        scpprpt = 'Y' AND scppque = *blanks
     C                   eval      *in90 = *on
     C                   eval      *in80 = *on
     C                   leave
     C                   endif
      *
     C                   if        scpprpt = 'Y'
     C                   call      'S0025C'
     C                   parm                    flag              1
     C                   parm      scppque       queue            10
     C                   if        flag = 'N'
     C                   eval      *in90 = *on
     C                   eval      *in80 = *on
     C                   leave
     C                   endif
     C                   endif
      ***** END ADD ***** 04/03/06 ***********************************************
      ***** BEG ADD ***** 03/18/08 ***********************************************
     C                   if        scgcks <> 'Y' and
     C                             scgcks <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in61 = *on
     C                   leave
     C                   endif
      ***** END ADD ***** 03/18/08 ***********************************************
    C*
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C                   if        *in90 = *off
     C                             and scaeft <> 'Y'
     C                             and scaeft <> ' '
     C                   eval      *in90 = *on
     C                   eval      *in62 = *on
     C                   endif
    C*
     C                   if        *in90 = *off
     C                             and scceft <> 'Y'
     C                             and scceft <> ' '
     C                   eval      *in90 = *on
     C                   eval      *in63 = *on
     C                   endif
    C*
     C                   if        *in90 = *off
     C                             and sceeft <> 'Y'
     C                             and sceeft <> ' '
     C                   eval      *in90 = *on
     C                   eval      *in64 = *on
     C                   endif
     C*** END   ADD ****** 06/29/09 ************************************
    C*
     *
E03  C                   enddo
     *
E03  C                   endsr
     *
      ***** End   Add *** 11/22/2002 ***************************************
      /EJECT
    C*================================================================
    C*  FINISH - Final processing for file update/display
    C*================================================================
     C     finish        begsr
     C     1             chain     smbup                              99
     C                   exsr      mvscdb
     C                   time                    dsdttm
     C                   movel     q1pgm         bupgm
     C                   movel     q1user        buuser
     C                   eval      budt = dtCYMD(dsdate)
     C                   movel     dstime        butime
     C                   if        *in99 = *off
     C                   update    pbu01
     C                   else
     C                   write     pbu01
     C                   endif
     C                   endsr
      /EJECT
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   movel     budevn        scdevn
     C                   movel     buwcms        scwcms
     C                   movel     buwcmc        scwcmc
     C                   movel     budart        scdart
     C                   movel     bupcms        scpcms
     C                   movel     buap          scap
     C                   movel     bugl          scgl
     C                   movel     bufil         scfil
     C                   movel     bumfil        scmfil
     C                   movel     buqte         scqte
     C                   movel     buprtr        scprtr
     C                   if        busavf = ' '
     C                   movel     'N'           scsavf
     C                   else
     C                   movel     busavf        scsavf
     C                   endif
     C                   movel     busflb        scsflb
     C                   if        buprmt = ' '
     C                   movel     'Y'           scprmt
     C                   else
     C                   movel     buprmt        scprmt
     C                   endif
     *
      ***** BEG ADD ***** 04/03/06 ***********************************************
     C                   movel     buprpt        scpprpt
     C                   movel     bupque        scppque
      ***** END ADD ***** 04/03/06 ***********************************************
      ***** BEG ADD ***** 03/18/08 ***********************************************
     C                   movel     bugcks        scgcks
      ***** END ADD ***** 03/18/08 ***********************************************
     *
     C                   endsr
      /EJECT
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   movel     scdevn        budevn
     C                   movel     scwcms        buwcms
     C                   movel     scwcmc        buwcmc
     C                   movel     scdart        budart
     C                   movel     scpcms        bupcms
     C                   movel     scap          buap
     C                   movel     scgl          bugl
     C                   movel     scfil         bufil
     C                   movel     scmfil        bumfil
     C                   movel     scqte         buqte
     C                   movel     scprtr        buprtr
     C                   movel     scsavf        busavf
     C                   movel     scsflb        busflb
     C                   movel     scprmt        buprmt
      ***** Begin Add *** 11/22/2002 ***************************************
     C                   movel     scceurpt      bucrpt
     C                   movel     scmprpt       bumrpt
     C                   movel     scceuque      bucque
     C                   movel     scmpque       bumque
      ***** End   Add *** 11/22/2002 ***************************************
     *
      ***** BEG ADD ***** 04/03/06 ***********************************************
     C                   movel     scpprpt       buprpt
     C                   movel     scppque       bupque
      ***** END ADD ***** 04/03/06 ***********************************************
      ***** BEG ADD ***** 03/18/08 ***********************************************
     C                   movel     scgcks        bugcks
      ***** END ADD ***** 03/18/08 ***********************************************
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C                   movel     scaeft        buaeft
     C                   movel     scceft        buceft
     C                   movel     sceeft        bueeft
     C*** END   ADD ****** 06/29/09 ************************************
      ***** beg add ***** 11/22/13 ***********************************************
      /free
       BUfyri = scPolYear;
       BUmesd = DtCYMD(scMbrExpDt);
      /end-free
      ***** end Add ***** 11/22/13 ***********************************************
     *
     C                   endsr
      /EJECT
