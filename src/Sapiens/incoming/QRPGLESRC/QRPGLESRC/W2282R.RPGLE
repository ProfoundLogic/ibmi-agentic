     Hcopyright('(c) Copyright Fiserv, Inc., 2005 - 2007')
    Hdftactgrp(*no)
     *****************************************************************
     *    PROGRAM: W2282R                                            *
     *                                                               *
     *    DESCRIPTION: W/W Audit assignment control                  *
     *                                                               *
     *    CREATED BY:  C:53   03/23/05   PROJECT# WC1196             *
     *                                                               *
     *****************************************************************
      * Revised:                                                      *
      *                                                               *
      *  06/04/07  C:33  Project: WC1196                              *
      *                  Changes from Q/A testing.                    *
      *                                                               *
      *  01/26/07  C:33  Project: WC1196                              *
      *                  Do not allow for activation when codes used  *
      *                   are not valid and active.                   *
      *                                                               *
      *  10/02/06 C:33  Project: WC1196                               *
      *                 Rewrite MVDBSC.                               *
      *                                                               *
      *  08/04/06 C:132 Project: WC1196 REWRITE to do following:      *
      *               - Rearrange the positions of lines on display.  *
      *               - Remove F9 option                              *
      *               - Add WTTAL, WTTGL to allow edits on Business   *
      *                 Category and Audit Type                       *
      *               - Add New Subset Fields with search.            *
      *                                                               *
     *****************************************************************
     Fw2282f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwtssp     if   e             disk
     F                                     rename(pss01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     F***  Begin Chg  ***  06/04/07  **************************************
     F**********wtssl     if   e           k disk
     Fwtssl1    if   e           k disk
     F***  End   Chg  ***  06/04/07  **************************************
     F                                     infds(dbfds)
     F                                     rename(pss01:dbrcdf)
    F*
     Fwttgp     if   e             disk
     F                                     rename(ptg01:ptg0p)
    F*
     Fwttgl     if   e           k disk
    F*
     Fwttap     if   e             disk
     F                                     rename(pta01:pta0p)
     Fwttal     if   e           k disk

     D***  Begin Add  ***  01/26/07  **************************************
     DValidAudTyp      pr             1n
     D code                                like(SCTYPA)
     DValidBusCat      pr             1n
     D code                                like(SCBCTG)
     D***  End   Add  ***  01/26/07  **************************************

    D*
     D hd              s             78    dim(2) ctdata perrcd(1)
     D of              s              1    dim(12) ctdata perrcd(1)
     D ofd             s             35    dim(12) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Audit Type Assignment Control'
     D slmsgi          c                   'WC39998'
     D deletd          c                   '*Deleted'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W2282R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D chgnum          c                   'SA020R'
     D infomx          c                   2
     D viewmx          c                   1
     D sf1pag          c                   11
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     D kkco#           s                   like(ssco#)
     D kkbctg          s                   like(ssbctg)
     D***  Begin Add  ***  06/04/07  **************************************
     D kknrbs          s                   like(ssnrbs)
     D kkprml          s                   like(ssprml)
     D***  End   Add  ***  06/04/07  **************************************
     D xxCall          s                   like(@@Call) inz
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D                 ds
     D xxdata                  1     70
     D   xxprml                1     11
     D   xxdash               13     13
     D   xxprmh               15     25
     D   xxbctg               33     35
     D***xxbcds********       11     30
     D   xxnrbs               45     45
     D   xxtypa               53     55
     D   xxstat               70     70
     D                 ds
     D x2data                  1     70
     D   x2bctg                3      5
     D   x2nrbs               14     14
     D   x2typa               19     21
     D   x2typd               23     42
     D   x2prml               44     54
     D   x2dash               56     56
     D   x2prmh               58     68
     D   x2stat               70     70
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@rrn
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
    C*
     C     acdpl1        plist
     C                   parm      '2'           ##mode
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      *zeros        ##rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkbctg
     C*
     C***  Begin Add  ***  06/04/07  **************************************
     C     key1a         klist
     C                   kfld                    kkco#
     C                   kfld                    kkbctg
     C                   kfld                    kknrbs
     C                   kfld                    kkprml
     C***  End   Add  ***  06/04/07  **************************************
     C*
     C     taky          klist
     C                   kfld                    kkco#
     C                   kfld                    sstypa
     C*
     C     taky1         klist
     C                   kfld                    kkco#
     C                   kfld                    sssta5
     C*
     C     tgky          klist
     C                   kfld                    kkco#
     C                   kfld                    ssbctg
     C*
     C     tgky1         klist
     C                   kfld                    kkco#
     C                   kfld                    sssta3
     C*
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        sssta1 <> *zeros  OR sssta3 <> *blanks OR
     C                             sssta4 <> *blanks OR sssta5 <> *blanks
     C                   eval      *in11 = *on
     C                   endif
     C*
     C                   endsr
     C*
     C***  Begin Add  ***  01/26/07  **************************************
     C*
    C*================================================================
    C* ErrChkOpt20 - Error checking for Opt 20
    C*================================================================
     C     ErrChkOpt20   begsr
     C*
     C  N90              if        not ValidAudTyp(SCTYPA) and SCSTAT = 'I'
     C                   seton                                        71  90
     C                   endif
     C*
     C  N90              if        not ValidBusCat(SCBCTG) and SCSTAT = 'I'
     C                   seton                                        72  90
     C                   endif
     C*
     C                   endsr
     C***  End   Add  ***  01/26/07  **************************************
     C*
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C*
     C***  Begin Add  ***  01/26/07  **************************************
     C     SCSEL         caseq     '20'          ErrChkOpt20
     C                   endcs
     C***  End   Add  ***  01/26/07  **************************************
     C*
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C*
     C* Check for Subset searches
     C                   EXSR      OTHFKS
     C*
     C*  Check Premium Low Limit
     C                   if        sssta1 <  *zeros
     C                   seton                                        81  90
     C                   endif
     C*
     C*  Chain to WTTGL with subset fields for Business Category
     C*
     C                   if        sssta3 <> *blanks AND *in90 = *off
     C     tgky1         chain     ptg01                              60
     C                   if        *in60
     C                   seton                                        83  90
     C                   endif
     C                   endif
     C*
     C*  Check on New/renewal ubset field
     C*
     C                   if        sssta4 <> *blanks AND *in90 = *off AND
     C                             sssta4 <> 'N' AND sssta4 <> 'R'
     C                   seton                                        84  90
     C                   endif
     C*
     C*  Chain to WTTAL with subset field for Audit Type
     C*
     C                   if        sssta5 <> *blanks AND *in90 = *off
     C     taky1         chain     pta01                              60
     C                   if        *in60
     C                   seton                                        85  90
     C                   endif
     C                   if        *in60 = *off AND tastat <> 'A'
     C                   seton                                        86  90
     C                   endif
     C                   endif
     C*
     C                   if        not *in90 and
     C                             s1Stat <> *blank and
     C                             s1Stat <> 'A'    and
     C                             s1Stat <> 'I'
     C                   seton                                        89  90
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C*
     C                   select
     C                   when      xxCall = '2'
     C                   eval      s1stat = 'A'
     C                   eval      sssta1 = *zeros
     C                   eval      sssta3 = *blanks
     C                   eval      sssta4 = *blanks
     C                   eval      sssta5 = *blanks
     C                   other
     C                   eval      s1Stat = *blank
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkco# = ##co#
     C                   move      *loval        kkbctg
     C***  Begin Add  ***  06/04/07  **************************************
     C                   eval      kknrbs = *loval
     C                   eval      kkprml = *loval
     C***  End   Add  ***  06/04/07  **************************************
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   eval      kkco# = ##co#
     C                   move      scpst1        kkbctg
     C***  Begin Add  ***  06/04/07  **************************************
     C                   eval      kknrbs = *loval
     C                   eval      kkprml = *loval
     C***  End   Add  ***  06/04/07  **************************************
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkco# = ##co#
     C                   eval      kkbctg= scbctg
     C***  Begin Add  ***  06/04/07  **************************************
     C                   eval      kknrbs = scnrbs
     C                   eval      kkprml = scprml
     C***  End   Add  ***  06/04/07  **************************************
     C*
     C                   endsr
     C*
    C*================================================================
    C* KJDO - Search Subroutine for F10
    C*================================================================
     C     KJDO          begsr
      *
     C                   select
     C     hfld          wheneq    'SSSTA3'
     C                   move      *blanks       SSSTA3
     C                   call      'W2280R'      acdpl1
     C     ##rrn         chain     ptg0p                              99
     C     *in99         ifeq      *on
     C                   eval      *in40 = *on
     C*
     C                   else
     C                   eval      *in41 = *on
     C                   eval      sssta3 = tgbctg
     C                   endif
     C*
     C     hfld          wheneq    'SSSTA5'
     C                   move      *blanks       sssta5
     C                   call      'W2281R'      acdpl1
     C     ##rrn         chain     pta0p                              99
     C     *in99         ifeq      *on
     C                   eval      *in43 = *on
     C*
     C                   else
     C                   eval      *in44 = *on
     C                   eval      sssta5 = tatypa
     C                   endif
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      SCBCTG = SSBCTG
     C***  Begin Add  ***  06/04/07  **************************************
     C                   eval      scnrbs = SSNRBS
     C                   eval      scprml = SSPRML
     C***  End   Add  ***  06/04/07  **************************************
     C***  Begin Add  ***  01/26/07  **************************************
     C                   eval      SCTYPA = SSTYPA
     C                   eval      SCSTAT = SSSTAT
     C*
     C                   eval      *in31 = *off
     C                   eval      *in32 = *off
     C***  End   Add  ***  01/26/07  **************************************
     C*
     C* Get desciption(s)
     C*
     C                   eval      TATYPD = *blanks
     C     taky          chain     pta01
     C*
     C                   eval      TGBCDS = *blanks
     C     tgky          chain     ptg01
     C*
     C                   select
     C                   when      info# = 1
     C                   eval      *in31 = *on
     C                   eval      SCDATA =         SSBCTG +
     C** **                                  '  ' + busCtgDesc +
     C                                       '  ' + %subst(TGBCDS:1:25) +
     C                                      '   ' + SSNRBS +
     C                                    '     ' + SSTYPA +
     C                                      '   ' + %editc(SSPRML:'J') +
     C                                       '- ' + %editc(SSPRMH:'J') +
     C                                        ' ' + SSSTAT
     C                   when      info# = 2
     C                   eval      *in32 = *on
     C                   eval      SCDATA =         SSBCTG +
     C                                     '    ' + SSNRBS +
     C                                     '    ' + SSTYPA +
     C                                       '  ' + %subst(TATYPD:1:27) +
     C** **                                  '  ' + audTypDesc +
     C                                        ' ' + %editc(SSPRML:'J') +
     C                                       '- ' + %editc(SSPRMH:'J') +
     C                                        ' ' + SSSTAT
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* Opt20 - Subroutine to change the status
    C*================================================================
     C     opt20         begsr
     C*
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                  99
     C*
     C                   endsr                                                  opt20
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C*
     C     *inkj         caseq     *on           kjdo                     90
     C                   ENDCS
     C*
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C*
     C     scsel         caseq     '20'          opt20
     C                   endcs
     C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C*
     C                   if        %parms >= 2
     C                   eval      @@kc = *inkc
     C                   endif
     C*
     C                   if        %parms >= 3
     C                   eval      @@kl = *inkl
     C                   endif
     C*
     C                   eval      sssta1 = *zeros
     C                   eval      sssta3 = *blanks
     C                   eval      sssta4 = *blanks
     C                   eval      sssta5 = *blanks
     C*
     C                   EVAL      *INLR = *ON
     C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   if        %parms >= 1
     C                   eval      xxCall = @@Call
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C     kkco#         reade     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     kkco#         readpe    dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C***  Begin Chg  ***  06/04/07  **************************************
     C*****key1*         setgt     dbrcdf                             96
     C     key1a         setgt     dbrcdf                             96
     C***  End   Chg  ***  06/04/07  **************************************
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C***  Begin Chg  ***  06/04/07  **************************************
     C*****key1*         setll     dbrcdf                             96
     C     key1a         setll     dbrcdf                             96
     C***  End   Chg  ***  06/04/07  **************************************
     C                   endif
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C*
     C     ##kl          ifeq      *off
     C     ##kc          andeq     *off
     C                   movel     deletd        scdata
     C                   endif
     C*
     C     ##kl          ifeq      *on
     C     ##kc          andeq     *on
     C     ##rrn         chain     pfrcdf                             99
     C                   if        *in99 = *off
     C                   exsr      mvdbsc
     C                   eval      scdbrr = ##rrn
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C*
     C                   z-add     scdbrr        @@rrn
     C*
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C* If premium below subset amount, do not select.
     C                   If        sssta1 > *zeros AND ssprml <  sssta1
     C                   eval      meetss = *off
     C                   endif
     C*
     C* If Business category does not match, do not select.
     C                   If        sssta3 > *blanks AND sssta3 <> ssbctg
     C                   eval      meetss = *off
     C                   endif
     C*
     C* If New/renew does not match, do not select.
     C                   If        sssta4 > *blanks AND sssta4 <> ssnrbs
     C                   eval      meetss = *off
     C                   endif
     C*
     C* If Audit Type does not match, do not select.
     C                   If        sssta5 > *blanks AND sssta5 <> sstypa
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   if        s1Stat <> *blanks and s1Stat <> SSSTAT
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   endsr





     P***  Begin Add  ***  01/26/07  **************************************
    P*================================================================
    P* ValidAudTyp - Check for a valid Audit Type
    P*================================================================
     PValidAudTyp      b
     DValidAudTyp      pi             1n
     D code                                like(SCTYPA)

     C     tableKey      klist
     C                   kfld                    kkCo#
     C                   kfld                    code

     C     tableKey      chain     pta01

     C                   if        not %found or TASTAT = 'I'
     C                   return    *off
     C                   endif

     C                   return    *on

     PValidAudTyp      e






    P*================================================================
    P* ValidBusCat - Check for a valid Business Category
    P*================================================================
     PValidBusCat      b
     DValidBusCat      pi             1n
     D code                                like(SCBCTG)

     C     tableKey      klist
     C                   kfld                    kkCo#
     C                   kfld                    code

     C     tableKey      chain     ptg01

     C                   if        not %found or TGSTAT = 'I'
     C                   return    *off
     C                   endif

     C                   return    *on

     PValidBusCat      e
     C***  End   Add  ***  01/26/07  **************************************





** Column headings
Opt  Ctgy Description               Renew  Type          Premium Range     Sts
Opt  Ctgy Renew Type Description                         Premium Range     Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
209F9=Change info           YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
