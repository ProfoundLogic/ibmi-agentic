     Hcopyright('(c) Copyright Fiserv, Inc., 2006 - 2007')
     *****************************************************************
     *    PROGRAM: W2283R                                            *
     *                                                               *
     *    DESCRIPTION: W/W Audit company assignment                  *
     *                                                               *
     *    CREATED BY:  C:53   03/23/05   PROJECT# WC1196             *
     *                                                               *
     *****************************************************************
      * Revised:                                                      *
      *                                                               *
      *  03/13/09  C:65  Project: WC1196/support
      *                  Change valid state edit to use state table file.
      *                                                               *
      *  01/14/08  C:180 SUPPORT 61314
      *                  Require State entry when doing subset by county.
      *
      *  06/04/07  C:33  Project: WC1196                              *
      *                  Changes from Q/A testing.                    *
      *                                                               *
      *  10/02/06  C:33  Project: WC1196                              *
      *                 -Remove Zip Code from key                     *
      *                 -Rewrite MVDBSC.                              *
      *                                                               *
      *  08/04/06  C: 132 Project: WC1196                             *
      *                 - Drop Zip Code & Type of Business            *
      *                 - Put Premium Range at beginning of display   *
      *                   line, rearrange other display items         *
      *                 - Shorten subfile lines to display F commands *
      *                                                               *
     *****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw2283f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwtaap     if   e             disk
     F                                     rename(paa01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwtaal     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(paa01:dbrcdf)
     Fwttal     if   e           k disk
     Fwttap     if   e             disk    rename(pta01:pta0p)
     F*
    F* Audit Company Table file
     Fwtacl     if   e           k disk
     Fwtacp     if   e             disk    rename(pac01:pac0p)
     F*
    F* County Code table file
     Fwtcnl1    if   e           k disk
     Fwtcnp     if   e             disk    rename(pcn01:pcn0p)
     F*
    F* State Code table file
     F***  Begin Chg  ***  03/13/09  **************************************
     F**********wtgtl     if   e           k disk
     Fststl3    if   e           k disk    rename(pst01:pst03)
     F***  End   Chg  ***  03/13/09  **************************************
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D***  Begin Chg  ***  06/04/07  **************************************
     D*of*******       s              1    dim(11) ctdata perrcd(1)
     D*ofd******       s             35    dim(11) alt(of)
     D#ofOFDs          c                   15
     D of              s              1    dim(#ofOFDs) ctdata perrcd(1)
     D ofd             s             35    dim(#ofOFDs) alt(of)
     D***  End   Chg  ***  06/04/07  **************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D***  Begin Chg  ***  06/04/07  **************************************
     D*optnm****       c                   'Audit Company Assignment'
     D optnm           c                   'Audit Co. Assignment Control'
     D***  End   Chg  ***  06/04/07  **************************************
     D slmsgi          c                   'WC39998'
     D deletd          c                   '*Deleted'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W2283R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D chgnum          c                   'SA020R'
     D infomx          c                   1
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
     D ##call          s              1
     D kkco#           s                   like(aaco#)
     D kkaco           s                   like(aaaco)
     D kktypa          s                   like(aatypa)
     D kkst            s                   like(aast)
     D kkcnty          s                   like(aacnty)
     D kkzip           s                   like(aazip)
     D***  Begin Add  ***  10/02/06  **************************************
     D kkprml          s                   like(AAPRML)
     D***  End   Add  ***  10/02/06  **************************************
     D***  Begin Chg  ***  03/13/09  **************************************
     D*kktbid***       s                   like(gttbid)
     D*kkkey****       s                   like(gtkey)
     D xxKey           s                   like(STKEY)
     D***  End   Chg  ***  03/13/09  **************************************
     D kkcnt1          s                   like(cnkey)
     D ##num           s             80
     D ##mask          s             10
     D ##dec           s              3  0
     D ##thou          s              1
     D ##view          s              1  0
     D ##st            s              2
     D xxCall          s                   like(@@Call) inz
     D xxdash          s              1
     D xxzip1          s              5
     D xxzip2          s              4
     D xxtypb          s              1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D                 ds
     D xxdata                  1     70
     D   xxaco                59     61
     D   xxtypa               31     33
     D   xxst                 40     41
     D   xxcnty               49     50
     D******   xxzip1               24     28
     D******   xxdash               29     29
     D******   xxzip2               30     33
     D******   xxtypb               38     38
     D   xxprml                1     11
     D   xxdas1               13     13
     D   xxprmh               15     25
     D   xxstat               70     70
    C*
     D                 ds
     D cnkey                   1     10
     D   xcnkey                3      5
    C*
     D                 ds
     D kcnkey                  1     10
     D   kcnky2                3      5
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
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
     C                   parm      '2'           ##call
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      *zeros        ##rrn
    C*
     C     acdpl2        plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      '2'           ##call
     C                   parm      *zeros        ##rrn
     C***  begin chg  ***  03/13/09  **************************************
     C*********          parm                    ##key
     C                   parm                    ##key            10
     C***  end chg ******  03/13/09  **************************************
     C                   parm      SCST          ##st
     C                   parm                    ##view
    C*
     C***  Begin Del  ***  03/13/09  **************************************
     C*****acdpl3        plist
     C*********          parm                    ##prds           40
     C*********          parm      '2'           ##call
     C*********          parm      'WCST'        ##id              4
     C*********          parm                    ##key            10
     C*********          parm                    ##rtc             1
     C***  end del ******  03/13/09  **************************************
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
     C     chgpl         plist
     C                   parm                    ##num
     C                   parm                    ##mask
     C                   parm                    ##dec
     C                   parm                    ##thou
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkaco
     C                   kfld                    kktypa
     C                   kfld                    kkst
     C                   kfld                    kkcnty
     C***  Begin Del  ***  10/02/06  **************************************
     C**********         kfld                    kkzip
     C***  End   Del  ***  10/02/06  **************************************
     C***  Begin Add  ***  10/02/06  **************************************
     C                   kfld                    kkprml
     C***  End   Add  ***  10/02/06  **************************************
     C*
     C     acky          klist
     C                   kfld                    ##co#
     C                   kfld                    sssta6
     C*
     C     cnky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkcnt1
     C*
     C     taky          klist
     C                   kfld                    ##co#
     C                   kfld                    sctypa
     C*
     C     taky1         klist
     C                   kfld                    ##co#
     C                   kfld                    sssta3
     C*
     C*** begin chg *****  03/13/09  **************************************
     C*****gtky          klist
     C*********          kfld                    ##co#
     C*********          kfld                    kktbid
     C*********          kfld                    kkkey
     C     keySTSTL3     klist
     C                   kfld                    xxKey
     C*** end chg *******  03/13/09  **************************************
      *
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        sssta1 <> *zeros  OR
     C                             sssta3 <> *blanks OR sssta4 <> *blanks OR
     C                             sssta5 <> *blanks OR sssta6 <> *blanks
     C                   eval      *in11 = *on
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C*
     C* Check for Subset searches
     C                   EXSR      OTHFKS
     C*
     C* Check Premium Levels
     C*
     C                   IF        sssta1 < *zeros
     C                   seton                                        81  90
     C                   EndIf
     C*
     C* chain to WTTAL with Audit Type field
     C                   IF        sssta3 <> *blanks and *IN90 =  *OFF
     C     TAKY1         CHAIN     PTA01                              60
     C                   IF        *IN60
     C                   seton                                        83  90
     C                   ENDIF
     C                   EndIf
     C*
     C* chain to WTGTL with State Code field
     C                   IF        sssta4 <> *blanks and *IN90 =  *OFF
     c*** begin chg ***  03/13/09  ****************************************
     C*********          movel(p)  'WCST'        KKTBID
     C*********          movel(P)  sssta4        kkkey
     C*****gtky          chain     pgt01                              60
     C                   eval      xxKey = SCST
     C     keySTSTL3     chain     pst03                              73
     c*** end chg *****  03/13/09  ****************************************
     C   60              seton                                        84  90
     C                   EndIf
     C*
     C******* BEGIN ADD ***** 01/14/08 ************************************
      * Require State when County is entered
     C                   IF        sssta5 <> *blanks and *IN90 = *OFF
     C                   IF        sssta4 = *BLANK
     C                   MOVE      *ON           *IN80
     C                   MOVE      *ON           *IN90
     C                   ELSE
     C                   MOVE      *OFF          *IN80
     C                   ENDIF
     C                   ENDIF
     C******* END   ADD ***** 01/14/08 ************************************
     C* chain to WTCNL1 with State & County Subset Fields
     C                   IF        sssta5 <> *blanks and *IN90 =  *OFF
     C                   movel(p)  sssta4        xxwa04            4
     C                   move      sssta5        xxwa04
     C                   movel(p)  xxwa04        kkcnt1
     C     cnky          chain     pcn01                              60
     C   60              seton                                        85  90
     C                   EndIf
     C*
     C*
     C* chain to WTACL with Audit Company subset field
     C                   IF        sssta6 <> *blanks and *IN90 =  *OFF
     C     acky          chain     pac01                              60
     C   60              seton                                        86  90
     C                   EndIf
     C*
     C                   if        not *in90 and
     C                             ssStat <> *blank and
     C                             ssStat <> 'A'    and
     C                             ssStat <> 'I'
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
     C                   eval      ssstat = 'A'
     C                   other
     C                   eval      ssstat = *blank
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkco# = ##co#
     C                   move      *loval        kkaco
     C                   move      *loval        kktypa
     C                   move      *loval        kkst
     C                   move      *loval        kkcnty
     C                   move      *loval        kkzip
     C***  Begin Add  ***  10/02/06  **************************************
     C                   eval      kkprml = *loval
     C***  End   Add  ***  10/02/06  **************************************
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C     scpst1        ifne      *blanks
     C                   movel(p)  scpst1        kkaco
     C                   endif
     C     scps1a        ifne      *blanks
     C                   move(p)   scps1a        kktypa
     C                   endif
     C     scps1b        ifne      *blanks
     C                   move(p)   scps1b        kkst
     C                   endif
     C     scps1c        ifne      *blanks
     C                   move(p)   scps1c        kkcnty
     C                   endif
     C*
     C***  Begin Add  ***  10/02/06  **************************************
     C                   eval      kkprml = *loval
     C***  End   Add  ***  10/02/06  **************************************
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kkco# = ##co#
     C                   eval      kkaco = scaco
     C                   eval      kktypa = sctypa
     C                   eval      kkst = scst
     C                   eval      kkcnty = sccnty
     C***  Begin Add  ***  10/02/06  **************************************
     C                   eval      kkprml = scprml
     C***  End   Add  ***  10/02/06  **************************************
     C                   endsr
    C*================================================================
    C* KJDO - Search Subroutine for F10
    C*================================================================
     C     KJDO          begsr
     C*
     C                   Select
     C     hfld          wheneq    'SSSTA3'
     C** **              move      *blanks       sssta3
     C                   call      'W2281R'      acdpl1
     C     ##rrn         chain     pta0p                              99
     C     *in99         ifeq      *on
     C                   eval      *in40 = *on
     C*
     C                   else
     C                   eval      *in41 = *on
     C                   eval      sssta3 = tatypa
     C                   endif
     C*
     C*** begin del *****  03/13/09  **************************************
     C*****hfld          wheneq    'SSSTA4'
     C*********          call      'W2493R'      acdpl3
      *
     C*****##rtc         ifeq      'A'
     C*********          exsr      pgmend
     C*********          endif
      *
     C*********          move(p)   'WCST'        kktbid
     C*********          movel(p)  ##key         kkkey
     C*****gtky          chain     pgt01                              99
     C**** *in99         ifeq      *on
     C*********          eval      *in41 = *on
     C*
     C*********          else
     C*********          eval      *in42 = *on
     C*********          movel(p)  gtkey         SSSTA4
     C*********          endif
     C***  end del ******  03/13/09  **************************************
     C*
     C     hfld          wheneq    'SSSTA5'
     C                   move      *blanks       sssta5
     C                   call      'W2215R'      acdpl2
     C     ##rrn         chain     pcn0p                              99
     C     *in99         ifeq      *on
     C                   eval      *in42 = *on
     C*
     C                   else
     C                   eval      *in43 = *on
     C                   movel(p)  xcnkey        SSSTA5
     C                   endif
      *
     C     hfld          wheneq    'SSSTA6'
     C                   move      *blanks       sssta6
     C                   call      'W2290R'      acdpl1
     C     ##rrn         chain     pac0p                              99
     C     *in99         ifeq      *on
     C                   eval      *in43 = *on
     C                   else
     C                   eval      *in44 = *on
     C                   movel(p)  ackey         SSSTA6
     C                   endif
     C*
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      SCACO  = AAACO
     C                   eval      SCTYPA = AATYPA
     C                   eval      SCST   = AAST
     C                   eval      SCCNTY = AACNTY
     C***  Begin Add  ***  10/02/06  **************************************
     C                   eval      scprml = AAPRML
     C***  End   Add  ***  10/02/06  **************************************
     C*
     C                   select
     C                   when      info# = 1
     C                   eval      *in31 = *on
     C                   eval      SCDATA =  '  ' + AAACO +
     C                                   '      ' + AATYPA +
     C                                  '       ' + AAST +
     C                                  '       ' + AACNTY +
     C                                   '      ' + %editc(AAPRML:'J') +
     C                                       '- ' + %editc(AAPRMH:'J') +
     C                                   '      ' + AASTAT
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
     C                   move      *on           *in12
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
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
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
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C* If premium below subset amount, do not select.
     C                   If        sssta1 > *zeros AND aaprml <  sssta1
     C                   eval      meetss = *off
     C                   endif
     C*
     C* If Audit Type is not matching, do not select
     C*
     C                   IF        sssta3 <> *blanks AND sssta3 <> aatypa
     C                   eval      meetss = *OFF
     C                   ENDIF
     C*
     C* If State Code not matching, do not select
     C*
     C                   IF        sssta4 <> *blanks AND sssta4 <> aast
     C                   eval      meetss = *OFF
     C                   EndIf
     C*
     C* If County Code not matching, do not select
     C*
     C                   IF        sssta5 <> *blanks AND sssta5 <> aacnty
     C                   eval      meetss = *OFF
     C                   EndIf
     C*
     C* If Audit Company not matching, do not select
     C*
     C                   IF        sssta6 <> *blanks AND sssta6 <> aaaco
     C                   eval      meetss = *OFF
     C                   EndIf
     C*
     C                   if        ssStat <> *blanks and ssStat <> AASTAT
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   endsr
** Column headings
Opt  Company   Type     State   County            Premium Range          Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change status         YNN                            06/04/07
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
215F15=Fold/truncate        NNN
217F17=Subset               YYY
3
