     H****************************************************************
     H*  (C) Copyright StoneRiver, Inc., 2009
     H****************************************************************
    H*                                                              *
     H* Description: Restriction of User IDs access to specific      *
     H*              claims maintenance program.                     *
     H*                                                              *
     H*                                                              *
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     H*  03/07/09  C:220  SUPPORT: 67464                             *
     H*                   To correct position to function            *
     H*                                                              *
     H*  07/05/07  C:132  PROJECT: CU114 - 58997                     *
     H*                   Missing RollUp/RollDown                    *
     H*                                                              *
     H*                                                              *
     H****************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw2474f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmxrp     if   e             disk
     F                                     rename(pxr01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwmxrl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pxr01:dbrcdf)
    F*
    F*  Secondary logical file definition (for displaying items)
     Fwmxrl1    if   e           k disk
     F                                     infds(dbfds2)
     F                                     rename(pxr01:dbrcd2)
    D*
     D hd              s             78    dim(2) ctdata perrcd(1)
     D of              s              1    dim(12) ctdata perrcd(1)
     D ofd             s             35    dim(12) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Claim Restriction Maintenance'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W2474R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   2
     D sf1pag          c                   12
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
     D kkco#           s                   like(xrco#)
     D kkusid          s                   like(xrusid)
     D kkfnd           s                   like(xrfnd)
     D kkfyr           s                   like(xrfyr)
     D kkcase          s                   like(xrcase)
     D*
     D xxco#           s                   like(xrco#)
     D xxusid          s                   like(xrusid)
     D xxfnd           s                   like(xrfnd)
     D xxfyr           s                   like(xrfyr)
     D xxcase          s                   like(xrcase)
     D xxstat          s                   like(xrstat)
     D zzcase          s                   like(xrcase)
     D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     D                 DS
     D  view1                  1     70
     D  x1usid                 1     10
     D  x1fnd                 13     15
     D  x1fyr                 20     23
     D  x1case                27     33
     D  x1STAT                53     53
     D*
     D                 DS
     D  view2                  1     70
     D  x2fnd                  1      3
     D  x2fyr                  8     10
     D  x2case                14     20
     D  x2usid                23     32
     D  x2STAT                53     53
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
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkusid
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C     key2          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkusid
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
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
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C***** BEG ADD ***** 03/07/09 ************************************
     C                   IF        scpst11 <> *blanks or scpst22 <> *blanks
     C                   MOVE      scpst11       kkusid
     C                   MOVE      scpst22       kkcase
     C                   END
     C***** END ADD ***** 03/07/09 ************************************
     C***** BEG Del ***** 03/07/09 ************************************
     C*************      If        *IN21 and scpst2 <> *Blanks
     C*************      If        *IN22 and scpst2 <> *Blanks
     C*************      movel     *blanks       scpst2
     C*************      endif
     C*************      if        *in22 and scpst1 <> *blanks
     C*************      if        *in21 and scpst1 <> *blanks
     C*************      movel     *blanks       scpst1
     C*************      endif
     C***** BEG Del ***** 03/07/09 ************************************
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C
     C                   eval      kkco#  = ##co#
     C                   eval      kkusid = *loval
     C                   eval      kkfnd  = *loval
     C                   eval      kkfyr  = *loval
     C                   eval      kkcase = *loval
     C
     C                   endsr
    C* ===============================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C
     C                   eval      kkco#  = XRco#
     C***** BEG CHG ***** 03/07/09 ************************************
     C*************      eval      kkusid = XRusid
     C                   eval      kkusid = scpst1
     C                   eval      kkfnd  = XRfnd
     C                   eval      kkfyr  = XRfyr
     C*************      eval      kkcase = XRcase
     C                   move      scpst2        kkcase
     C
     C***** END CHG ***** 03/07/09 ************************************
     C
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
      ***** BEG ADD ***** 07/05/07 ************************************
     C                   eval      kkco#  = XXco#
     C                   eval      kkusid = XXusid
     C                   eval      kkfnd  = XXfnd
     C                   eval      kkfyr  = XXfyr
     C                   eval      kkcase = XXcase
      ***** END ADD ***** 07/05/07 ************************************
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     *
     C                   eval      xxco#  = XRco#
     C                   eval      xxusid = XRusid
     C                   eval      xxfnd  = XRfnd
     C                   eval      xxfyr  = XRfyr
     C                   eval      xxcase = XRcase
     C                   eval      xxstat = XRstat
      *
     C                   if        *in21
     C                   movel     xxusid        x1usid
     C                   move      xxfnd         x1fnd
     C                   move      xxfyr         x1fyr
     C                   movel     xxcase        x1case
     C                   move      xxstat        x1stat
     C                   movel(P)  view1         scdata
     C                   movel(P)  hd(1)         schdl1
     C                   endif
      *
     C                   if        *in22
     C                   movel     xxusid        x2usid
     C                   move      xxfnd         x2fnd
     C                   move      xxfyr         x2fyr
     C                   movel     xxcase        x2case
     C                   move      xxstat        x2stat
     C                   movel(P)  view2         scdata
     C                   movel(P)  hd(2)         schdl1
     C                   endif
      *
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
      *
     c     scsel         caseq     '20'          opt20
     c                   endcs

     C                   endsr

      *================================================================
      * OPT20 - SUBROUTINE FOR OPTION 20
      *================================================================
      *
      * OPTION 20=CHANGE STATUS
      *
     C     OPT20         Begsr

     C                   Move      'S'           ##Mode
     C                   Call      ACDPGM        ACDPL                  99

     C                   Endsr

    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   Eval      *INLR = *ON
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C                   read      dbrcdf                                 95
     C* N95              z-add     dbrrn         ##rrn
     C                   endif
     C*
     C                   if        *in22 = *on
     C                   read      dbrcd2                                 95
     C* N95              z-add     dbrrn2        ##rrn
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C                   readp     dbrcdf                                 95
     C* N95              z-add     dbrrn         ##rrn
     C                   endif
     C                   if        *in22 = *on
     C                   readp     dbrcd2                                 95
     C* N95              z-add     dbrrn2        ##rrn
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C                   if        *in22 = *on
     C     key2          setgt     dbrcd2                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif
     C                   if        *in22 = *on
     C     key2          setll     dbrcd2                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
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
     C***** BEG CHG ***** 03/07/09 ************************************
     C*************      IF        *in21 AND scpst1 <> *blanks AND
     C*************                scpst1 <> XRusid
     C                   IF        scpst11 <> *blanks AND
     C                             scpst11 <> XRusid
     C***** END CHG ***** 03/07/09 ************************************
     C                   move      *OFF          meetss
     C                   EndIF
     C*
     C***** BEG CHG ***** 03/07/09 ************************************
     C*************      IF        *in22 AND scpst2  > *blanks
     C                   IF        scpst22 <> *blanks
     C*************      move      scpst2        zzcase
     C                   move      scpst22       zzcase
     C***** END CHG ***** 03/07/09 ************************************
     C                   if        zzcase <> XRcase
     C                   move      *OFF          meetss
     C                   EndIF
     C                   EndIF                                                  *IN22 & SCPST2
     C*
     C                   endsr
    C* ===============================================================
** Column headings
Opt  User ID     Group   Year  Case                   Status
Opt  Group  Year  Case     User ID                    Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
