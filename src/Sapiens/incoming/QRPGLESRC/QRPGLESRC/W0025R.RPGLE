     H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2005
    H*
     H*****************************************************************
     H*  CREATED:
     H*
     H* 06/24/20 C:180 Support: 89768
     H*                Retrieve correct fund name
     H*
     H* 02/21/20 C:079 Support: 99 87576
     H*                Subset on group
     H*
     H* 05/24/07 C:136 Support: WA135 59723
     H*                Use group that calling program is viewing
     H*                instead of user parms.
     H*
     H* 09/21/05 C:53  PROJECT: WC1309
     H*
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0025f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwdr1p     if   e             disk
     F                                     rename(pr101:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwdr1l     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pr101:dbrcdf)
     Fwtabl     if   e           k disk
      *** Begin Add *** 06/24/20 **************************************
     Fwmfnl     if   e           k disk
      *** End   Add *** 06/24/20 **************************************
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'State Comp Rates by Ben Type'
     D deletd          c                   '*Deleted'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0025R02'
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
     D kkco#           s                   like(r1co#)
     D kkfnd           s                   like(r1fnd)
     D kkst            s                   like(r1st)
     D kkbr2           s                   like(r1br2)
     D kkefdt          s                   like(r1efdt)
     D ##num           s             80
     D ##mask          s             10
     D ##dec           s              3  0
     D ##thou          s              1
     D*** Beg Add *** 02/21/20 *********************************************
     D decFnd          s                   like(R1fnd)
     D*** End Add *** 02/21/20 *********************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D                 ds
     D xxdata                  1     70
     D   xxst                  2      3
     D   xxefdt                8     19
     D   xxbr2                25     26
     D   xxbr2d               35     55
     D   xxrate               58     66
     D   xxstat               70     70
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
      *** beg add ***** 08/24/07 **************************************
     C                   parm                    @@fnd             3
      *** end add ***** 08/24/07 **************************************
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
      *** beg add ***** 08/24/07 **************************************
     C                   parm                    @1fnd             3
      *** end add ***** 08/24/07 **************************************
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
     C     chgpl         plist
     C                   parm                    ##num
     C                   parm                    ##mask
     C                   parm                    ##dec
     C                   parm                    ##thou
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkbr2
     C                   kfld                    kkefdt
     C*
     C     t2key         klist
     C                   kfld                    ##co#
     C                   kfld                    kkkey
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C     *like         define    b2key         kkkey
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        ssstat <> *blanks or
     C                             ssst   <> *blanks
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
     C     ssstat        ifne      ' '
     C     ssstat        andne     'A'
     C     ssstat        andne     'I'
     C                   movea     '11'          *in(89)
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C*
     C     @@call        ifeq      '2'
     C                   eval      ssstat = 'A'
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkco# = ##co#
     C                   eval      kkfnd = ##fnd
     C                   move      *loval        kkst
     C                   move      *loval        kkefdt
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   move      scpst1        kkst
     C                   move      *loval        kkefdt
     C*
     C                   if        scps1a <> *blanks
     C                   movel(p)  scps1a        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
     C                   endif
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kkst = scst
     C                   eval      kkefdt = scefdt
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      xxst = r1st
     C                   eval      xxbr2 = r1br2
     C                   eval      xxstat = r1stat
     C*
     C*
     C                   eval      kkkey  = r1br2
     C     t2key         chain     pb201                              99
     C                   eval      xxbr2d = b2desc
     C*
     C                   z-add     r1efdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         x1efdt            8 0
     C*
     C                   move      *blanks       ##dec
     C                   move      *zeros        ##thou
     C*
     C                   movel(p)  x1efdt        ##num
     C                   movel(p)  'DATE'        ##mask
     C                   call      chgnum        chgpl
     C                   movel(p)  ##num         xxefdt
     C*
     C                   move      4             ##dec
     C                   movel(p)  r1rate        ##num
     C                   movel(p)  'XXXX'        ##mask
     C                   call      chgnum        chgpl
     C                   movel(p)  ##num         xxrate
     C*
     C                   eval      scdata = xxdata
     C*
     C                   eval      scst = r1st
     C                   eval      scefdt = r1efdt
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
      /Free
      //*** Begin Add *** 06/24/20 ************************************
       If Q1prms > 1;
          ##fnd = %int(@@fnd);
          chain (##co#:##fnd) WMFNL;
          If %found(WMFNL);
             ##fnam = FNname;
             exsr keyinz;
          Endif;
       Endif;
      //*** End   Add *** 06/24/20 ************************************
      /End-Free
     C     @@call        comp      '1'                                    09
     C     @@call        comp      '2'                                    30
     C                   movel(p)  ##fnam        scfnam
     C                   movel(p)  ##fnd         scfnd
      *** beg add ***** 08/24/07 **************************************
     C                   if        q1prms > 1
     C                   move(p)   @@fnd         scfnd
     C                   endif
     C                   move(p)   scfnd         @1fnd
      *** end add ***** 08/24/07 **************************************
       //*** Beg Add ***** 02/21/20 ***********************************
      /Free
                         decFnd = %Dec(@@fnd:3:0);
      /End-free
       //*** End Add ***** 02/21/20 ***********************************
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
     C*
     C                   z-add     scdbrr        ##rrn
     C*
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C     ssstat        ifne      *blanks
     C     ssstat        andne     r1stat
     C                   eval      meetss = *off
     C                   endif
     C*
     C     ssst          ifne      *blanks
     C     ssst          andne     r1st
     C                   eval      meetss = *off
     C                   endif
     C*** Beg Add *** 02/21/20 *********************************************
     C*
     C     decFnd        ifne      *zeros
     C     decFnd        andne     r1fnd
     C                   eval      meetss = *off
     C                   endif
     C*** End Add *** 02/21/20 *********************************************
     C*
     C                   endsr
** Column headings
Opt  State   Eff Date   Benefit Type   Benefit Description       Rate    Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
