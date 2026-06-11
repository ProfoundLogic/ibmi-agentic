     Hcopyright('(c) Copyright Fiserv, Inc., 2005 - 2007')
    Hdftactgrp(*no)
     *****************************************************************
     *    PROGRAM: W2280R                                            *
     *                                                               *
     *    DESCRIPTION: W/W Business Catagories                       *
     *                                                               *
     *    CREATED BY:  C:53   03/23/05   PROJECT# WC1196             *
     *                                                               *
     *****************************************************************
      * Revised:                                                      *
      *                                                               *
      *  01/26/07  C:33  Project: WC1196                              *
      *                  Do not allow for deletion/inactivation when  *
      *                   code is used in Audit Type Assignment Ctl   *
      *                                                               *
      *  08/21/06  C: 132 Project: WC1196                             *
      *                 - Change to default subset to A=Active        *
      *                                                               *
     *****************************************************************
     Fw2280f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwttgp     if   e             disk
     F                                     rename(ptg01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwttgl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(ptg01:dbrcdf)
     F***  Begin Add  ***  01/26/07  **************************************
     Fwtssl     if   e           k disk
     F*
     DCodeBeingUsed    pr             1n
     D code                                like(SCBCTG)
     D***  End   Add  ***  01/26/07  **************************************
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Business Categories'
     D deletd          c                   '*Deleted'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W2280R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
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
     D @@rrn           s              9  0
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D kkco#           s                   like(tgco#)
     D kkbctg          s                   like(tgbctg)
     D xxCall          s                   like(@@Call) inz
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D                 ds
     D xxdata                  1     70
     D   xxbctg                3      5
     D   xxbcds               11     40
     D   xxstat               47     47
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
     C                   kfld                    kkbctg
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        ssstat <> *blanks
     C                   eval      *in11 = *on
     C                   endif
     C*
     C                   endsr
     C*
     C***  Begin Add  ***  01/26/07  **************************************
    C*================================================================
    C* ErrChkOpt04 - Error checking for Opt 4
    C*================================================================
     C     ErrChkOpt04   begsr
     C*
     C  N90              if        CodeBeingUsed(SCBCTG)
     C                   seton                                        71  90
     C                   endif
     C*
     C                   endsr
     C*
    C*================================================================
    C* ErrChkOpt20 - Error checking for Opt 20
    C*================================================================
     C     ErrChkOpt20   begsr
     C*
     C  N90              if        CodeBeingUsed(SCBCTG)
     C                   seton                                        71  90
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
     C     SCSEL         caseq     '4'           ErrChkOpt04
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
     C                   select
     C                   when      xxCall = '2'
     C                   eval      ssstat = 'A'
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkco# = ##co#
     C                   move      *loval        kkbctg
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C                   eval      kkco# = ##co#
     C                   move      scpst1        kkbctg
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkco# = ##co#
     C                   eval      kkbctg= scbctg
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      xxbctg = tgbctg
     C                   eval      xxbcds = tgbcds
     C                   eval      xxstat = tgstat
     C*
     C                   eval      scdata = xxdata
     C*
     C                   eval      scbctg= tgbctg
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
     C*
     C                   if        %parms >= 2
     C                   eval      @@kc = *inkc
     C                   endif
     C*
     C                   if        %parms >= 3
     C                   eval      @@kl = *inkl
     C                   endif
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
     C     xxCall        comp      '1'                                    09
     C     xxCall        comp      '2'                                    30
     C***********        eval      ssstat = 'A'
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
     C*
     C                   z-add     scdbrr        @@rrn
     C*
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C     ssstat        ifne      *blanks
     C     ssstat        andne     tgstat
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   endsr





     P***  Begin Add  ***  01/26/07  **************************************
    P*================================================================
    P* CodeBeingUsed - Checked for code being used in another file
    P*================================================================
     PCodeBeingUsed    b
     DCodeBeingUsed    pi             1n
     D code                                like(SCBCTG)

     C     tableKey      klist
     C                   kfld                    kkCo#
     C                   kfld                    code

     C     tableKey      chain     pss01

     C                   if        %found and SSSTAT <> 'I'
     C                   return    *on
     C                   endif

     C                   return    *off

     PCodeBeingUsed    e
     C***  End   Add  ***  01/26/07  **************************************





** Column headings
Opt  Category  Description                       Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
