     Hcopyright('(c) Copyright Fiserv, Inc., 2006 - 2007')
     Hdftactgrp(*no)
     *****************************************************************
     *    PROGRAM: W2285R                                            *
     *                                                               *
     *    DESCRIPTION: W/W Business Catagory Assignment              *
     *                                                               *
     *    CREATED BY:  C:132  08/15/06   PROJECT# WC1196             *
     *                 C:33                                          *
     *****************************************************************
      * Revised:                                                      *
      *                                                               *
      *  06/04/07  C:33  Project: WC1196                              *
      *                  Changes from Q/A testing.                    *
     *****************************************************************
     Fw2285f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwtb3p     if   e             disk
     F                                     rename(pb301:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwtb3l     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pb301:dbrcdf)
     F*
     Fwttgp     if   e             disk
     F                                     rename(ptg01:ptg0p)
     fwttgl     if   e           k disk
     F*
     Fwmprp     if   e             disk
     F                                     rename(ppr01:ppr0p)
     fwmprl     if   e           k disk
     Fwtgtl     if   e           k disk    rename(pgt01:pgt00)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Gov Class/Business Ctgy Assoc'
     D deletd          c                   '*Deleted'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W2285R02'
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
     D ##rtcd          s              3
     D ##view          s              1  0
     D ##date          s              8
     D @@rrn           s              9  0
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@rtcd          s              3
     D length          s              3  0
     D kkco#           s                   like(b3co#)
     D kkType          s                   like(B3TYPE)
     D kkgovn          s                   like(b3govn)
     D kkbctg          s                   like(b3bctg)
     D kkefdt          s                   like(prefdt)
     D kkClas          s                   like(PRCLAS)
     D xxClas          s              5
     D xxCall          s                   like(@@Call) inz
     D x1              s              3  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D                 ds
     D xxdata                  1     70
     D   xxgovn                3      7  0
     D   xxbctg               13     15
     D   xxbcds               24     50
     D   xxstat               67     67
     D
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D*
     DSearchGenTable   pr                  like(GTKEY)
     D tableID                             like(GTTBID) const
     D*
     DValidGenTable    pr                  like(*in72)
     D inValue                             like(SCGOVN) const
     D tableID                             like(GTTBID) const
     D*
     D/COPY sPrString
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
    C     dtcymd        plist
    C                   parm                    srdti
    C                   parm      *blanks       srffmt
    C                   parm      '*CYMD'       srtfmt
    C                   parm                    srdto
    C                   parm                    srerr
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C     key1          klist
     C                   kfld                    kkco#
     C*
     C     key1a         klist
     C                   kfld                    kkco#
     C                   kfld                    kkType
     C                   kfld                    kkgovn
     C                   kfld                    kkbctg
     C*
     C     tgky          klist
     C                   kfld                    kkco#
     C                   kfld                    xxbctg
     C*
     C     rpky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkClas
     C                   kfld                    kkefdt
     C     rpky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkClas
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        SSTYPE <> *blanks or
     C                             sssta1 <> *blanks or
     C                             sssta2 <> *blanks
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
     C                   if        SSTYPE <> *blank and not *in90
     C                   if        SSTYPE <> 'G' and
     C                             SSTYPE <> 'S' and
     C                             SSTYPE <> 'N'
     C                   eval      *in86 = *on
     C                   eval      *in90 = *on
     C***  Begin Add  ***  06/04/07  **************************************
     C                   eval      ssSta1 = *blanks
     C***  End   Add  ***  06/04/07  **************************************
     C                   endif
     C                   endif
     C*
     C                   if        SSSTA1 <> *blank and not *in90
     C*
     C                   select
     C*_____
     C                   when      SSTYPE = 'G'
     C*
     C                   eval      xxClas = RightJstfy(ssSta1:%len(kkClas))
     C                   move      xxClas        kkClas
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         kkefdt
     C     rpky          setgt     ppr01
     C     rpky1         readpe    ppr01                                  99
     C                   IF        *IN99 = *ON
     C                   move      *on           *IN81
     C     *in81         cabeq     *on           ender2                   90
     C                   endif
     C                   IF        prstat <> 'A'
     C                   move      *on           *IN82
     C     *in82         cabeq     *on           ender2                   90
     C                   endif
     C***  Begin Add  ***  06/04/07  **************************************
     C                   eval      ssSta1 = xxClas
     C                   eval      length = %len(kkClas)
     C                   do        length        x1
     C                   if        %subst(ssSta1:x1:1) = ' '
     C                   eval      %subst(ssSta1:x1:1) = '0'
     C                   endif
     C                   enddo
     C***  End   Add  ***  06/04/07  **************************************
     C*_____
     C                   when      SSTYPE = 'S'
     C                   eval      *in81 = not ValidGenTable(SSSTA1:'SIC')
     C*_____
     C                   when      SSTYPE = 'N'
     C                   eval      *in81 = not ValidGenTable(SSSTA1:'NAIC')
     C*_____
     C                   other
     C                   eval      *in87 = *on
     C                   eval      *in90 = *on
     C*_____
     C                   endsl
     C*
     C                   if        *in81
     C                   eval      *in90 = *on
     C                   else
     C                   select
     C                   when      SCTYPE = 'G'
     C** **              eval      SCCCDS = PRDESC
     C                   when      SCTYPE = 'S' or SCTYPE = 'N'
     C** **              eval      SCCCDS = GTDESC
     C                   endsl
     C                   endif
     C*
     C                   endif
     C*
     C     sssta2        ifne      ' '
     C                   movel(p)  sssta2        xxbctg
     C     tgky          chain     ptg01                              99
     C                   IF        *IN99 = *ON
     C                   move      *on           *IN83
     C     *in83         cabeq     *on           ender2                   90
     C                   endif
     C                   IF        tgstat <> 'A'
     C                   move      *on           *IN84
     C     *in84         cabeq     *on           ender2                   90
     C                   endif
     C                   endif
     C*
     C                   if        not *in90 and
     C                             ssStat <> *blank and
     C                             ssStat <> 'A'    and
     C                             ssStat <> 'I'
     C                   seton                                        89  90
     C                   endif
     C*
     C     ender2        endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C*
     C                   clear                   SSTYPE
     C                   clear                   SSSTA1
     C                   clear                   SSSTA2
     C*
     C                   select
     C                   when      xxCall = '2'
     C                   eval      ssstat = 'A'
     C                   other
     C                   eval      ssstat = *blank
     C                   endsl
     C*
     C                   endsr
      *===============================================================
      * KJDO - Subroutine for F10
      *===============================================================
      *
     C     kjdo          begsr
      *
      * Search for Class Code or Business Category in subset window
      *
     C                   select
      *_____
     C                   when      hfld = 'SCGOVN' and
     C                              SCTYPE <> 'G' and
     C                              SCTYPE <> 'S' and
     C                              SCTYPE <> 'N'
     C                   eval      *in87 = *on
      *______
     C                   when      hfld = 'SSSTA1' and SSTYPE = 'G'
     C                   z-add     *zeros        ##RRN
     C                   move      *blanks       ##rtcd
     C                   call      'W0630R'
     C                   parm      '2'           ##call            1
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##RRN
     C                   parm                    ##rtcd
     C                   parm      2             ##view
     C                   parm                    ##date
     C
     C                   clear                   PRCLAS
     C     ##RRN         CHAIN     PPR0P                              99
     C                   select
     C                   when      %found
     C                   movel     PRCLAS        SSSTA1
     C                   move      *on           *in41
     C                   other
     C                   move      *on           *in71
     c                   endsl
      *______
     C                   when      hfld = 'SSSTA1' and SSTYPE = 'S'
     C                   eval      SSSTA1 = SearchGenTable('SIC')
      *______
     C                   when      hfld = 'SSSTA1' and SSTYPE = 'N'
     C                   eval      SSSTA1 = SearchGenTable('NAIC')
      *______
     C                   when      hfld = 'SSSTA1'
     C                   eval      *in87 = *on
     C                   eval      *in90 = *on
      *______
     C     hfld          wheneq    'SSSTA2'
     C                   call      'W2280R'      acdpl1
     C     ##rrn         chain     ptg0p                              99
     C                   if        *in99 =*OFF                                  Active
     C                   movel(p)  tgbctg        sssta2
     C                   move      *off          *in44                          Not Found
     C                   endif
     C*
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkco#  = ##co#
     C                   eval      kkType = *loval
     C                   eval      kkGovn = *loval
     C                   eval      kkBCtg = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C                   eval      kkco# = ##co#
     C                   eval      kkType = scPst1
     C                   select
     C                   when      scPst1 = 'G'
     C                   eval      length = %len(kkClas)
     C                   eval      kkGovn = RightJstfy(scPst1A:length)
     C                   do        length        x1
     C                   if        %subst(kkGovn:x1:1) = ' '
     C                   eval      %subst(kkGovn:x1:1) = '0'
     C                   endif
     C                   enddo
     C                   other
     C                   eval      kkGovn = scPst1A
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkco# = ##co#
     C                   eval      kkType= scType
     C                   eval      kkGovn= scGovn
     C                   eval      kkBCtg= scBCtg
     C*
     C                   endsr
    C*================================================================
    C* %VDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C***  Begin Add  ***  06/04/07  **************************************
     C                   clear                   scData
     C***  Begin Add  ***  06/04/07  **************************************
     C*
     C                   eval      xxbctg = b3bctg
     C                   eval      xxstat = b3stat
     C                   eval      scType = B3TYPE
     C                   eval      scGovn = B3GOVN
     C                   eval      scBCtg = B3BCTG
     C                   clear                   TGBCDS
     C*
     C     tgky          chain     wttgl                              99
     C                   if        not *in99
     C                   movel(P)  tgbcds        xxbcds
     C                   else
     C                   eval      xxbcds = *blanks
     C                   endif
     C*
     C                   select
     C                   when      view# = 1 and info# = 1
     C                   eval      %subst(scData: 3: 1) = B3TYPE
     C                   eval      %subst(scData: 7:10) = B3GOVN
     C                   eval      %subst(scData:20: 3) = B3BCTG
     C                   eval      %subst(scData:29:30) = TGBCDS
     C                   eval      %subst(scData:65: 1) = B3STAT
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
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   TIME                    DSDTTM
     C*
     C                   if        %parms >= 1
     C                   eval      xxCall = @@Call
     C                   endif
     C*
     C                   select
     C                   when      xxCall = '1'
     C                   eval      *in09 = *on
     C                   when      xxCall = '2'
     C                   eval      *in30 = *on
     C                   endsl
     C*
     C                   clear                   sssta1
     C                   clear                   sssta2
     C*
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   select
     C                   when      *in21
     C     key1          reade     dbrcdf                                 95
     C                   endsl
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   select
     C                   when      *in21
     C     key1          readpe    dbrcdf                                 95
     C                   endsl
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   select
     C                   when      *in21
     C     key1a         setgt     dbrcdf                             96
     C                   endsl
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   select
     C                   when      *in21
     C     key1a         setll     dbrcdf                             96
     C                   endsl
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
     C                   select
     C                   when      SSTYPE <> *blanks and SSTYPE <> B3TYPE
     C                   eval      meetss = *off
     C*
     C*  Active Status if Search Mode
     C                   when      SSSTAT <> *blanks and SSSTAT <> B3STAT
     C                   eval      meetss = *off
     C*
     C*  Subset on Status
     C                   when      SSSTA1 <> *blanks and SSSTA1 <> B3GOVN
     C                   eval      meetss = *off
     C*
     C*  Subset on Business category
     C                   when      SSSTA2 <> *blanks and SSSTA2 <> B3BCTG
     C                   eval      meetss = *off
     C*
     C                   endsl
     C*
     C                   endsr

     P*====================================================================
     P* Search the generic table values
     P*====================================================================
     PSearchGenTable   b

     DSearchGenTable   pi                  like(GTKEY)
     D tableID                             like(GTTBID) const

     D ##Data          s             40
     D ##Mode          s              1                 inz
     D ##TbID          s                   like(GTTBID)
     D ##Key           s                   like(GTKEY)  inz
     D rtnCode         s              1                 inz

     C                   call      'W2493R'
     C                   parm                    ##data
     C                   parm                    ##mode
     C                   parm      tableID       ##TbID
     C                   parm                    ##Key
     C                   parm                    rtnCode

     C                   select
     C                   when      ValidGenTable(##Key:tableID)
     C                   eval      *in41 = *on
     C                   other
     C                   eval      *in71 = *on
     C                   endsl

     C                   return    ##Key

     PSearchGenTable   e

     P*====================================================================
     P* Validate the generic table values
     P*====================================================================
     PValidGenTable    b

     DValidGenTable    pi                  like(*in72)
     D inValue                             like(SCGOVN) const
     D tableID                             like(GTTBID) const

     C     tableKey      klist
     C                   kfld                    ##Co#
     C                   kfld                    tableID
     C                   kfld                    inValue

     C                   clear                   GTDESC
     C     tableKey      chain     pgt00
     C** **              eval      SCCCDS = GTDESC
     C                   return    %found

     PValidGenTable    e





** Column headings
Opt  Type  Code       Category   Description                        Sts
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
