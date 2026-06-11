     H*  (C) Copyright Progressive Data Solutions, Inc., 2003
      *
      *****************************************************************
      *  DESCRIPTION:  Clarendon preprocessor data extract            *
      *                Prompt Screen                                  *
      *                                                               *
      *  CALLS TO:                                                    *
      *                                                               *
      *  CREATED: 09/18/2001  JOHN STONE                              *
      *****************************************************************
      *  NOTE:  @@RRN is used for returning the RRN of Subfile for    *
      *         selection.                                            *
      *                                                               *
      *  01/07/05 C:136 Support: WA121 44786                          *
      *                 Add exclude A/R fields to prompt              *
      *                 Change look of screen to be more standardized.*
      *                 Made changes to error check routine for       *
      *                 screen 04.                                    *
      *****************************************************************
      * Revised:
      *
      * 07/22/03  C:61  Support Wa099 32376
      *               - Correct option "20=Change Status"
      *               - Change to comply with company standards
      *               - Changes not commented.
      *
      *****************************************************************
      *  INDICATOR USAGE:                                             *
      *                                                               *
      *                                                               *
      *****************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw8125f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
     *
     *  Physical file definition (for refreshing subfile record)
     Fwxpgp     uf   e             disk
     F                                     rename(xpg01:pfrcdf)
     *
     *  Primary logical file definition (for displaying items)
     Fwxpgl     uf a e           k disk
     F                                     infds(dbfds)
     F                                     rename(xpg01:dbrcdf)
      *
      *  Fund file used to load the subfile
     FWMFNL     If   E           K Disk
      *
      *** beg add ***** 01/07/05 ***********************************************
     FWTABL     if   e           k disk
      *** end add ***** 01/07/05 ***********************************************
     *
     *
     *
     *
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
     D Arr10           S              1    Dim(10)
     *
     *
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Clarendon Pre-Proc Extract'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W8125R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12


    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##lvl           s              3  0
     D ##type          s              1
     D ##outq          s             10
     D H6              S              6
     D H8              S              8
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D kkco#           s                   like(pgco#)
     D kkgrp           s                   like(pggrp)
     D Kkco#A          S              3
     D Srdfld          S             80
     D Srdlen          S              3  0
     D Datea           S              6
     D Datea8          S              8
     D DateJan         S              8
     D Mth             S              4  0
     D Basedate        S               D   DATFMT(*ISO)
     D ME_Date         S               D   DATFMT(*ISO)
     D Today           S               D   DATFMT(*USA)
     D Datusa          S               D   DATFMT(*USA)
     D Asofdt          S               D   DATFMT(*USA)
     D Timestmp        S             12  0
      *
     D Edited          S              1n
     D Six_Pos         S              1n
     D Eight_Pos       S              1n
     D Ten_Pos         S              1n
      *** beg add ***** 01/07/05 ***********************************************
     d goodARcode      s               n
     d firstExfmt      s               n
     d monthEnd        s               d
     d ix              s              1  0 inz(1)
     d lda             ds          1024
     d  scar1                         2
     d  scar2                         2
     d  scar3                         2
     d  scar4                         2
     d  scar5                         2
      *** end add ***** 01/07/05 ***********************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     *
     *
      *** beg chg ***** 01/07/05 ***********************************************
     C***************** /COPY SCOPYWW01C
     d/copy sprdate
     d/copy sprmessage
     d/copy sprruncmd
     d/copy sprstring
     d/copy sprwrkwth
     d/copy wprar
     c/copy scopyww41c

     c     *dtaara       define    *lda          lda
      *** end chg ***** 01/07/05 ***********************************************
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
     C                   parm                    ##lvl
     C                   parm                    ##type
     C                   parm                    ##outq
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkgrp

     C     key2          Klist
     C                   Kfld                    kkco#
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
     C                   endsr


    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   z-add     *loval        Kkgrp
     C                   endsr


    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   Move      Scpst1        Kkgrp
     C                   endsr



    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   z-add     scgrp         Kkgrp
     C                   endsr



    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr

     C                   Eval      scgrp  = Pggrp
     C                   Eval      scgrpn = Pgname

     c                   clear                   scsts
     C                   Eval      *in31 = *off

     C                   If        Pgflag = 'E'
     C                   Eval      scsts  = 'Exclude'
     C                   Endif

     C                   If        Pgflag = 'I'
     C                   Eval      scsts  = 'Include'
     C                   Eval      *in31 = *on
     C                   Endif

     C                   endsr



      *================================================================
      * OPT20 - SUBROUTINE FOR OPTION 20
      *================================================================
      *
      * OPTION 20=CHANGE STATUS
      *
     C     OPT20         Begsr

     c                   unlock    wxpgp
     c                   unlock    wxpgl
     C                   Move      'S'           ##Mode
     C                   Call      ACDPGM        ACDPL                  99

     C                   Endsr



    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr

     c     *inkg         caseq     *on           Dsp_Scr04
     c                   endcs

     C                   endsr



    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
      *
     c     scsel         caseq     '20'          opt20
     c                   endcs

     C                   endsr



    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   Eval        *Inlr = *On
     C                   endsr



    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   z-add     ##co#         Kkco#
      *
     C                   Time                    Timestmp
     C     *mdy          Move      Timestmp      Today
     C     *USA          Move      '01/31/1980'  basedate
      *
     C                   Movel(P)  ##conm        Srdfld
     C                   CALL      'SRCENTER'
     C                   Parm                    Srdfld
     C                   Parm      40            Srdlen
     C                   Movel     Srdfld        Scconm
      *
     C                   Exsr      Load_Wxpgp

     C                   endsr



    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C                   read(n)   dbrcdf                                 95
     C                   endif
     C                   endsr



    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C                   readp(n)  dbrcdf                                 95
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
     C                   endsr



    C*================================================================
      * LOAD WORK FILE                                                *
    C*================================================================
     C     Load_Wxpgp    Begsr
      *
      * Clear file first
      *
     C                   Dou       %eof
     C                   Read      Wxpgp
      *
     C                   If        not %EOF
     C                   Delete    pfrcdf
     C                   Endif
      *
     C                   Enddo
      *
      * Load from group master
      *
     C     Kkco#         Setll     Wmfnl
      *
     C                   Dou       %eof
     C     Kkco#         Reade     Wmfnl
      *
     C                   If        not %eof
     C                   Eval      Pgco#  = Kkco#
     C                   Eval      Pggrp  = Fnfnd
     C                   Eval      Pgname = Fnname
     C                   Eval      Pgflag = 'E'
     C                   Write     Dbrcdf
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr



      *****************************************************************
      *   DISPLAY SCR04 TO PROMPT FOR AS OF DATE                      *
      *****************************************************************
      *
     C     Dsp_Scr04     Begsr

     C                   Dou       *INKC = *on or
     C                             *INKL = *on

      *** beg chg **** 01/07/05 ***************************************
     C****************** If        *In90= *Off
     C                   If        firstExfmt = *off
     c                   eval      firstExfmt = *on
      *** end chg **** 01/07/05 ***************************************
     C     Today         Subdur    Basedate      Mth:*M
     C     Basedate      Adddur    Mth:*M        ME_Date
     C     *Usa          Movel     ME_Date       Scasof
     C                   Eval      Schm = 'M'
     C                   Endif

     C                   Exfmt     Scr04

     C                   If        *INKC = *on or
     C                             *INKL = *on
     C                   Leave
     C                   Endif

      *** beg chg ***** 01/07/05 ***********************************************
     C****************   Exsr      Date_Chk
     C                   Exsr      errCkScr04
      *** end chg ***** 01/07/05 ***********************************************

     C                   If        *In90 = *on
      *** beg chg ***** 01/07/05 ***********************************************
     C*****************  Iter
     c                   eval      *in90 = *off
     c                   iter
     c                   else
     c                   out       lda
      *** end chg ***** 01/07/05 ***********************************************
     C                   Endif

      *** beg chg ***** 01/07/05 ***********************************************
     C******Mdy0*******  Move      Asofdt        Datea
     C     *usa          move      scasof        datUSA
      *** end chg ***** 01/07/05 ***********************************************
     C                   Move      Kkco#         Kkco#A
      *** beg del ***** 01/07/05 ***********************************************
     C******Mdy0*******  Move      Datea         Datusa
      *** end del ***** 01/07/05 ***********************************************
     C     *Usa0         Move      Datusa        Datea8
     C     *Usa0         Move      Datusa        DateJan
     C                   Movel     '0101'        DateJan

     C                   If        Schm = 'M'
     C                   Call      'W8126C1'
     C                   Parm                    Kkco#A
     C                   Parm                    DateJan
     C                   Parm                    Datea8
     C                   Parm                    ##outq
     C                   Leave

     C                   Else

     C                   Call      'W8127C1'
     C                   Parm                    Kkco#A
     C                   Parm                    Datea
     C                   Parm                    Datea8
     C                   Leave
     C                   Endif

     C                   Enddo

     C                   Eval      *inlr = *on
     C                   Endsr



      *** beg del ***** 01/07/05 **************************************
    C*================================================================
    C* Date Check Routine
    C*================================================================
     C*****Date_Chk****  Begsr
      *****************
     C*****************  Movea     Scasof        Arr10(1)
     C*****************  Eval      Edited    = *Off
     C*****************  Eval      Six_Pos   = *Off
     C*****************  Eval      Eight_Pos = *Off
     C*****************  Eval      Ten_Pos   = *Off
     C*****************  Eval      *In30     = *On
      *****************
     C*****************  Eval      I = 1
     C*****'/'*********  Lookup    Arr10(I)                               58
     C*****************  If        *in58 = *On
     C*****************  Eval      Edited = *On
     C*****************  Endif
      *****************
     C*****************  Select
     C*****************  When      Arr10(4) = *Blank
     C*****************  Eval      Six_Pos = *On
     C*****************  Move      Scasof        H6
     C*****************  When      Arr10(2) = *Blank
     C*****************  Eval      Eight_Pos = *On
     C*****************  Move      Scasof        H8
     C*****************  Other
     C*****************  Eval      Ten_Pos = *On
     C*****************  Endsl
      *****************
     C*****************  If        Edited
      *****************
     C*****************  If        Eight_Pos
     C******Mdy********  Test(DE)                H8
     C*****************  If        Not %Error
     C******Mdy********  Move      H8            Asofdt
     C*****************  Eval      *In30     = *Off
     C*****************  Else
     C******Ymd********  Test(DE)                H8
     C*****************  If        Not %Error
     C******Ymd********  Move      H8            Asofdt
     C*****************  Eval      *In30     = *Off
     C*****************  Endif
     C*****************  Endif
     C*****************  Else
     C*****************  If        Ten_Pos
     C******Usa********  Test(DE)                Scasof
     C*****************  If        Not %Error
     C******Usa********  Move      Scasof        Asofdt
     C*****************  Eval      *In30     = *Off
     C*****************  Else
     C******Iso********  Test(DE)                Scasof
     C*****************  If        Not %Error
     C******Iso********  Move      Scasof        Asofdt
     C*****************  Eval      *In30     = *Off
     C*****************  Endif
     C*****************  Endif
     C*****************  Endif
     C*****************  Endif
      *****************
     C*****************  Else
      *****************
     C*****************  If        Eight_Pos
     C******Usa0*******  Test(DE)                H8
     C*****************  If        Not %Error
     C******Usa0*******  Move      H8            Asofdt
     C*****************  Eval      *In30     = *Off
     C*****************  Else
     C******Iso0*******  Test(DE)                H8
     C*****************  If        Not %Error
     C******Iso0*******  Move      H8            Asofdt
     C*****************  Eval      *In30     = *Off
     C*****************  Endif
     C*****************  Endif
     C*****************  Else
     C*****************  If        Six_Pos
     C******Mdy0*******  Test(DE)                H6
     C*****************  If        Not %Error
     C******Mdy0*******  Move      H6            Asofdt
     C*****************  Eval      *In30     = *Off
     C*****************  Else
     C******Ymd0*******  Test(DE)                H6
     C*****************  If        Not %Error
     C******Ymd0*******  Move      H6            Asofdt
     C*****************  Eval      *In30     = *Off
     C*****************  Endif
     C*****************  Endif
     C*****************  Endif
     C*****************  Endif
      *****************
     C*****************  Endif
      *****************
     C*****************  Endsr
      *** end del ***** 01/07/05 **************************************
      *** beg add ***** 01/07/05 **************************************
    C*================================================================
    C* Error Check Subroutine for SCR04
    C*================================================================
     c     errCkScr04    begsr
      *
      *    Date must be valid and a month end
      *
     c                   eval      scasof = dtCheck(dateTo8(cvtToDate(scasof)))

     c                   if        scasof = 0
     c                   seton                                        71  90
     c                   endif

     c  n90              eval      monthEnd = getLastDay(cvtToDate(scasof))

     c                   if        scasof <> dateto8(monthEnd)
     c                   seton                                        72  90
     c                   endif

     c  n90              do

     c                   select

     c                   when      scar1 <> *blank
     c                   eval      goodARcode = validCode(scar1)
     c                   eval      ix = 1

     c                   when      scar2 <> *blank
     c                   eval      goodARcode = validCode(scar2)
     c                   eval      ix = 2

     c                   when      scar3 <> *blank
     c                   eval      goodARcode = validCode(scar3)
     c                   eval      ix = 3

     c                   when      scar4 <> *blank
     c                   eval      goodARcode = validCode(scar4)
     c                   eval      ix = 4

     c                   when      scar5 <> *blank
     c                   eval      goodARcode = validCode(scar5)
     c                   eval      ix = 5

     c                   other
     c                   eval      goodARcode = *on
     c                   endsl

     c                   if        not(goodARcode)
     c                   eval      *in(80 + ix) = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   enddo

     c                   endsr
      *** end add ***** 01/07/05 **************************************
** Column headings
Opt  Group   Group Name                                            Status
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYN
207F7=Submit                YNN
212F12=Cancel               NNN
3
