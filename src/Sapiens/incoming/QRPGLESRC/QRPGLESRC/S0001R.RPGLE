     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('STBNDDIR')
    H**********************************************************************************************
    H/TITLE - Company Maintenance
    H**********************************************************************************************
    H*  DESCRIPTION:                                                                              *
    H*    List all companies.                                                                     *
    H*                                                                                            *
    H*  CALLED FROM:                                                                              *
    H*    MENU                                                                                    *
    H*                                                                                            *
    H*  CALLS TO:                                                                                 *
    H*                                                                                            *
    H*  REWRITE: 10/23/02      C:136     Support: WA099 31785                                     *
    H**********************************************************************************************
     H* REVISIONS:                                                                                 *
     H*
     H* 11/27/18 C:33  Project: SY406
     H*                Add Option 75 = Template Associations
     H*                                                               *
     H* 06/01/11 C:53  Project: 25WPW
     H*                Add Option 28 = TX Min/Max Benefits             flags 23.
     H*
     H* 04/14/10  C:62  Project: 25WQK  Priss #31                     *
     H*                -Only show option 9 if client 114.             *
     H*                                                               *
     H* 04/09/10  C:33  Project: 25WQK  Priss #30                     *
     H*                -Add contact heading option                    *
     H*                -Change # used for JDE                         *
     H*                                                               *
     H* 11/17/09 C:191 PROJECT: 25WQI                                 *
     H*                JDE - Detailed Transaction Download            *
     H*                New option for JDE Control Program
     H*
     H* 06/25/09 C:220 PROJECT: 114WTR                                *
     H*                Interface with Zurich                          *
     H*
     H* 03/28/08 C:65  PROJECT: CI019                                 *
     H*                ENABLE OPTION 26 FOR ACH IF ONLINE PROCESSING. *
     H*                                                                                            *
     H* 06/11/07 c:61  Project: WC1504                                                             *
     H*                Add options "30=WCPOLS Default"; "31=WCSTAT Default".                       *
     H*                                                                                            *
     H* 04/11/03 c:136 Project: WC1116                                                             *
     H*                Add Option 26 = ACH Control Info dependent on system flag 24.               *
     H*                                                                                            *
     H* 02/18/03 C:136 Project: WC1115                                                             *
     H*                Add Option 10 = W/W Vendors dependent on system flags 23.                   *
    H**********************************************************************************************
     Fs0001f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fsmcop     if   e             disk
     F                                     rename(pco01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fsmcol     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pco01:dbrcdf)
      *** beg add ***** 02/18/03 **********************************
      * Sytem application file
     Fssylf     if   e           k disk
      *** end add ***** 02/18/03 **********************************
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
      ******* beg chg ***** 06/11/07 ***************************************************************
      *** beg chg ***** 02/18/03 **********************************
     D*of******        s              1    dim(12) ctdata perrcd(1)
     D*ofd*****        s             35    dim(12) alt(of)
      *** beg chg ***** 06/25/09 **********************************
     D*of******        s              1    dim(14) ctdata perrcd(1)
     D*ofd*****        s             35    dim(14) alt(of)
     D*** BEGIN CHG ****** 11/17/09 ************************************
     D********* of              s              1    dim(17) ctdata perrcd(1)
     D********* ofd             s             35    dim(17) alt(of)
     D*** Begin Chg  ***  04/09/10  ***************************************
     D*of******        s              1    dim(18) ctdata perrcd(1)
     D*ofd*****        s             35    dim(18) alt(of)
     D#ofOFs           c                   25
     D of              s              1    dim(#ofOFs) ctdata perrcd(1)
     D ofd             s             35    dim(#ofOFs) alt(of)
     D*** End   Chg  ***  04/09/10  ***************************************
     D*** END   CHG ****** 11/17/09 ************************************
      *** end chg ***** 06/25/09 **********************************
      ******* end chg ***** 06/11/07 ***************************************************************
      *** end chg ***** 02/18/03 **********************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Companies'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'SYMESG'
     D acdpgm          c                   'S0001R02'
     D comapi          c                   'SA005R'
     D uspapi          c                   'S0006R'
     D infomx          c                   1
     D viewmx          c                   1
      ******* beg chg ***** 06/11/07 ***************************************************************
     D*sf1pag********  c                   12
     D sf1pag          c                   11
      ******* END chg ***** 06/11/07 ***************************************************************
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
     D @@prds          s             40
     D @@call          s              1
     D kkco#           s                   like(coco#)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     D***  Begin Add  ***  11/27/18  *************************************
      /copy WITMPLTD
      /copy sPrModule
     D***  End   Add  ***  11/27/18  *************************************
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@prds
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
     C                   parm                    ##conm
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
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
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkco# = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     c                   move(p)   scpst1        kkco#
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kkco# = scco#
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   eval      scco# = COCO#
     C                   eval      sccnam = CONAME
     C                   endsr
      *** beg add ***** 06/25/09 **********************************
    C*================================================================
    C* OPT08 -  TPA Contact Information
    C*================================================================
     C     opt08         begsr
     c                   move      scco#         ##co#
     c                   call      'W0009R'
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm                    ##co#
     c                   parm      @@call        ##call            1
     c                   parm      scdbrr        @@rrn             9 0
     C                   endsr
    C*================================================================
    C* OPT09 -  Company Contact Information
    C*================================================================
     C     opt09         begsr
     c                   move      scco#         @@co#             3
     c                   call      'S0012R02'
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm                    ##mode
     c                   parm      @@call        ##call
     c                   parm      scdbrr        ##rrn
     c                   parm                    @@co#
     C                   endsr
      *** end add ***** 06/25/09 **********************************
      *** beg add ***** 02/18/03 **********************************
    C*================================================================
    C* OPT10 - Call W/W Vendors
    C*================================================================
     C     opt10         begsr
     c                   call      'W0003R'
     c     *inkc         parm                    ##kc              1
     c     *inkl         parm                    ##kl              1
     c                   parm      @@call        ##call            1
     c                   parm      scdbrr        ##rrn             9 0
     C                   endsr
      *** end add ***** 02/18/03 **************************************
     C***  Begin Add  ***  04/09/10  **************************************
     C*================================================================
     C* Contact Headings
     C*================================================================
     C     opt25         begsr
     C*
     C                   call      'S0225R'
     C                   parm      @@call        ##call
     C     *inkc         parm      *blanks       ##kc
     C     *inkl         parm      *blanks       ##kl
     C                   parm      scCo#         ##Co#
     C*
     C                   endsr
     C***  End   Add  ***  04/09/10  **************************************
     C*
     C*** BEGIN ADD ****** 11/17/09 ************************************
    C*================================================================
    C* JDE Control information
    C*================================================================
     C***  Begin Chg  ***  04/09/10  **************************************
     C*****opt25         begsr
     C     opt27         begsr
     C***  End   Chg  ***  04/09/10  **************************************
     c                   if        @@call = '1'
     c                   eval      ##mode = 'D'
     c                   else
     c                   eval      ##mode = ' '
     c                   endif
     c*
     c                   call      'W9210R'
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm                    ##mode            1
     c                   parm      scdbrr        ##rrn
     C                   endsr
     C*** END   ADD ****** 11/17/09 ************************************
      *** beg add ***** 04/11/03 **************************************
    C*================================================================
    C* OPT26 - ACH Control Info
    C*================================================================
     C     opt26         begsr
     c                   call      'W0005R'
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm                    ##mode            1
     c                   parm      scdbrr        ##rrn
     C                   endsr
      *** end add ***** 04/11/03 **************************************
      ******* beg add ***** 06/11/07 ***************************************************************

      *** beg add ***** 06/01/11 **************************************
    C*================================================================
    C* OPT28 -
    C*================================================================
     C     opt28         begsr
     c                   call      'W0591R'
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm                    ##mode            1
     c                   parm      scdbrr        ##rrn
     C                   endsr
      *** end add ***** 06/01/11 **************************************

      *================================================================
      * OPT30 - WCPOLS Default Transmittal Information
      *================================================================
     c     opt30         begsr

     c                   select
      * add/change
     c                   when      @@call = *blank
     c                   eval      #1mode = 'C'
      * display
     c                   when      @@call = '1'
     c                   eval      #1mode = 'D'

     c                   endsl

     c                   call      'W0008R'
     c                   parm      *blank        #1kc              1
     c                   parm      *blank        #1kl              1
     c                   parm                    #1mode            1
     c                   parm      scdbrr        #1rrn             9 0
     c                   parm      'P'           #1call            1

     C                   endsr


      *================================================================
      * OPT31 - WCSTAT Default Transmittal Information
      *================================================================
     c     opt31         begsr

     c                   select
      * add/change
     c                   when      @@call = *blank
     c                   eval      #1mode = 'C'
      * display
     c                   when      @@call = '1'
     c                   eval      #1mode = 'D'

     c                   endsl

     c                   call      'W0008R'
     c                   parm      *blank        #1kc
     c                   parm      *blank        #1kl
     c                   parm                    #1mode            1
     c                   parm      scdbrr        #1rrn
     c                   parm      'S'           #1call            1

     c                   endsr


      ******* end add ***** 06/11/07 ***************************************************************
     c***  Begin Add  ***  11/27/18  *************************************
    C*================================================================
    C* Opt75 - Process other function keys
    C*================================================================
      /Free
       Begsr Opt75;
        Select;
        When @@call = ' ';
           #1Mode = 'C';
        Other;
           #1Mode = 'D';
        Endsl;
        Reset dsWITMPLTparms;
        dsWITMPLTparms.co# = scCo#;
        TemplateAssociations(dsWITMPLTparms:'Company':#1Mode);
       Endsr;
      /End-free
     c***  End   Add  ***  11/27/18  *************************************
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
      ******* beg add ***** 06/25/09 ***************************************************************
     c     scsel         caseq     '8'           opt08
     c     scsel         caseq     '9'           opt09
      ******* end add ***** 06/25/09 ***************************************************************
     c*** BEGIN ADD ****** 11/17/09 ************************************
     c     scsel         caseq     '25'          opt25
     c*** END   ADD ****** 11/17/09 ************************************
      *** beg add ***** 04/11/03 **************************************
     c     scsel         caseq     '26'          opt26
      *** end add ***** 04/11/03 **************************************
     c***  Begin Add  ***  04/09/10  **************************************
     c     scsel         caseq     '27'          opt27
     c***  End   Add  ***  04/09/10  **************************************
      *** beg add ***** 06/01/11 **************************************
     c     scsel         caseq     '28'          opt28
      *** end add ***** 06/01/11 **************************************
      *** beg add ***** 02/18/03 **************************************
     c     scsel         caseq     '10'          opt10
      ******* beg add ***** 06/11/07 ***************************************************************
     c     scsel         caseq     '30'          opt30
     c     scsel         caseq     '31'          opt31
      ******* end add ***** 06/11/07 ***************************************************************
     c***  Begin Add  ***  11/27/18  *************************************
     c     scsel         caseq     '75'          opt75
     c***  End   Add  ***  11/27/18  *************************************
     c                   endcs
      *** end add ***** 02/18/03 **************************************
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

      *** beg add ***** 02/18/03 **************************************
     c                   movel     'SY'          sykey             2
     C     sykey         chain     ssy01
     c                   if        %found and
     c                             sywcif = 'Y'

     c                   movel     'WC'          wckey             2
     C     wckey         chain     swc01
     c                   if        %found and
     c                             wcfl23 = 'Y'
     c                   eval      xxopfn = '1'
     c                   eval      xxnmbr = '10'
     c                   eval      xxmis  = 'YYN'
     c                   exsr      updofa
     c                   endif

      *** beg add ***** 04/11/03 **************************************
      *** beg del ***** 08/27/10 **************************************
     c***********        if        %found and
     c***********                  wcfl24 = 'Y'
     c****begin*add ***  03/28/08  ************************************
     c***********                  or %found and wcfl38 = 'Y'
     c****end*add *****  03/28/08  ************************************
     c***********        eval      xxopfn = '1'
     c***********        eval      xxnmbr = '26'
     c***********        eval      xxmis  = 'YYN'
     c***********        exsr      updofa
     c***********        endif
      *** end del ***** 08/27/10 **************************************
      *** end add ***** 04/11/03 **************************************

     c                   endif
      *** end add ***** 02/18/03 **************************************
     c*** BEGIN ADD ****** 11/17/09 ************************************
     c* Retrieve client id for client specific processing
     c                   call      'RTVCLTID'
     c                   parm                    cltid             3
     c** Enable TASB only options
     c                   if        cltid = '025'
     c                   eval      xxopfn = '1'
     c                   eval      xxnmbr = '25'
     c                   eval      xxmis  = 'YYN'
     c                   exsr      updofa
     c***  Begin Add  ***  04/09/10  **************************************
     c                   eval      xxopfn = '1'
     c                   eval      xxnmbr = '27'
     c                   eval      xxmis  = 'YYN'
     c                   exsr      updofa
     C                   eval      xxopfn = '1'
     C                   eval      xxnmbr = '28'
     C                   eval      xxmis  = 'YYN'
     C                   exsr      updofa
     c***  End   Add  ***  04/09/10  **************************************
     c                   endif
     c*** END   ADD ****** 11/17/09 ************************************
     c***  Begin Add  ***  04/14/10  **************************************
     c                   if        cltid <> '114'
     c                   eval      xxopfn = '1'
     c                   eval      xxnmbr = '09'
     c                   eval      xxmis  = 'NNN'
     c                   exsr      updofa
     c                   endif
     c***  End   Add  ***  04/14/10  **************************************
     c***  Begin Add  ***  11/27/18  *************************************
     c* Opt 75 is only allowed if module installed
      /Free
       If isModEnabled('Portal Module');
          xxOpFn = '1';
          xxNmbr = '75';
          xxMIS  = 'YYN';
          Exsr UpdOfA;
       Endif;
      /End-free
     c***  End   Add  ***  11/27/18  *************************************

     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C                   read      dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C                   readp     dbrcdf                                 95
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
** Column headings
Opt  Co#   Company Name
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1066=                       NNN
1088=TPA Contact InformationYYN
1099=Company Contact Info   YYN
11010=Vendors               NNN
12525=Contact Headings      NNN                            04/09/10
12626=ACH Control Info      NNN
12727=JDE Controls          NNN
12828=TX Min/Max Benefits   NNN
13030=WCPOLS Default        YYN
13131=WCSTAT Default        YYN
17575=Template Associations NNN                            11/27/18
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               NNN
3
