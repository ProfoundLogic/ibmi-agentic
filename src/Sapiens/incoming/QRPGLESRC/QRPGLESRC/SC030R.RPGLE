     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     *===================================================================*
    H/TITLE SC030R - DARTS SECURITY MAINTENANCE PROGRAM
     *===================================================================*
     * Description: This program is the entry point for the maintenance  *
     *              of the DARTS Security information.                   *
     *                                                                   *
     * Called from: Menu, W0005R Work With User Parameters               *
     *                                                                   *
     * Calls to   : SC031R02 - DARTS Security Maintenance                *
     *                                                                   *
     * Created    : 12/04/99                                             *
      * Programmer : C:113                                                *
      * Project    : DR079                                                *
     *===================================================================*
     * REVISIONS:                                                         *
      *
      * 05/30/17  C:33   Support 47421
     H*                  Pass-thru to STUSRPRM for anything but       *
     H*                   selection mode.                             *
      *
      * 01/28/10  C:XXX  Project: SY215                               *
      *           C:189  Change Brand Name
      *
      * 03/21/01  C:062  PROJECT: PRMNT
      *                  Swap out WA001R with SA005R.
      *
      * 05/15/00  C:118  PROJECT: WC577
      *                  Modify to receive mode into @@call
      *
      * 08/09/00  C:128  PROJECT: SY041
      *                  Modify to call new search program that displays
     *                  users of all status's.                           *
     *===================================================================*
     FSC030F    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF

      ** DARTS Security File
     FSMSLL     if   e           k disk

     *===================================================================*
     *  Data Structure Definitions
     *===================================================================*
     ** External data structures
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
    
      ** Date and time
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     *===================================================================*
     *  Field Definitions
     *===================================================================*
     D*** BEGIN CHG *********** 01/28/10 ************************************
     D***pdsnam        c                   const('Progressive Data So-
     D***                                  lutions, Inc.')
     D pdsnam          C                   const('Stoneriver, Inc')
     D*** BEGIN CHG *********** 01/28/10 ************************************
     D ##conm          s             40
     D ##data          s             80
     D ##leng          s              3  0
     D f3reqd          s              1
    
     D acdpgm          c                   const('SC031R02')
     D optnm           c                   const('DARTS Security Maintenance')
     d*** begin chg *** 03/21/01 *******************************************
     D*comapi          c                   const('WA001R')
     D comapi          c                   const('SA005R')
     d*** end   chg *** 03/21/01 *******************************************
     D @@user          s             10
     D @@sec           s              3
     D @@desc          s             40
     D @@call          s              1
     D qqcall          s                   like(@@call)
     D qqrtc           s                   like(@@call)
     D***  Begin Add  ***  05/30/17  **************************************
     D ##call          s              1
     D***  End   Add  ***  05/30/17  **************************************
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D***  Begin Add  ***  01/28/10  **************************************
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
     D***  Begin Add  ***  05/30/17  **************************************
     d StUsrPrms       pr                  Extpgm('STUSRPRM')
     d  outCall                       1a
     d  outAvlMods                   50a   const
     D***  End   Add  ***  05/30/17  **************************************
    
     *===================================================================*
     *  PLIST definitions
     *===================================================================*
      ** Add/Change program parameters
     C     acdpl         plist
     C                   parm                    scusr
     C                   parm                    @@sec
     C                   parm                    @@desc
     C                   parm                    qqcall
    
     ** Company file/fund (group) retrieve program parameter list
     C     compl         plist
     C                   parm                    ##co#
     C*** begin del *** 03/21/01 **************************************
     C**********         parm                    ##fnd
     C*** end   del *** 03/21/01 **************************************
     C                   parm                    ##conm
     C*** begin del *** 03/21/01 **************************************
     C**********         parm                    ##fnam
     C*** end   del *** 03/21/01 **************************************
    
      ** Date Check/Conversion parameters
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr

      ** Entry parameters
     C     *entry        plist
     C                   parm                    @@user
     C                   parm                    @@sec
     C                   parm                    @@desc
     C                   parm                    @@call

     *===================================================================*
     *  KLIST definitions
     *===================================================================*
     ** DARTS Security File
     C     slkey         klist
     C                   kfld                    @@user

     *===================================================================*
     * Main
     *===================================================================*
     C                   move      *off          f3reqd
     C                   eval      *inkc = *off
     C                   eval      *inkl = *off
     C                   eval      *in90 = *on
    
     C                   exsr      getcof
     C                   exsr      pgmstr
    
B01  C                   dow       *inkc <> *on
     C                             and *inkl <> *on
     C                             and *in90 <> *off
     C                   exsr      predsp
     C                   exfmt     scr01
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)

B02  C                   if        *inkc = *off
     C                             and *inkl = *off
     C                   exsr      errck1
B03  C                   if        *in90 = *off
     C                   exsr      finish
E03  C                   endif
E02  C                   endif

E01  C                   enddo
    
     C                   exsr      pgmend
    
     C                   eval      *inlr = *on
     *===================================================================*
     *  FINISH - Final processing for file update/display
     *===================================================================*
     C     FINISH        begsr

     C                   exsr      prsctl

B01  C                   if        f3reqd = *on
     C                   move      *on           *in90
E01  C                   endif

     C                   endsr
     *===================================================================*
     *  GETCOF - Gets company and fund names if possible; otherwise
     *           get PDS name for company name
     *===================================================================*
     C     GETCOF        begsr

     C                   move      *on           *in91
     C                   exsr      dscapi
B01  C                   if        *in91 = *on
     C***  Begin Chg  ***  01/28/10  **************************************
     C**********         movel(p)  pdsnam        ##data
     C                   callp     RtvBrnd('I':brandName)
     C*
     C                   eval      ##data  = brandname
     C***  End   Chg  ***  01/28/10  **************************************
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        ##conm
E01  C                   endif
     C                   movel     ##conm        scconm

     C                   endsr
     *===================================================================*
     * DSCAPI - Call API for getting descriptions (use 91 on error)
     *===================================================================*
     C     DSCAPI        begsr
    
     C                   call      comapi        compl                  91
    
     C                   endsr
     *===================================================================*
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *===================================================================*
     C     ERRCK1        begsr
    
     C                   exsr      othfks

     C                   if        *in90 = *off
     C                             and scusr = *blanks
     C                   seton                                        71  90
     C                   endif

     C                   if        *in90 = *off
     C                   call      acdpgm        acdpl

     C                   if        qqcall = 'C'
     C                   eval      *inlr = *on
     C                   else
     C                   eval      *in90 = *on
     C                   eval      scusr = *blanks
     C                   endif

     C                   endif

     C                   endsr
     *===================================================================*
     * KJDO   - Processing for F10
     *===================================================================*
     C     KJDO          begsr

      *** Begin Chg 08/09/00 **********************************************
     C******             call      'SC530R'
     C                   call      'SC530R01'
      *** End   Chg 08/09/00 **********************************************
     C                   parm                    scusr
     C                   parm                    qqrtc

     C                   endsr
     *===================================================================*
     * OTHFKS - Processing for other functions keys
     *===================================================================*
     C     OTHFKS        begsr
    
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
    
     C                   endsr
     *===================================================================*
     * PGMEND - End of program processing
     *===================================================================*
     C     PGMEND        begsr
     C                   endsr
     *===================================================================*
     * PGMSTR - Used for any start of program processing (one-time)
     *===================================================================*
     C     PGMSTR        begsr
     C***  Begin Add  ***  05/30/17  **************************************
      /Free
       Select;
       // Continue with W/W
       When %parms >= 1 and @@call = 'O';
          @@call = ' ';
       // Call new user parm program
       When %parms < 1 or @@call <> '2';
          Clear ##call;
          If %parms >= 1;
             ##call = @@call;
          Endif;
          Monitor;
             StUsrPrms(##call:'DARTS');
             Return;
          On-error;
          // If there is an issue with STUSRPRM, continue with W/W
          Endmon;
       Endsl;
      /End-free
     C***  End   Add  ***  05/30/17  **************************************
    
     C                   movel(p)  optnm         ##data
    
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
    
     C                   movel     ##data        scprds
    
     C                   movel     @@call        qqcall
    
     C***** begin chg **** 05/15/00 ****************************************
     C**********         if        qqcall = '3'
     C     @@user        ifne      *blanks
     C***** end   chg **** 05/15/00 ****************************************
     C                   eval      scusr = @@user
     C                   call      acdpgm        acdpl
     C                   eval      *in90 = *off
     C                   eval      *inlr = *on
     C                   endif

     C                   endsr
     *===================================================================*
     * PREDSP - Pre-processing before displaying (every time)
     *===================================================================*
     C     PREDSP        begsr
     C                   endsr
     *===================================================================*
     * PRSCTL - Process control after no errors found (Call Submitter)
     *===================================================================*
     C     PRSCTL        begsr
     C                   endsr
