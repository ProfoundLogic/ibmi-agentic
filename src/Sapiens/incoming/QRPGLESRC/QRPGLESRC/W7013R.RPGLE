     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H*****************************************************************
     H/TITLE W7013R - EFT Download Process                            *
     H option(*nodebugio:*srcstmt)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION: EFT Download Process                     *
     H*                                                               *
     H* CREATED: 08/09/07  C:191 PROJECT WC1453                       *
     H* REWRITTEN 06/29/09 BY C:191 PROJECT: WC1806
     H*****************************************************************
     H* REVISIONS:                                                    *
    H*                                                               *
    H* 03/21/25 C:079 Support: 152/118722                            *
    H*                Recompile and install to fix Non Group VPay    *
     H*                Issue                                          *
     H*                                                               *
     H*  08/27/20 C:062 Project: WC2749                               *
     H*                 Add E-Pay Processing                          *
     H*                                                               *
     H*  04/17/12 C:255 Project: WC2044                               *
     H*                 Modify program to use EFT flag instead of EFT *
     H*                 flag instead of EFT in bank code.             *
     H*                                                               *
     H*  06/29/09 C:191 Project: WC1806                               *
     H*                 RE-WRITE taking parts from original program   *
     H*                 Change to combine with ACH process.           *
     H*                 Now calls WA073R to write EFT to WAEFP        *
     H*                                                               *
     H*  07/01/09 C:191 Support: WC025 68967                          *
     H*                 CLT025 requested to back out changes for      *
     H*                 68967 (06/26/09)                              *
     H*                                                               *
     H*  06/26/09 C:227 Support: WC025 68967                          *
     H*                 Prefix '1' in origin id.                      *
     H*                                                               *
     H*  04/16/09 C:191 Support: WA090 67892                          *
     H*                 Fix date format                               *
     H*                                                               *
     H*  04/02/09 C:136 Support: WA025 67192                          *
     H*                 Additional changes required for xxprenot      *
     H*                 processing.                                   *
     H*                                                               *
     H*  05/25/07 C:191 PROJECT: WC1453 Priss #08                     *
     H*                          Add processing for Pre-notification  *
     H*  12/05/08 C:025 PROJECT: 025WOO                               *
     H*                          ACH File Format Adjustments          *
     H*                                                               *
     H*****************************************************************
    F*
     F*  User id file definition (for getting user param)
     Fwmupl     if   e           k disk
    F*
    F*  Logical file definition
     Fwmefl3    if   e           k disk
     F                                     rename(pef01:pef03)
     F                                     infds(dbfds3)
     Fwmefl2    if   e           k disk
     F                                     rename(pef01:pef02)
     F                                     infds(dbfds2)
     Fwdpal     if   e           k disk
     F                                     infds(dbfds)
    F*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------
     D @@bdt           s              8A                                        Batch Date
     D xxbdt           s              7  0 inz                                  Batch Date
     D xxco#           s              3  0 inz                                  Company Number
     D xxapprvd        s               N   inz                                  Approved flag
     D xxcleanup       s               N   inz
     D xxrrn           s              9  0 inz
     D*
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D*
     D kkco#           s              3  0
     D kkfnd           s              3  0
     D kkfyr           s              3  0
     D kkcase          s              7  0
     D kkdsq#          s              5  0
     D kkfein          s              9  0
     D kkmod           s              3  0
    D*---------------------------------------------------------------------
    D* Data Structures
    D*---------------------------------------------------------------------
      *
     D dbfds           ds
     D  dbrrn                397    400b 0
      *
     D dbfds3          ds
     D  dbrrn3               397    400b 0
      *
     D dbfds2          ds
     D  dbrrn2               397    400b 0
      *
     D psds          esds                  extname(szq1p)
    D*
     D                 DS
     Ddsdttm                         14  0
     Ddstime                          6  0 overlay(dsdttm)
     Ddsdate                          8  0 overlay(dsdttm:7)
    D*---------------------------------------------------------------------
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     efky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkdsq#
      *
     C     efky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfein
     C                   kfld                    kkmod
      *
      **********************************************************************************************
      * MAINLINE
      **********************************************************************************************
     C*
     C     xxco#         setll     ppa01
     C     xxco#         reade     ppa01
      *
     C                   dow       not %eof(wdpal)
      *
      * Process records where: a) Trasaction date = batch date, and
      *                        b) bank code = 'EFT'  , and
      *                        c) payment type = 'P'
      *
       //*** Begin Chg *** 08/27/20 **********************************
       //*** If (PAtrdt = XXbdt) And (PAeftc = 'E') And
       //***  (PAtype = 'P');
       If (PAtrdt = XXbdt) And
        (PAeftc = 'E' Or (PAeftC = 'V' And PApgTp <> 'G')) And
        (PAtype = 'P');
       //*** End   Chg *** 08/27/20 **********************************
      *
      * Check approval date from EFT information file
     C                   exsr      srchkapr
      *
     C                   if        xxapprvd
      * Approved record found, process payment record
     C                   exsr      srprcrec
     C                   endif                                                  xxapprvd
     C                   endif                                                  patrdt
      *
     C     xxco#         reade     ppa01
     C                   enddo
      *
     C                   if        xxcleanup
      * Call API to finally cleanup
     C                   call      'WA073R'
     C                   parm      'X'           ##type
     C                   parm      *zeros        ##rrn
     C                   parm      *zeros        ##rrn1
     C                   parm      ' '           ##rtn
     C                   endif                                                  xxcleanup
      *
     C                   eval      *inlr = *on
      *
      **********************************************************************************
      * srchkapr - Check approval date for EFT processing                              *
      **********************************************************************************
      *
     C     srchkapr      begsr
     C*
     C                   eval      xxapprvd = *off
     C*** Begin Add *** 08/27/20 *******************************************
       If PAeftC = 'V';
          xxApprvd = *On;
       Else;
     C*** End   Add *** 08/27/20 *******************************************
      * if fein = 0 then assume claimant, else payee
     C                   if        pafein = 0
     C* Access records using claim number
     C                   eval      kkco# = xxco#
     C                   eval      kkfnd = pafnd
     C                   eval      kkfyr = pafyr
     C                   eval      kkcase= pacase
     C                   eval      kkdsq#= padsq#
     C     efky1         chain     pef03                              60
     C                   eval      xxrrn = dbrrn3
     C                   else
     C* Access records using fein/mod
     C                   eval      kkco# = xxco#
     C                   eval      kkfein= pafein
     C                   eval      kkmod = pamod
     C     efky2         chain     pef02                              60
     C                   eval      xxrrn = dbrrn2
     C                   endif                                                  pafein = 0
      *
     C                   dow       not *in60
     C* find effective record, i.e. batch date in between effective and expiry dates
     C                   if        xxbdt >= efefdt and
     C                             (xxbdt < efexdt or efexdt = 0)
     C                   eval      xxapprvd = *on
      * effective record found leave loop
     C                   leave
     C                   endif                                                  xxbdt>= efefdt
      *
     C                   if        pafein = 0
     C     efky1         reade     pef03                                  60
     C                   eval      xxrrn = dbrrn3
     C                   else
     C     efky2         reade     pef02                                  60
     C                   eval      xxrrn = dbrrn2
     C                   endif                                                  pafein=0
     C                   enddo                                                  *in60
     C*** Begin Add *** 08/27/20 *******************************************
       EndIf;
     C*** End   Add *** 08/27/20 *******************************************
      *
     C                   endsr
      *
      **********************************************************************************
      * srprcrec - Process Payment record                                             *
      **********************************************************************************
      *
     C     srprcrec      begsr
     c*
      *
      * Call API to write to EFT central holding file API
     C                   call      'WA073R'
     C                   parm      'C'           ##type            1
     C                   parm      dbrrn         ##rrn             9 0
     C                   parm      xxrrn         ##rrn1            9 0
     C                   parm      ' '           ##rtn             1
      *
     C                   eval      xxcleanup = *on
      *
     C                   endsr
      *
      **********************************************************************************
      *                             Initialisation Subroutine                          *
      **********************************************************************************
     C     *inzsr        begsr
     C* get company no. from user parameter
     C     *entry        plist
     C                   parm                    @@bdt
     C*
     C* convert batch date to correct format
     C                   move      @@bdt         srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxbdt
     C*
     C* Get Company number
     C     q1user        chain     pup01
     C                   if        %found
     C                   eval      xxco# = upco#
     C                   endif                                                  %found
     C*
     C                   endsr
