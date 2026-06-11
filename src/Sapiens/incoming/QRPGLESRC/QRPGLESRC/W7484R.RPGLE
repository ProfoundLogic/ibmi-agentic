     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H*====================================================================
     H/title Import Response File prompt
     H*====================================================================
     H*  Created:07/16/09  C:198  Project: WC1819 Phase 3
     H*  Called by: Menu
     H*  Calls to : W7484C1
     H*====================================================================
     H* Given:
     H*   80 Character Folder name.  Example: CMS
     H*
     H* Return:
     H*   If at data center:  Folder name with prefix.
     H*             Example PDS/132/CMS
     H*
     H*   Otherwise: Return Folder name withouut suffix
     H*             Example CMS
     H*
     H*====================================================================
     H*  Revisions:
     H*
     H*  03/06/25 C:333  Project WC2998
     H*                  Changes to CMS Settlement Reporting.
     H*                  - Recompiled.
     H*
/* 07H* 05/03/13  C:190 Project: 114WEI
     H*                 Added prompt for the date.
     H*
/* 07H* 07/07/10  C:198 Project: WC1819 Priss 113
/*   H*                 fixed the issue where test file might override
/*   H*                 the file from production in CMS.
/*   H*
/* 01H* 01/07/10  C:220 Project: WC1819 Phase 5
/*   H*                 corrected the retrieval of folder name.
/*   H*
     H*
     H*====================================================================
     Fw7484f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflExtdate:rrnExtDate)
      /ENDIF
     Fssypf     if   e             disk
     F*** Beg Add *** 05/03/13 ***********************************
     Fwdd1l2    if   e           k disk    rename(pd101:pd102)
     F*** End Add *** 05/03/13 ***********************************
     d*** Beg Add *** 05/03/13 ***********************************
     d/copy sPrDate
     d*** End Add *** 05/03/13 ***********************************
     D*
     D*====================================================================
    D* Stand Alone Fields
     D*====================================================================
     Dfolder           s             80    inz('CMS')
     Dfullfolder       s             80
     Dauth             s              8    inz('        ')
     Dfolder_save      s             80
     Dprefix           s             80
     Dsuffix           s              3
     D ##CO#           s              3  0 inz
     D ##FND           s              3  0 inz
     D ##CONM          s             40a   inz
     D ##FNAM          s             40a   inz
     D IfsObj          s            256a   inz
     D IfsObjFnd       s               n   inz
     D xxcmd           s            200    inz(*blanks)
     D xxcmdln         s             15  5 inz(*zeros)
     D xxenv           s              1
      *** Beg Add *** 05/03/13 ***********************************
     d isError         s               n   inz(*off)
     d y               s              4  0 inz(*zeros)
     d z               s              4  0 inz(*zeros)
     d xxIndex         s              4  0 inz(*zeros)
     d kkDate          s              7  0
     d xxDate          s              8
     d xxDate1         s              8  0
      *** End Add *** 05/03/13 ***********************************
     D*====================================================================
     D* Constants
     D*====================================================================
     D comapi          c                   const('WA001R')
     D*
     D*====================================================================
     D* Data Structures
     D*====================================================================
     DtheSuffix        ds             3
     D syfl08                         1
     D syfl09                         1
     D syfl10                         1
      *** Beg Add *** 05/03/13 ***********************************
     d arrExtDate      s              7  0 dim(9999)
      *** End Add *** 05/03/13 ***********************************
     C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
      *** Beg Add *** 05/03/13 ***********************************
     c     keyWDD1L2     Klist
     c                   Kfld                    kkDATE
      *** End Add *** 05/03/13 ***********************************
    C*================================================================
    C*  Mainline
    C*================================================================
     C*
     C*
     C                   call      comapi
     C                   parm                    ##CO#
     C                   parm                    ##FND
     C                   parm                    ##CONM
     C                   parm                    ##FNAM
     C*
     C                   movel     ##CO#         xxco#             3
     C                   movel     folder        xxpds             4
     C*
     C                   Call      'SA030R'
     C                   parm                    suffix
     C                   parm                    prefix
     C*
     C                   call      'SRTEST'
     C                   parm                    xxenv
     C*
     C                   If        prefix <> *blanks
     C                   If        xxenv = 'T'
     C                   Eval      IfsObj = %trim(prefix) + '/TEST/' +
     C                                      %trim(folder)
     C                   Else
     C                   Eval      IfsObj = %trim(prefix) + '/' + %trim(folder)
     C                   EndIf
     C                   else
     C                   If        xxenv = 'T'
     C                   Eval      IfsObj = 'TEST/' +
     C                                      %trim(folder)
     C                   Else
     C                   Eval      IfsObj = %trim(folder)
     C                   EndIf
     C                   endif
     C*
     C                   Eval      fullfolder = IfsObj
     C*
     C*** check for folder
     C                   Call      'CHKIFSOBJ'
     C                   parm                    IfsObj
     C                   parm                    IfsObjFnd
     C*
     C                   If        not IfsObjFnd
     C*
     C                   eval      xxcmd= 'MKDIR DIR(''' +%trim(IfsObj)+ ''')'
     C                              + ' DTAAUT(*INDIR)'
     C                              + ' OBJAUT(*INDIR) CRTOBJAUD(*SYSVAL)'
     C                              + ' CRTOBJSCAN(*PARENT) RSTDRNMUNL(*NO)'
     C
     C*
     C                   eval      xxcmdln = %len(xxcmd)
     C*
     C                   Call      'QCMDEXC'
     C                   parm                    xxcmd
     C                   parm                    xxcmdln
     C                   endif
     C*
     C                   Eval      IfsObj = %trim(IfsObj) +
     C                                       '/' + 'IMPORT.TXT'
     C*
     C                   Call      'CHKIFSOBJ'
     C                   parm                    IfsObj
     C                   parm                    IfsObjFnd

      *** Beg Chg *** 05/03/13 ***********************************
     C********           If        not IfsObjFnd
     C********           exfmt     SRC02#
     C********           else
     C********           exfmt     SRC01#
      /free
       If not IfsObjFnd;
          Exfmt SRC02#;
       Else;
          Dou BtnEnter = *On or BtnExit = *On;
            Dou isError = *off or BtnExit = *On;
               Exfmt SRC01#;
               isError = *off;
               msDate1=' ';
               erDate1=*Off;
               erDate = *off;
               msDate = ' ';
               If BtnDateSrc = *On;
                  DoU BtnCancel = *On or btnselect = *On;
                     Exsr LoadExtDate;
                     Exfmt RcdExtDate;
                     ReadC sflExtDate;
                     If not %eof and btnselect = *On;
                        scDate = scExtDate;
                     Else;
                        scExtDate = 0;
                     Endif;
                  Enddo;
               Endif;
               If scDate <> 0;
                  Exsr ErrorCheck;
               Endif;
            Enddo;
          Enddo;
          xxDate  = ' ';
          xxDate1 = 0;
          xxDate1 = dtYYMD(scDate);
          xxDate = %editc(xxDate1:'X');
      /end-free
      *** End Add *** 05/03/13 ***********************************
     C*
     C                   movel     IfsObj        fullfolder
     C*
     C                   If        not *inkc
     C                   Call      'W7484C1'
     C                   parm                    xxco#
     C                   parm                    fullfolder
      *** Beg Add *** 05/03/13 ***********************************
     C                   parm                    xxDate
      *** End Add *** 05/03/13 ***********************************
     C                   endif
     C*
     C                   endif                                                  object is found
     C*
     C                   seton                                        lr
      *** Beg Add *** 05/03/13 ***********************************
      *** Validate Date.
      /free
       Begsr ErrorCheck;
       // ** Date must be valid if entered.
          isError = *off;
          msDate1=' ';
          erDate1=*Off;
          erDate = *off;
          msDate = ' ';
          If dtCheck(scDate)=0;
             iserror=*On;
             erDate1=*On;
             msDate1='Invalid Date';
             LeaveSr;
          Endif;

          kkDate = dtCYMD(scDate);
          Chain kkDate pd102;
          If not %found(wdd1l2);
             isError= *on;
             erDate = *on;
             msDate = 'Date does not match Medicare Extract Log File';
             scDate =dtCheck(scDate);
          Endif;

          Setll keyWDD1L2 pd102;
          Reade keyWDD1L2 pd102;
          Dow not %eof;
             If D1rsdt = 0;
                scDate =dtCheck(scDate);
                Leave;
             Endif;
             If %eof(wdd1l2) and D1rsdt <> 0;
                isError= *on;
                erDate = *on;
                msDate = 'Extract already ran on this date';
                scDate =dtCheck(scDate);
             Endif;
             Reade keyWDD1L2 pd102;
          Enddo;
       Endsr;
      /end-free

      ************************************************************************************
      *** Load Extract Date.
      /free
       BegSr LoadExtDate;

       //** Populate date array with exract dates having response date = 0(Distinct records).
          setgt 9999999 pd102 ;
          Readp pd102;
          DoW not %Eof;
              If D1rsdt = 0;
                 xxIndex = %Lookup(D1exdt : arrExtDate);
                 If xxIndex = 0;
                    z = z + 1;
                    arrExtDate(z) = D1exdt;
                 Endif;
              Endif;
              Readp PD102;
          Enddo;

       // Populate grid with Extract dates.
       // Clear the subfile.
          clrExtDate = *On;
          dspExtDt = *Off;
          Write rcdExtDate;
          clrExtDate = *Off;
          dspExtDt = *On;
          rrnExtDate = 0;

          y = 1;
          DoW  y <= 9999;
            Clear scExtDate;
            If arrExtDate(y) = 0;
              Leave;
            Endif;
            scExtDate = dtmdyy(arrExtDate(y));
            y = y + 1;
            rrnExtDate =  rrnExtdate + 1;
            Write sflExtDate;
          Enddo;
          rrnExtDate = 1;

       EndSr;
      /end-free
      *** End Add *** 05/03/13 ***********************************
