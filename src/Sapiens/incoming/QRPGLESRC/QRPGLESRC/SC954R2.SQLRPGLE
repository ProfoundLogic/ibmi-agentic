    H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
    H*********************************************************************
    H/TITLE SC954R2 - DARTS deletion remarks
     h DFTACTGRP(*no) ACTGRP(*caller)
     h option(*nodebugio: *srcstmt)
    H*********************************************************************
    H*
    H* Created on 03/15/2018 by c:62 for Project 114IWR (Priss 48148)
    H*********************************************************************
    H* Revisions:
    H*
    H*  06/14/23  C340  Project: 114WRJ (CSI)
    H*                  Create Cursor Based on inDartType
    H*                  If inDartType = 'W0040R', then do not include
    H*                  Group on Where Clause.
    H*********************************************************************
      * DARTS transfer history
     fsxdtp     o  a e           k disk    Rename(pdt01:pdt0p)
      *
     d*==========================================================
     d* Work fields
     d*==========================================================
     d inGroup         s              3
     d inFromDt        s              7
     d inToDate        s              7
     d inDartType      s             10
     d dCount          s              6s 0 Inz(*Zero)
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
     d strSqlDarts     s           1000
     d isDeleteDart    s               N   Inz(*On)
     d intCount        s              6s 0 Inz(*Zero)
     *** End Add *** 06/14/23 *** 114WRJ ****************************

     d*==========================================================
     d* Prototypes
     d*==========================================================
     d* Delete SMARP records - Legacy Additional Remarks
     d DltSMARP        pr

     d* Delete SMDLP records - Document Location file
     d DltSMDLP        pr

     d* Delete SMMDP records - Multi-Object File
     d DltSMMDP        pr

     d* Delete IMDOP records - Document Object File
     d DltIMDOP        pr

     d* Delete IMDOP1 records - Indexed Document Object File
     d DltIMDOP1       pr

     d* Delete SMCLP records - iDarts Additional Remark File
     d DltSMCLP        pr

     d* Delete SDAIP records - Darts Additional Information File
     d DltSDAIP        pr

     d* Delete SDCMP records - Darts Master File
     d DltSDCMP        pr

     d* Write Darts Deletion Record
     d WrtSXDTP        pr

     D dartsInfoDs     DS                  inz
     D  CMkey                  1     40
     D  CMseq#                41     45  0
     D  CMcDat                46     52  0
     D  CMrmCd                53     57
     D  CMdiFl                58     58
     D  CMuser                59     68

     c     *entry        plist
     c                   parm                    inGroup
     c                   parm                    inFromDt
     c                   parm                    inToDate
     c                   parm                    inDartType
      *
      /free
     *** Beg Chg *** 06/14/23 *** 114WRJ ****************************
    
       //  // Declare Cursor
       //  Exec SQL
       //    Declare DartsCSR Cursor for
       //      SELECT CMkey, CMseq#, CMcDat, CMrmCd, CMdiFl, CMuser
       //      FROM sdcmp
       //      WHERE Substr(CMkey,4,3) = :inGroup And
       //       CMpnam = :inDartType And CMcdat >= :inFromDt and
       //       CMcdat <= :inToDate;
    
         If inDartType = 'W0040R';
            strSqlDarts = 'SELECT CMkey, CMseq#, CMcDat, CMrmCd, '      +
                          'CMdiFl, CMuser '                             +
                          'From sdcmp '                                 +
                          'Where '                                      +
                          'CMpnam = ''' + %Trim(inDartType) + ''' and ' +
                          'CMcdat >= ' + inFromDt + ' and '             +
                          'CMcdat <= ' + inToDate + ' and ' +
                          'CMstat <> ''I''';
         Else;
            strSqlDarts = 'SELECT CMkey, CMseq#, CMcDat, CMrmCd, '      +
                          'CMdiFl, CMuser '                             +
                          'From sdcmp '                                 +
                          'Where '                                      +
                          'Substr(CMkey,4,3) = ''' + inGroup + ''' and '+
                          'CMpnam = ''' + %Trim(inDartType) + ''' and ' +
                          'CMcdat >= ' + inFromDt + ' and '             +
                          'CMcdat <= ' + inToDate   ;
         EndIf;

         Exec Sql Prepare sqlDarts From :strSqlDarts;
         Exec Sql Declare DartsCSR Cursor For sqlDarts;

     *** End Chg *** 06/14/23 *** 114WRJ ****************************

         // Open Cursor
         Exec SQL Open DartsCSR;

         // Read first darts entry
         Exec SQL Fetch DartsCSR into :DartsInfoDS;

         // Loop as long as there is a darts item to process
         Dow SQLCOD = 0;

     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
      // Check if Agency is assigned to another group
         If inDartType = 'W0040R';
            Exec SQL
               Select Count(*) into : intCount From WMAAP
               Where AAFEIN = Cast(Substr(:CmKEY,1,9) as Dec(9,0))  and
                     AAMOD  = Cast(Substr(:CmKEY,10,3) as Dec(3,0)) and
                     AAFND  <> Cast(:inGroup as Dec(3,0)) ;

       // If agency is assigned to another group then don't delete any darts information.
            If intCount <> 0;
               isDeleteDart = *Off;
            Else;
               isDeleteDart = *On;
            EndIf;

         EndIf;

         If isDeleteDart = *On;

     *** End Add *** 06/14/23 *** 114WRJ ****************************

           // Delete SMARP records - Legacy Additional Remarks
           DltSMARP();

           // Delete SMDLP records - Document Location file
           DltSMDLP();

           // Delete SMMDP records - Multi-Object File
           DltSMMDP();

           // Delete IMDOP records - Document Object File
           DltIMDOP();

           // Delete IMDOP1 records - Indexed Document Object File
           DltIMDOP1();

           // Delete SMCLP records - iDarts Additional Remark File
           DltSMCLP();

           // Delete SDAIP records - Darts Additional Information File
           DltSDAIP();

           // Write Darts Deletion Record
           WrtSXDTP();

           // Delete SDCMP records - Darts Master File
           DltSDCMP();

     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
          EndIf;                                                // isDeleteDart = *On;
     *** End Add *** 06/14/23 *** 114WRJ ****************************

           // Read next darts item
           Exec SQL Fetch DartsCSR into :DartsInfoDS;

         Enddo;

         // Finish
         Return;

      /end-free
       //===================================================================
       // DltSMARP - Delete SMARP records
       //===================================================================
     P DltSMARP        B
     D DltSMARP        PI
     D
      /Free

       Exec SQL
          Delete From SMARP
          Where ARkey = :CMkey And ARseq# = :CMseq#;

      /End-Free

     P DltSMARP        E
       //===================================================================
       // DltSMDLP - Delete SMDLP records
       //===================================================================
     P DltSMDLP        B
     D DltSMDLP        PI
     D
      /Free

       Exec SQL
          Delete From SMDLP
          Where DLkey = :CMkey And DLseq# = :CMseq#;

      /End-Free

     P DltSMDLP        E
       //===================================================================
       // DltSMMDP - Delete SMMDP records
       //===================================================================
     P DltSMMDP        B
     D DltSMMDP        PI
     D
      /Free

       Exec SQL
          Delete From SMMDP
          Where MDkey = :CMkey And MDseq# = :CMseq#;

      /End-Free

     P DltSMMDP        E
       //===================================================================
       // DltIMDOP - Delete IMDOP records
       //===================================================================
     P DltIMDOP        B
     D DltIMDOP        PI
     D
      /Free

       Exec SQL
          Delete From IMDOP
          Where DOdKey = :CMkey And DOdSq# = :CMseq#;

      /End-Free

     P DltIMDOP        E
       //===================================================================
       // DltIMDOP1 - Delete IMDOP1 records
       //===================================================================
     P DltIMDOP1       B
     D DltIMDOP1       PI
     D
      /Free

       Exec SQL
          Delete From IMDOP1
          Where DOdKey = :CMkey And DOdSq# = :CMseq#;

      /End-Free

     P DltIMDOP1       E
       //===================================================================
       // DltSMCLP - Delete SMCLP records
       //===================================================================
     P DltSMCLP        B
     D DltSMCLP        PI
     D
      /Free

       Exec SQL
          Delete From SMCLP
          Where CLkey = :CMkey And CLseq = :CMseq#;

      /End-Free

     P DltSMCLP        E
       //===================================================================
       // DltSDAIP - Delete SDAIP records
       //===================================================================
     P DltSDAIP        B
     D DltSDAIP        PI
     D
      /Free

       Exec SQL
          Delete From SDAIP
          Where AIkey = :CMkey And AIseq# = :CMseq#;

      /End-Free

     P DltSDAIP        E
       //===================================================================
       // DltSDCMP - Delete SDCMP records
       //===================================================================
     P DltSDCMP        B
     D DltSDCMP        PI
     D
      /Free

       Exec SQL
          Delete From SDCMP
          Where CMkey = :CMkey And CMseq# = :CMseq#;

      /End-Free

     P DltSDCMP        E
       //===================================================================
       // WrtSXDTP - Write Darts Deletion Record
       //===================================================================
     P WrtSXDTP        B
     D WrtSXDTP        PI
     D
      /Free

       Clear pdt0p;

       DTfrKy = CMkey;
       DTfrDd = CMcDat;
       DTfrS# = CMseq#;
       DTfrRc = CMrmCd;
       DTfrDi = CMdiFl;
       DTfrDu = CMuser;

       DTtoKy = 'DELETED';
       DTtoDd = 0;
       DTtoS# = 0;
       DTtoRc = *blanks;
       DTtoDi = *blanks;
       DTtoDu = *blanks;

       Write pdt0p;

      /End-Free

     P WrtSXDTP        E
