     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H nomain
     H option(*nodebugio:*srcstmt) bnddir('WPICLMPMT')
      *===================================================================
      * Revisions:                                                       *
      *                                                                  *
      *  02/22/21 C:079 Support: 133/94489                               *
      *                 Change logic to give Sequence number priority    *  state.
      *                 when looking up the provider name                *  state.
      *                                                                  *
      *===================================================================

     D/copy sPrDate
     D/copy sPrSql
     D/copy sPrString
     D/copy wPrClmPmt

     DdsWDPAP        e ds                  extname(WDPAP) inz qualified
     DdsWMCDP        e ds                  extname(WMCDP) inz qualified
     DdsWMCMP        e ds                  extname(WMCMP) inz qualified

     D psds          esds                  extname(szq1p)

     P*====================================================================
     P* GetCheckPayeeName - Get the payee name for the payment made
     P*  // There are other methods used in W0576R that might be needed for legacy payments
     P*
     P*  Returns the name as it would appear on the check
     P*   co#       - accepts the check company #           (required)
     P*   group     - accepts the check group               (required)
     P*   fyr       - accepts the check fyr                 (required)
     P*   claim     - accepts the check claim               (required)
     P*   seq#      - accepts the check seq#                (required)
     P*====================================================================

     PGetCheckPayeeName...
     P                 b                   export

     DGetCheckPayeeName...
     D                 pi          1525    varying
     D inCo#                          3  0 const
     D inGroup                        3  0 const
     D inFyr                          3  0 const
     D inClaim                        7  0 const
     D inSeq#                         5  0 const

     DdsName           ds                  qualified inz
     D last                         500    varying
     D first                        500    varying
     D middle                       500    varying
     DstrName          s           1525    varying inz
      /Free
       LoadPaymentRecord(inCo#:inGroup:inFyr:inClaim:inSeq#);
       Exec sql
        select RPpNam into :strName
         from WDRPP
         where RPco# = :inCo# and RPfnd = :inGroup and RPfyr = :inFyr
          and RPcase = :inClaim
          //*** Beg Chg *** 02/22/21 ***************************************
          //***and (RPseq# = :inSeq# or RPchk# = :dsWDPAP.PAchk#)
          and RPseq# = :inSeq#
          //*** End Chg *** 02/22/21 ***************************************
         limit 1;
       If IsValidFetch(sqlstt);
          Return strName;
       //*** Beg Add *** 02/22/21 ******************************************
       Else;
          Exec sql
           select RPpNam into :strName
            from WDRPP
            where RPco# = :inCo# and RPfnd = :inGroup and RPfyr = :inFyr
             and RPcase = :inClaim and RPchk# = :dsWDPAP.PAchk#
            limit 1;
          If IsValidFetch(sqlstt);
             Return strName;
          Endif;
       //*** End Add *** 02/22/21 ******************************************
       Endif;
       Reset dsName;
       Select;
       When dsWDPAP.PAfein = 0 or dsWDPAP.PAreTp = 'C'
        or %editc(dsWDPAP.PAfein:'X') = dsWMCMP.CMss#;
          Exec sql
           select CDlsN, CDfsN, CDmid into :dsName
            from WMCDP
            where CDss# = :dsWMCMP.CMss#;
       Other;
          Exec sql
           select PDname, PDfsN, ' ' into :dsName
            from WMPDP
            where PDfein = :dsWDPAP.PAfein and PDmod = :dsWDPAP.PAmod;
       Endsl;
       strName = FormatName(dsName.last:dsName.first:dsName.middle);
       Return strName;
      /End-free
     PGetCheckPayeeName...
     P                 e

     P*====================================================================
     P* LoadPaymentRecord - Load the payment record to the global data structure
     P*
     P*   co#       - accepts the check company #           (required)
     P*   group     - accepts the check group               (required)
     P*   fyr       - accepts the check fyr                 (required)
     P*   claim     - accepts the check claim               (required)
     P*   seq#      - accepts the check seq#                (required)
     P*====================================================================

     PLoadPaymentRecord...
     P                 b
     DLoadPaymentRecord...
     D                 pi
     D inCo#                          3  0 const
     D inGroup                        3  0 const
     D inFyr                          3  0 const
     D inClaim                        7  0 const
     D inSeq#                         5  0 const

      /Free
       If inCo#=dsWDPAP.PAco# and inGroup=dsWDPAP.PAfnd
        and inFyr=dsWDPAP.PAfyr and inClaim=dsWDPAP.PAcase
        and inSeq#=dsWDPAP.PAseq#;
          Return;
       Endif;
       Reset dsWDPAP;
       Reset dsWMCMP;
       Reset dsWMCDP;
       Exec sql
        select * into :dsWDPAP
         from WDPAP
         where PAco# = :inCo# and PAfnd = :inGroup and PAfyr = :inFyr
          and PAcase = :inClaim and PAseq# = :inSeq#;
       Exec sql
        select * into :dsWMCMP
         from WMCMP
         where CMco# = :inCo# and CMfnd = :inGroup and CMfyr = :inFyr
          and CMcase = :inClaim;
      /End-free
     PLoadPaymentRecord...
     P                 e
