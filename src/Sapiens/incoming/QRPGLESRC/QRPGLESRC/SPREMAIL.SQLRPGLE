      *Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     ddsRunInfo        ds                  qualified inz
     d runInfo                      150
     d outType                      100    overlay(runInfo)
     d docId                         60    overlay(runInfo)
     d docCat                         5    overlay(runInfo:101)
     d docSub                         5    overlay(runInfo:106)
     d docLang                       40    overlay(runInfo:111)

      *=====================================================================
      * Get the default Email recipients
      *=====================================================================
     dGetDefaultEmailRecipients...
     d                 pr          5000a   varying
     d inRunInfo                           const likeds(dsRunInfo)
     d inRrn                          9  0 const
      *** Beg Chg *** 01/26/23 *** WC2875 **********************************
     d***inRrnType                     25    const varying options(*nopass)
     d inRrnType                    100    const varying options(*nopass)
      *** End Chg *** 01/26/23 *** WC2875 **********************************
      *** Begin Add *** 09/26/22 **********************************
     d outInsured                     1    options(*nopass)
     d outAgent                       1    options(*nopass)
      *** End   Add *** 09/26/22 **********************************
      *** Begin Add *** 01/16/23 **********************************
     d outInsuredEmails...
     d                             1000    options(*nopass) varying
     d outAgencyEmails...
     d                             1000    options(*nopass) varying
      *** End   Add *** 01/16/23 **********************************
      *** Beg Add *** 03/22/24 ************************************
     d outClaim                       1    options(*nopass)
     d outClaimEmails...
     d                             1000    options(*nopass) varying
      *** End Add *** 03/22/24 ************************************

      *=====================================================================
      * Get the available tokens for the run type
      *=====================================================================
     dGetTokensEmail...
     d                 pr             1n
     d inScreenPtr                     *   const
     d inRunType                    100    const varying options(*nopass)
      *** Beg Add *** 01/26/23 *** WC2875 **********************************
     d inPrcType                    100    const varying options(*nopass)
      *** End Add *** 01/26/23 *** WC2875 **********************************
      *** Beg Add ***** 05/18/23 ***************************************
     d inRunType2                   100    const varying options(*nopass)
      *** End Add ***** 05/18/23 ***************************************

      *=====================================================================
      * Load the default Email information (subject/body/recipients) to screen
      *=====================================================================
     dLoadDefaultEmailData...
     d                 pr             1n
     d inScreenPtr                     *   const
     d inRunInfo                           const likeds(dsRunInfo)
     d inRrn                          9  0 const         options(*nopass)
      *** Beg Chg *** 01/26/23 *** WC2875 **********************************
     d***inRrnType                     25    const varying options(*nopass)
     d inRrnType                    100    const varying options(*nopass)
      *** End Chg *** 01/26/23 *** WC2875 **********************************
      *** Beg Add *** 01/26/23 **********************************
     d inPrcType                    100    const varying options(*nopass)
      *** End Add *** 01/26/23 **********************************

      *=====================================================================
      * Replace tokens with the appropriate associated data
      *=====================================================================
     dReplaceTokensEmail...
     d                 pr              a   len(40000) varying
     d inString                        a   len(32000) varying const
     d inRunInfo                           const likeds(dsRunInfo)
     d inRrn                          9  0 const
      *** Beg Chg *** 01/26/23 *** WC2875 **********************************
     d***inRrnType                     25    const varying options(*nopass)
     d inRrnType                    100    const varying options(*nopass)
      *** End Chg *** 01/26/23 *** WC2875 **********************************
     d inOtherInfo                     a   const len(16000) varying             json format
     d                                     options(*nopass)

