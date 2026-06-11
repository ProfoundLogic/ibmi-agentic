     D*Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     D* Payment Service Processing Procedure Prototypes
       //===================================================================
     dFinalizeThirdPartyInvoicing...
     d                 pr             1n
     d inCo#                          3  0 Const
     d inGroup                        3  0 Const

       //===================================================================
     dICGroupOpt...
     d                 pr             1n
     d inCo#                          3  0 const
     d inGroup                        3  0 const

       //===================================================================
     dICGroupRan...
     d                 pr             1n
     d inCo#                          3  0 const
     d inGroup                        3  0 const

       //===================================================================
     dICPolicyOpt...
     d                 pr             1
     d inCo#                          3  0 const
     d inGroup                        3  0 const
     d inEmp#                         9  0 const

       //===================================================================
     dICPolicyNoOutput...
     d                 pr             1n
     d inCo#                          3  0 const
     d inGroup                        3  0 const
     d inEmp#                         9  0 const

       //===================================================================
     dICQuoteOpt...
     d                 pr             1n
     d inCo#                          3  0 const
     d inGroup                        3  0 const

       //===================================================================
     dProcessAdjustedInvoicing...
     d                 pr             1n
     d inJson                     32000    const varying

       //===================================================================
     dQuotePaymentRecieved...
     d                 pr             1n
     d inCo#                          3  0 const
     d inGroup                        3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const

       //===================================================================
     dThirdPartyInvoicing...
     d                 pr             1n
     d inJson                     32000    const varying

       //===================================================================
     dThirdPartyUpdates...
     d                 pr             1n
     d inJson                     32000    const varying

       //===================================================================
     dICPaperlessOpt   pr             7  0
     d inCo#                          3  0 const
     d inGroup                        3  0 const
     d inEmp#                         9  0 const
       //===================================================================
      *** Beg Add *** 10/25/23 *** WC2922 ***********************************
     dRemoveInvoices...
     d                 pr             1n
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFYR                          3  0 const
     d inTyp                          1    const

     dResendInvoices...
     d                 pr             1n
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFYR                          3  0 const
     d inTyp                          1    const

     dReplaceHeldInvoices...
     d                 pr             1n
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFYR                          3  0 const
     d inInv#                        10  0 const
     d inTyp                          1    const
     d*** Begin Add *** 09/27/24 *******************************************
     d inMode                         1    const options(*nopass)
     d*** End   Add *** 09/27/24 *******************************************
      *** End Add *** 10/25/23 *** WC2922 ***********************************
