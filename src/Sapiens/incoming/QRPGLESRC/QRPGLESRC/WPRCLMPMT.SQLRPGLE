     P*====================================================================
     P* GetCheckPayeeName - Get the payee name for the payment made
     P*
     P*  Returns the name as it would appear on the check
     P*   co#       - accepts the check company #           (required)
     P*   group     - accepts the check group               (required)
     P*   fyr       - accepts the check fyr                 (required)
     P*   claim     - accepts the check claim               (required)
     P*   seq#      - accepts the check seq#                (required)
     P*====================================================================
     DGetCheckPayeeName...
     D                 pr          1525    varying
     D inCo#                          3  0 const
     D inGroup                        3  0 const
     D inFyr                          3  0 const
     D inClaim                        7  0 const
     D inSeq#                         5  0 const
     P*====================================================================
     P* LoadPaymentRecord - Load the payment record to the global data structure
     P*
     P*   co#       - accepts the check company #           (required)
     P*   group     - accepts the check group               (required)
     P*   fyr       - accepts the check fyr                 (required)
     P*   claim     - accepts the check claim               (required)
     P*   seq#      - accepts the check seq#                (required)
     P*====================================================================
     DLoadPaymentRecord...
     D                 pr
     D inCo#                          3  0 const
     D inGroup                        3  0 const
     D inFyr                          3  0 const
     D inClaim                        7  0 const
     D inSeq#                         5  0 const
     D*====================================================================
     D* Payment Edit I (Fund/Claim)
     D*====================================================================
     D PayEditFndClm   pr
     D inCO#                          3  0 const
     D inFND                          3  0 const
     D inFYR                          3  0 const
     D inCASE                         7  0 const
     D inRC                           2
     D in03                           1    const
     D in05                           1    const
     D in09                           1    const
     D*====================================================================
     D* Payment Edit II (Claim/Payment)
     D*====================================================================
     D PayEditClmPay   pr
     D inCO#                          3  0 const
     D inFND                          3  0 const
     D inFYR                          3  0 const
     D inCASE                         7  0 const
     D inTRCD                         2    const
     D inFEIN                         9  0 const
     D inMOD                          3  0 const
     D inRC                           2
     D inRECD                         2
     D inRETP                         1
     D inTYPE                         1
     D inPGM                         10    const
     D*====================================================================
     D* Payment Edit III (Amount/Reserves)
     D*====================================================================
     D PayEditAmtRes   pr
     D inCO#                          3  0 const
     D inFND                          3  0 const
     D inFYR                          3  0 const
     D inCASE                         7  0 const
     D inEOB#                         9  0 const
     D inSEQ#                         5  0 const
     D inAMT                          9  2 const
     D inRECD                         2    const
     D inRETP                         1    const
     D inRC                           2
     D inTAMT                         9  2 const
     D inRCN                          7  0 const
     D inTRCD                         2    const
