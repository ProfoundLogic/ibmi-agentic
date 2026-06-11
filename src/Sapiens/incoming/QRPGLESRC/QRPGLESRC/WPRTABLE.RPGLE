     D*=====================================================================
     D* Table processing procedure prototypes
     D*=====================================================================
    D*
    P*====================================================================
    P* AgtTypCdDs - Get the agent type code description
    P*
    P*  AgtTypCdDs - returns the agent type code description
    P*   co#       - accepts the company #       (required)
    P*   code      - accepts the agent type code (required)
    P*   ATrrn     - returns the RRN of WTATP
    P*====================================================================
    P*
     DAgtTypeDes       PR            30
     D co#                            3  0 const
     D code                          10    const
     D ATrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* ARBtchCdDs - Get the A/R batch code description
    P*
    P*  ARBtchCdDs - returns the A/R batch code description
    P*   co#       - accepts the company #     (required)
    P*   code      - accepts the A/R batch code (required)
    P*   SYrrn     - returns the RRN of WTSYP
    P*====================================================================
    P*
     DARBtchCdDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D SYrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* ARTranCdDs - Get the A/R transaction code description
    P*
    P*  ARTranCdDs - returns the A/R transaction code description
    P*   co#       - accepts the company #      (required)
    P*   code      - accepts the A/R trans code (required)
    P*   TTrrn     - returns the RRN of WTTTP
    P*====================================================================
    P*
     DARTranCdDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D TTrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* BenAdjCdDs - Get the benefit adjustment type code description
    P*
    P*  AgtTypeDes - returns the benefit adjustment type code description
    P*   co#     - accepts the company #               (required)
    P*   code    - accepts the benefit adjustment code (required)
    P*   BArrn   - returns the RRN of WTBAP
    P*====================================================================
    P*
     DBenAdjCdDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D BArrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* BillCmntDs - Get the billing comment code description
    P*
    P*  AgtTypeDes - returns the billing comment code description
    P*   co#       - accepts the company #            (required)
    P*   code      - accepts the billing comment code (required)
    P*   BCrrn     - returns the RRN of WTBCP
    P*====================================================================
    P*
     DBillCmntDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D BCrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* BillSchdDs - Get the billing schedule code description
    P*
    P*  BillSchdDs - returns the billing schedule code description
    P*   co#       - accepts the company #             (required)
    P*   code      - accepts the billing schedule code (required)
    P*   BSrrn     - returns the RRN of WTBSP
    P*====================================================================
    P*
     DBillSchdDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D BSrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* BillSchdType - Get the billing schedule code type
    P*
    P*  BillSchdType - returns the billing schedule code type
    P*   co#       - accepts the company #             (required)
    P*   code      - accepts the billing schedule code (required)
    P*   BSrrn     - returns the RRN of WTBSP
    P*====================================================================
    P*
     DBillSchdType     PR            30
     D co#                            3  0 const
     D code                          10    const
     D BSrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* BR2CodeDs - Get the BR-2 code description
    P*
    P*  BR2CodeDs - returns the BR-2 code description
    P*   co#      - accepts the company #  (required)
    P*   code     - accepts the BR-2 code  (required)
    P*   B2rrn    - returns the RRN of WTB2P
    P*====================================================================
    P*
     DBR2CodeDs        PR            30
     D co#                            3  0 const
     D code                          10    const
     D B2rrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* BR7CodeDs - Get the BR-7 code description
    P*
    P*  BR7CodeDs - returns the BR-7 code description
    P*   co#      - accepts the company #  (required)
    P*   code     - accepts the BR-7 code  (required)
    P*   B7rrn    - returns the RRN of WTB7P
    P*====================================================================
    P*
     DBR7CodeDs        PR            30
     D co#                            3  0 const
     D code                          10    const
     D B7rrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* CauseCdDs  - Get the cause code description
    P*
    P*  CauseCdDs - returns the cause code description
    P*   co#      - accepts the company #  (required)
    P*   code     - accepts the cause code (required)
    P*   CArrn    - returns the RRN of WTCAP
    P*====================================================================
    P*
     DCauseCdDs        PR            30
     D co#                            3  0 const
     D code                          10    const
     D CArrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* ChkCtlCdDs - Get the check control code description
    P*
    P*  ChkCtlCdDs - returns the check control code description
    P*   co#       - accepts the company #     (required)
    P*   code      - accepts the rate set code (required)
    P*   CCrrn     - returns the RRN of WTCCP
    P*====================================================================
    P*
     DChkCtlCdDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D CCrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* CmmRcdCdDs - Get the commission record code description
    P*
    P*  CmmRcdCdDs - returns the commission record code description
    P*   co#       - accepts the company #              (required)
    P*   code      - accepts the commission record code (required)
    P*   CSrrn     - returns the RRN of WTCSP
    P*====================================================================
    P*
     DCmmRcdCdDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D CSrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* CountyCdDs - Get the county code description
    P*
    P*  CountyCdDs - returns the county code description
    P*   co#       - accepts the company #   (required)
    P*   code      - accepts the county code (required)
    P*   CNrrn     - returns the RRN of WTCNP
    P*====================================================================
    P*
     DCountyCdDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D CNrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* IndDisCdDs - Get the indemnity disability type description
    P*
    P*  IndDisCdDs - returns the indemnity disablity type description
    P*   co#       - accepts the company #                (required)
    P*   code      - accepts the indemnity disablity code (required)
    P*   IDrrn     - returns the RRN of WTIDP
    P*====================================================================
    P*
     DIndDisCdDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D IDrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* IndSusCdDs - Get the indemnity suspension reason description
    P*
    P*  IndSusCdDs - returns the indemnity suspension reason description
    P*   co#       - accepts the company #                  (required)
    P*   code      - accepts the indemnity susp reason code (required)
    P*   IDrrn     - returns the RRN of WTIDP
    P*====================================================================
    P*
     DIndSusCdDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D ISrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* LatePmtDs - Get the late payment reason description
    P*
    P*  LatePmtDs - returns the late payment reason description
    P*   co#      - accepts the company #         (required)
    P*   code     - accepts the late payment code (required)
    P*   LRrrn    - returns the RRN of WTLRP
    P*====================================================================
    P*
     DLatePmtDs        PR            30
     D co#                            3  0 const
     D code                          10    const
     D LRrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* LossEduDs - Get the loss control education/training description
    P*
    P*  LossEduDs - returns the loss control description
    P*   co#      - accepts the company #         (required)
    P*   code     - accepts the loss control code (required)
    P*   TRrrn    - returns the RRN of WTTRP
    P*====================================================================
    P*
     DLossEduDs        PR            30
     D co#                            3  0 const
     D code                          10    const
     D TRrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* LossHygDs - Get the loss control hygiene description
    P*
    P*  LossHygDs - returns the loss control description
    P*   co#      - accepts the company #         (required)
    P*   code     - accepts the loss control code (required)
    P*   HYrrn    - returns the RRN of WTHYP
    P*====================================================================
    P*
     DLossHygDs        PR            30
     D co#                            3  0 const
     D code                          10    const
     D HYrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* LossVstDs - Get the loss control visit description
    P*
    P*  LossVstDs - returns the loss control description
    P*   co#      - accepts the company #         (required)
    P*   code     - accepts the loss control code (required)
    P*   VSrrn    - returns the RRN of WTVSP
    P*====================================================================
    P*
     DLossVstDs        PR            30
     D co#                            3  0 const
     D code                          10    const
     D VSrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* NatureCdDs - Get the nature code description
    P*
    P*  NatureCdDs - returns the nature code description
    P*   co#       - accepts the company #    (required)
    P*   code      - accepts the nauture code (required)
    P*   NArrn     - returns the RRN of WTNAP
    P*====================================================================
    P*
     DNatureCdDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D NArrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* NCCICdDs - Get the NCCI code description
    P*
    P*  NCCICdDs - returns the agent type description
    P*   co#     - accepts the company # (required)
    P*   code    - accepts the NCCI code (required)
    P*   NCrrn   - returns the RRN of WTNCP
    P*====================================================================
    P*
     DNCCICdDs         PR            30
     D co#                            3  0 const
     D code                          10    const
     D NCrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* PartCdDs - Get the part code description
    P*
    P*  PartCdDs - returns the part code description
    P*   co#     - accepts the company # (required)
    P*   code    - accepts the part code (required)
    P*   PArrn   - returns the RRN of WTPAP
    P*====================================================================
    P*
     DPartCdDs         PR            30
     D co#                            3  0 const
     D code                          10    const
     D PArrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* PaymntCdDs - Get the payment code description
    P*
    P*  AgtTypeDes - returns the payment code description
    P*   co#     - accepts the company #    (required)
    P*   code    - accepts the payment code (required)
    P*   PCrrn   - returns the RRN of WTPCP
    P*====================================================================
    P*
     DPaymntCdDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D PCrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* RemarkCdDs - Get the remark code description
    P*
    P*  RemarkCdDs - returns the remark code description
    P*   co#       - accepts the company #   (required)
    P*   code      - accepts the remark code (required)
    P*   RCrrn     - returns the RRN of WTRCP
    P*====================================================================
    P*
     DRemarkCdDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D RCrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* RfdStsCdDs - Get the refund status code description
    P*
    P*  RfdStsCdDs - returns the agent type description
    P*   co#       - accepts the company #          (required)
    P*   code      - accepts the refund status code (required)
    P*   RSrrn     - returns the RRN of WTATP
    P*====================================================================
    P*
     DRfdStsCdDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D RSrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* ResTypCdDs - Get the reserve type code description
    P*
    P*  ResTypCdDs - returns the reserve type code description
    P*   co#       - accepts the company #         (required)
    P*   code      - accepts the reserve type code (required)
    P*   RTrrn     - returns the RRN of WTRTP
    P*====================================================================
    P*
     DResTypCdDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D RTrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P*  GetRsvType - returns the reserve type
    P*   co#       - accepts the company #         (required)
    P*   code      - accepts the reserve type code (required)
    P*   Type      - returns the Type
    P*====================================================================
    P*
     DGetRsvType       PR             1
     D co#                            3  0 const
     D code                          10    const
     D RTrrn                          9  0       options(*nopass)

    P*====================================================================
    P* SpcltyCdDs - Get the specialty code description
    P*
    P*  SpcltyCdDs - returns the specialty code description
    P*   co#       - accepts the company #           (required)
    P*   code      - accepts the specialty type code (required)
    P*   SPrrn     - returns the RRN of WTSPP
    P*====================================================================
    P*
     DSpcltyCdDs       PR            30
     D co#                            3  0 const
     D code                          10    const
     D SPrrn                          9  0       options(*nopass)
    P*
    P*====================================================================
    P* ValidGenCode - Validate generic table code
    P*  Returns  - '1' is valid code, '0' if invalid code
    P*   co#       - accepts the company # (required)
    P*   tbid      - accepts the table id (required)
    P*   key/code  - key or code value (required)
    P*   prefix    - possible table id prefix (optional)
    P*====================================================================
    P*
     DValidGenCode     pr              n
     D co#                            3  0 const
     D tbid                           4    const
     D key                           10    const varying
     D inPrefix                       4    const varying options(*nopass)

    P*====================================================================
    P* GetGenCodeDs - Get generic code description
    P*  Returns  - 40A descriptio
    P*   co#       - accepts the company # (required)
    P*   tbid      - accepts the table id (required)
    P*   key/code  - key or code value (required)
    P*   prefix    - possible table id prefix (optional)
    P*====================================================================
     DGetGenCodeDs     pr            40
     D co#                            3  0 const
     D tbid                           4    const
     D key                           10    const varying
     D inPrefix                       4    const varying options(*nopass)

     P*====================================================
     P* Retrieves payroll class code description
    P* inCo#        - accepts the company # (required)
    P* inEfDt       - accepts the effective date (required)
    P* inState      - accepts the state (required)
    P* inRtCd       - accepts the rate set code (required)
    P* inClas       - accepts the class code (required)
    P* inExt        - accepts the extension (required)
     P* outHazGrp    - Returns Hazard Group (Optional)
     P* outContClass - Returns Contractor Class (Optional)
     P* outRate      - Returns Class Code Rate (Optional)
     P*====================================================
     dGetPayrollDesc   pr            30
     d inCo#                          3  0 const
     d inEfDt                         7s 0 const
     d inState                        2    const
     d inRtCd                         3    const
     d inClas                         5s 0 const
     d inExt                          5    const
     d outHazGrp                      5    options(*nopass)
     d outContClass                   1    options(*nopass)
      **** Beg Chg **** 06/17/24 *******************************************
     d***outRate                        7s 2 options(*nopass)
     d outRate                        8s 3 options(*nopass)
      **** End Chg **** 06/17/24 *******************************************

